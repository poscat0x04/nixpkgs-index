{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Nixpkgs.Files where

import Codec.Serialise
import Control.Monad
import Data.Aeson
import Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (hash)
import Data.IntSet (IntSet, insert, member)
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import GHC.Generics
import Optics.TH
import Path

-- | A polymorphic file node that can be used to represent both a file and
-- a file tree.
data FileNode t
  = -- | A regular file
    Regular
      { size :: {-# UNPACK #-} Int,
        executable :: Bool
      }
  | -- | A symboli link that hasn't been resolved or cannot be resolved for some reason
    Symlink
      { targetPath :: {-# UNPACK #-} Text
      }
  | -- | A resolved symbolic link
    ResolvedSymlink
      { targetFile :: {-# UNPACK #-} FileTreeEntry
      }
  | -- | A directory
    Directory
      { contents :: t
      }
  deriving (Show, Eq, Generic)
  deriving (Serialise)

data FileTreeEntry = FileTreeEntry
  { path :: Path 'Rel,
    node :: FileNode ()
  }
  deriving (Show, Eq, Generic)
  deriving (Serialise)

newtype FileTree = FileTree
  { unTree :: FileNode (HashMap (Path 'Rel) FileTree)
  }
  deriving (Show, Eq, Generic)
  deriving (Serialise)

instance FromJSON FileTree where
  parseJSON = withObject "File tree" $ \o -> do
    _type <- o .: "type"
    case _type :: Text of
      "regular" -> do
        size <- o .: "size"
        executable' <- o .:? "executable"
        let executable = Just True == executable'
        pure $ FileTree Regular {..}
      "symlink" -> do
        target <- o .: "target"
        pure $ FileTree $ Symlink target
      "directory" -> do
        entries <- o .: "entries"
        pure $ FileTree $ Directory entries
      _ -> fail [i|unknown type #{_type}|]

newtype FileListing = FileListing
  { root :: FileTree
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, Serialise)

toList :: FileTree -> [FileTreeEntry]
toList = toList' [relp|.|]

toList' :: Path 'Rel -> FileTree -> [FileTreeEntry]
toList' path (FileTree n)
  | Regular {..} <- n =
    let node = Regular {..}
     in [FileTreeEntry {..}]
  | Symlink {..} <- n =
    let node = Symlink {..}
     in [FileTreeEntry {..}]
  | ResolvedSymlink {..} <- n =
    let node = ResolvedSymlink {..}
     in [FileTreeEntry {..}]
  | Directory {..} <- n =
    let l = HM.toList contents
        es = concatMap (uncurry toList' . first (path </>)) l
     in FileTreeEntry {node = Directory (), ..} : es

-- | Recursively try to resolve all symbolic links in a tarball
-- (represented as a list of 'FileTreeEntry')
resolveSymlink :: [FileTreeEntry] -> [FileTreeEntry]
resolveSymlink entries = map resolve entries
  where
    index = HM.fromList $ zip (map path entries) entries
    lkup :: Path 'Rel -> Maybe FileTreeEntry
    lkup = lkup' mempty
    lkup' :: IntSet -> Path 'Rel -> Maybe FileTreeEntry
    lkup' s p = do
      let h = hash p
      guard $ not $ h `member` s
      e@FileTreeEntry {..} <- HM.lookup p index
      case node of
        Symlink {..} -> do
          p' <- parseRel targetPath
          let newPath = parent path </> p'
          lkup' (insert h s) newPath
        ResolvedSymlink {..} -> pure targetFile
        _ -> pure e
    resolve e@FileTreeEntry {..}
      | Symlink {..} <- node,
        path /= [relp|.|] =
        fromMaybe e $ do
          p <- parseRel targetPath
          let newPath = parent path </> p
          targetFile <- lkup newPath
          pure $ FileTreeEntry path ResolvedSymlink {..}
      | otherwise = e

makeFieldLabels ''FileNode
makeFieldLabels ''FileTree
makeFieldLabels ''FileTreeEntry
makeFieldLabels ''FileListing
