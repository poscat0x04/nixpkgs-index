{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Nixpkgs.Files where

import Codec.Serialise
import Data.Aeson
import Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.String.Interpolate
import Data.Text (Text)
import GHC.Generics
import Optics.TH
import Path

data FileNode t
  = Regular
      { size :: {-# UNPACK #-} Int,
        executable :: {-# UNPACK #-} Bool
      }
  | Symlink
      { targetPath :: {-# UNPACK #-} Text
      }
  | ResolvedSymlink
      { targetFile :: {-# UNPACK #-} FileTreeEntry
      }
  | Directory
      { contents :: {-# UNPACK #-} t
      }
  deriving (Show, Eq, Generic)
  deriving (Serialise)

data FileTreeEntry = FileTreeEntry
  { path :: {-# UNPACK #-} Path 'Rel,
    node :: {-# UNPACK #-} FileNode ()
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

resolveSymlink :: [FileTreeEntry] -> [FileTreeEntry]
resolveSymlink entries = map resolve entries
  where
    index = HM.fromList $ zip (map path entries) entries
    lkup p = do
      e@FileTreeEntry {..} <- HM.lookup p index
      case node of
        Symlink {..} -> do
          p' <- parseRel targetPath
          let newPath = parent path </> p'
          lkup newPath
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
