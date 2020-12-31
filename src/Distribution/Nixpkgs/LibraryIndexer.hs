{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module Distribution.Nixpkgs.LibraryIndexer where

import Codec.Serialise
import Control.Effect
import Control.Effect.Http
import Control.Effect.PooledConc
import Control.Monad
import Data.Char
import Data.Foldable
import Data.HashMap.Strict (HashMap, alter)
import Data.Maybe
import Data.Text (Text, pack)
import Data.Void
import Distribution.Nixpkgs.Files
import Distribution.Nixpkgs.Hydra
import Distribution.Nixpkgs.Packages
import GHC.Generics
import Path
import Text.Megaparsec

type Parser = Parsec Void Text

underLdLibPath :: Path 'Rel -> Bool
underLdLibPath = (== [relp|lib|]) . parent

underPkgConfigPath :: Path 'Rel -> Bool
underPkgConfigPath = (== [relp|lib/pkgconfig|]) . parent

lib :: Text -> Bool -> Parser Text
lib name allowVersion = do
  _ <- chunk "lib"
  libname <- takeWhile1P Nothing (/= '.')
  _ <- chunk name
  when allowVersion verString
  pure libname

dyn :: Bool -> Parser Text
dyn = lib ".so"

static :: Bool -> Parser Text
static = lib ".a"

pkgconfig :: Parser Text
pkgconfig = do
  l <- some $ do
    notFollowedBy (chunk ".pc")
    anySingle
  _ <- chunk ".pc"
  pure $ pack l

verString :: Parser ()
verString = do
  _ <- many $ do
    _ <- single '.'
    takeWhile1P Nothing isNumber
  pure ()

dynLib :: FileTreeEntry -> Maybe Text
dynLib FileTreeEntry {..}
  | Regular {..} <- node,
    underLdLibPath path,
    executable =
    parseMaybe (dyn False) $ fromRel $ filename path
  | ResolvedSymlink {..} <- node,
    underLdLibPath path,
    FileTreeEntry path' node' <- targetFile,
    Regular {..} <- node',
    executable = do
    name1 <- parseMaybe (dyn False) $ fromRel $ filename path
    name2 <- parseMaybe (dyn True) $ fromRel $ filename path'
    guard $ name1 == name2
    pure name1
  | otherwise = Nothing

staticLib :: FileTreeEntry -> Maybe Text
staticLib FileTreeEntry {..}
  | Regular {} <- node,
    underLdLibPath path =
    parseMaybe (static False) $ fromRel $ filename path
  | otherwise = Nothing

pkgconfigLib :: FileTreeEntry -> Maybe Text
pkgconfigLib FileTreeEntry {..}
  | Regular {} <- node,
    underPkgConfigPath path =
    parseMaybe pkgconfig $ fromRel $ filename path
  | otherwise = Nothing

buildIndex :: [(PathOrigin, [Text])] -> HashMap Text [PathOrigin]
buildIndex = foldl' innerFold mempty
  where
    innerFold :: HashMap Text [PathOrigin] -> (PathOrigin, [Text]) -> HashMap Text [PathOrigin]
    innerFold m (o, l) = foldl' insert m l
      where
        insert :: HashMap Text [PathOrigin] -> Text -> HashMap Text [PathOrigin]
        insert =
          flip $
            alter
              ( \case
                  Nothing -> Just [o]
                  Just os -> Just (o : os)
              )

data LibIndex = LibIndex
  { dynLibIndex :: HashMap Text [PathOrigin],
    staticLibIndex :: HashMap Text [PathOrigin],
    pkgconfigLibIndex :: HashMap Text [PathOrigin]
  }
  deriving (Show, Eq, Generic)
  deriving (Serialise)

buildLibIndex :: (Eff Http m, Eff PooledConc m) => [StorePath] -> m LibIndex
buildLibIndex paths = withTaskGroup 10 $ \g -> do
  asyncs <- traverse (async g . fetchFileListing) paths
  let origins = map origin paths
  res <- traverse wait asyncs
  let dynLibIndex = buildIndex $ zip origins $ map (mapMaybe dynLib) res
  let staticLibIndex = buildIndex $ zip origins $ map (mapMaybe staticLib) res
  let pkgconfigLibIndex = buildIndex $ zip origins $ map (mapMaybe pkgconfigLib) res
  pure LibIndex {..}
