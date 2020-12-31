module Main where

import Cli
import Codec.Serialise
import Control.Effect
import Control.Effect.Http
import Control.Effect.PooledConc
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Distribution.Nixpkgs.LibraryIndexer
  ( LibIndex (..),
    buildLibIndex,
  )
import Distribution.Nixpkgs.NixEnv
import Network.HTTP.Client.TLS
import System.Directory

main :: IO ()
main = do
  cmd <- parseCmd
  cacheDir <- getXdgDirectory XdgCache "nixpkgs-index"
  let defLibIndexPath = cacheDir <> "/library-index.cbor"
  let indexPath = fromMaybe defLibIndexPath $ indexFile cmd
  case cmd of
    Index {..} -> do
      manager <- newTlsManager
      f <- B.readFile xml
      case parseNixEnvOutput f of
        Nothing -> error "xml parse failed with unknown output"
        Just ps -> do
          index <- runM $ pooledConcToIO $ httpToIO manager $ buildLibIndex ps
          writeFileSerialise indexPath index
    Query {..} -> do
      LibIndex {..} <- readFileDeserialise indexPath
      if
          | static -> print $ fromMaybe [] $ HM.lookup libname staticLibIndex
          | pkgconfig -> print $ fromMaybe [] $ HM.lookup libname pkgconfigLibIndex
          | otherwise -> print $ fromMaybe [] $ HM.lookup libname dynLibIndex
