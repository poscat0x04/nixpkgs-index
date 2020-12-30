{-# LANGUAGE QuasiQuotes #-}

module Distribution.Nixpkgs.Hydra where

import Codec.Compression.Brotli
import Control.Effect
import Control.Effect.Http
import Data.Aeson
import Data.String.Interpolate
import Distribution.Nixpkgs.Files
import Distribution.Nixpkgs.Packages
import Network.HTTP.Client
  ( Response (responseBody, responseStatus),
  )
import Network.HTTP.Types

fetchFileListing :: Eff Http m => StorePath -> m [FileTreeEntry]
fetchFileListing StorePath {..} = do
  let url = [i|GET https://cache.nixos.org/#{hash}.ls|]
  initReq <- parseRequest url
  resp <- performRequest initReq
  let code = responseStatus resp
  if
      | code == ok200 -> do
        let body = decompress $ responseBody resp
        case decode body of
          Nothing -> pure []
          Just l -> pure $ resolveSymlink $ toList $ root l
      | code == notFound404 -> pure []
      | otherwise -> undefined
