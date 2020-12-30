{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Effect.Http where

import Control.Effect
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client
import qualified Network.HTTP.Client as C

data Http :: Effect where
  ParseRequest :: String -> Http m Request
  PerformRequest :: Request -> Http m (Response ByteString)

parseRequest :: Eff Http m => String -> m Request
parseRequest s = send $ ParseRequest s

performRequest :: Eff Http m => Request -> m (Response ByteString)
performRequest r = send $ PerformRequest r

httpToIO :: Eff (Embed IO) m => Manager -> SimpleInterpreterFor Http m
httpToIO manager = interpretSimple $ \case
  ParseRequest s -> embed $ C.parseRequest @IO s
  PerformRequest r -> embed $ httpLbs r manager
