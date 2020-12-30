{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Effect.Http where

import Control.Effect
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Client

data Http :: Effect where
  PerformRequest :: Request -> Http m (Response ByteString)

performRequest :: Eff Http m => Request -> m (Response ByteString)
performRequest r = send $ PerformRequest r

httpToIO :: Eff (Embed IO) m => Manager -> SimpleInterpreterFor Http m
httpToIO manager = interpretSimple $
  \(PerformRequest r) -> embed $ httpLbs r manager
