{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Nixpkgs.Packages
  ( PathOrigin (..),
    StorePath (..),
    parseStorePath,
  )
where

import Codec.Serialise
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.String.Interpolate
import Data.Void
import GHC.Generics
import Optics.TH
import Text.Megaparsec

type Parser = Parsec Void ByteString

data PathOrigin = PathOrigin
  { attr :: {-# UNPACK #-} ByteString,
    output :: {-# UNPACK #-} ByteString
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Serialise, Hashable)

instance Show PathOrigin where
  show PathOrigin {..} = [i|#{attr}.#{output}|]

data StorePath = StorePath
  { hash :: {-# UNPACK #-} ByteString,
    name :: {-# UNPACK #-} ByteString,
    origin :: {-# UNPACK #-} PathOrigin
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise, Hashable)

storePath :: PathOrigin -> Parser StorePath
storePath origin = do
  _ <- chunk "/nix/store/"
  hash <- takeWhile1P (Just "hash") (/= 45)
  _ <- single 45
  name <- takeRest
  pure StorePath {..}

parseStorePath :: PathOrigin -> ByteString -> Maybe StorePath
parseStorePath origin = parseMaybe (storePath origin)

makeFieldLabels ''PathOrigin
makeFieldLabels ''StorePath
