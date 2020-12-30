{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Distribution.Nixpkgs.Packages where

import Codec.Serialise
import Data.ByteString (ByteString)
import Data.Hashable
import GHC.Generics

data PathOrigin = PathOrigin
  { attr :: {-# UNPACK #-} ByteString,
    output :: {-# UNPACK #-} ByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise, Hashable)

data StorePath = StorePath
  { hash :: {-# UNPACK #-} ByteString,
    name :: {-# UNPACK #-} ByteString,
    origin :: {-# UNPACK #-} PathOrigin
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise, Hashable)
