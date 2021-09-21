{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hasql.Interpolate.Internal.OneColumn
  ( OneColumn (..),
  )
where

import GHC.Generics (Generic)
import qualified Hasql.Decoders as D
import Hasql.Interpolate.Internal.Decoder

newtype OneColumn a = OneColumn
  { getOneColumn :: a
  }
  deriving stock (Show, Eq, Generic)

-- | Parse a single column row
instance DecodeField a => DecodeRow (OneColumn a) where
  decodeRow = OneColumn <$> D.column decodeField
