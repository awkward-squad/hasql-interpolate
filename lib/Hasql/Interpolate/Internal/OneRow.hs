{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hasql.Interpolate.Internal.OneRow
  ( OneRow (..),
  )
where

import GHC.Generics (Generic)
import qualified Hasql.Decoders as D
import Hasql.Interpolate.Internal.Decoder

newtype OneRow a = OneRow
  { getOneRow :: a
  }
  deriving stock (Show, Eq, Generic)

-- | Parse a single row result, throw
-- 'Hasql.Errors.UnexpectedAmountOfRows'
-- otherwise. ('Hasql.Decoders.singleRow')
instance DecodeRow a => DecodeResult (OneRow a) where
  decodeResult = OneRow <$> D.singleRow decodeRow
