{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hasql.Interpolate.Internal.RowsAffected
  ( RowsAffected (..),
  )
where

import Data.Int
import GHC.Generics (Generic)
import qualified Hasql.Decoders as D
import Hasql.Interpolate.Internal.Decoder

newtype RowsAffected = RowsAffected
  { getRowsAffected :: Int64
  }
  deriving stock (Show, Eq, Generic)

-- | Parse the rows affected from the query result, as in an @insert@,
-- @update@, or @delete@ statement without a returning clause. ('D.rowsAffected')
instance DecodeResult RowsAffected where
  decodeResult = RowsAffected <$> D.rowsAffected
