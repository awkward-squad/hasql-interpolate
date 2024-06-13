{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasql.Interpolate.Internal.Decoder
  ( -- * Decoding type classes
    DecodeValue (..),
    DecodeField (..),
    DecodeRow (..),
    DecodeResult (..),

    -- * Generics
    GDecodeRow (..),
  )
where

import Data.Int
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, DiffTime, LocalTime, UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Network.IP.Addr
import GHC.Generics
import Hasql.Decoders
import Hasql.Interpolate.Internal.Decoder.TH

-- | This type class determines which decoder we will apply to a query
-- field by the type of the result.
--
-- ==== __Example__
--
-- @
--
-- data ThreatLevel = None | Midnight
--
-- instance DecodeValue ThreatLevel where
--   decodeValue = enum \\case
--     "none"     -> Just None
--     "midnight" -> Just Midnight
--     _          -> Nothing
-- @
class DecodeValue a where
  decodeValue :: Value a

-- | You do not need to define instances for this class; The two
-- instances exported here cover all uses. The class only exists to
-- lift 'Value' to hasql's 'NullableOrNot' GADT.
class DecodeField a where
  decodeField :: NullableOrNot Value a

-- | Determine a row decoder from a Haskell type. Derivable with
-- generics for any product type.
--
-- ==== __Examples__
--
-- A manual instance:
--
-- @
-- data T = T Int64 Bool Text
--
-- instance DecodeRow T where
--   decodeRow = T
--     <$> column decodeField
--     <*> column decodeField
--     <*> column decodeField
-- @
--
-- A generic instance:
--
-- @
-- data T
--  = T Int64 Bool Text
--  deriving stock (Generic)
--  deriving anyclass (DecodeRow)
-- @
class DecodeRow a where
  decodeRow :: Row a
  default decodeRow :: (Generic a, GDecodeRow (Rep a)) => Row a
  decodeRow = to <$> gdecodeRow

class GDecodeRow a where
  gdecodeRow :: Row (a p)

-- | Determine a result decoder from a Haskell type.
class DecodeResult a where
  decodeResult :: Result a

instance GDecodeRow a => GDecodeRow (M1 t i a) where
  gdecodeRow = M1 <$> gdecodeRow

instance (GDecodeRow a, GDecodeRow b) => GDecodeRow (a :*: b) where
  gdecodeRow = (:*:) <$> gdecodeRow <*> gdecodeRow

instance DecodeField a => GDecodeRow (K1 i a) where
  gdecodeRow = K1 <$> column decodeField

-- | Parse a postgres @array@ using 'listArray'
instance DecodeField a => DecodeValue [a] where
  decodeValue = listArray decodeField

-- | Parse a postgres @array@ using 'vectorArray'
instance DecodeField a => DecodeValue (Vector a) where
  decodeValue = vectorArray decodeField

-- | Parse a postgres @bool@ using 'bool'
instance DecodeValue Bool where
  decodeValue = bool

-- | Parse a postgres @text@ using 'text'
instance DecodeValue Text where
  decodeValue = text

-- | Parse a postgres @int2@ using 'int2'
instance DecodeValue Int16 where
  decodeValue = int2

-- | Parse a postgres @int4@ using 'int4'
instance DecodeValue Int32 where
  decodeValue = int4

-- | Parse a postgres @int8@ using 'int8'
instance DecodeValue Int64 where
  decodeValue = int8

-- | Parse a postgres @float4@ using 'float4'
instance DecodeValue Float where
  decodeValue = float4

-- | Parse a postgres @float8@ using 'float8'
instance DecodeValue Double where
  decodeValue = float8

-- | Parse a postgres @char@ using 'char'
instance DecodeValue Char where
  decodeValue = char

-- | Parse a postgres @date@ using 'date'
instance DecodeValue Day where
  decodeValue = date

-- | Parse a postgres @timestamp@ using 'timestamp'
instance DecodeValue LocalTime where
  decodeValue = timestamp

-- | Parse a postgres @timestamptz@ using 'timestamptz'
instance DecodeValue UTCTime where
  decodeValue = timestamptz

-- | Parse a postgres @numeric@ using 'numeric'
instance DecodeValue Scientific where
  decodeValue = numeric

-- | Parse a postgres @interval@ using 'interval'
instance DecodeValue DiffTime where
  decodeValue = interval

-- | Parse a postgres @uuid@ using 'uuid'
instance DecodeValue UUID where
  decodeValue = uuid

-- | Parse a postgres @inet@ using 'inet'
instance DecodeValue (NetAddr IP) where
  decodeValue = inet

-- | Overlappable instance for parsing non-nullable values
instance {-# OVERLAPPABLE #-} DecodeValue a => DecodeField a where
  decodeField = nonNullable decodeValue

-- | Instance for parsing nullable values
instance DecodeValue a => DecodeField (Maybe a) where
  decodeField = nullable decodeValue

-- | Parse any number of rows into a list ('rowList')
instance DecodeRow a => DecodeResult [a] where
  decodeResult = rowList decodeRow

-- | Parse any number of rows into a 'Vector' ('rowVector')
instance DecodeRow a => DecodeResult (Vector a) where
  decodeResult = rowVector decodeRow

-- | Parse zero or one rows, throw 'Hasql.Errors.UnexpectedAmountOfRows' otherwise. ('rowMaybe')
instance DecodeRow a => DecodeResult (Maybe a) where
  decodeResult = rowMaybe decodeRow

-- | Ignore the query response ('noResult')
instance DecodeResult () where
  decodeResult = noResult

$(traverse genDecodeRowInstance [2 .. 8])
