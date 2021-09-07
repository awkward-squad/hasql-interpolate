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

import Data.Functor.Identity
import Data.Int
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, DiffTime, LocalTime, UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
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
  {-# INLINE decodeRow #-}

class GDecodeRow a where
  gdecodeRow :: Row (a p)

-- | Determine a result decoder from a Haskell type.
class DecodeResult a where
  decodeResult :: Result a

instance GDecodeRow a => GDecodeRow (M1 t i a) where
  gdecodeRow = M1 <$> gdecodeRow
  {-# INLINE gdecodeRow #-}

instance (GDecodeRow a, GDecodeRow b) => GDecodeRow (a :*: b) where
  gdecodeRow = (:*:) <$> gdecodeRow <*> gdecodeRow
  {-# INLINE gdecodeRow #-}

instance DecodeField a => GDecodeRow (K1 i a) where
  gdecodeRow = K1 <$> column decodeField
  {-# INLINE gdecodeRow #-}

-- | Parse a postgres @array@ using 'listArray'
instance DecodeField a => DecodeValue [a] where
  decodeValue = listArray decodeField
  {-# INLINE decodeValue #-}

-- | Parse a postgres @array@ using 'vectorArray'
instance DecodeField a => DecodeValue (Vector a) where
  decodeValue = vectorArray decodeField
  {-# INLINE decodeValue #-}

-- | Parse a postgres @bool@ using 'bool'
instance DecodeValue Bool where
  decodeValue = bool
  {-# INLINE decodeValue #-}

-- | Parse a postgres @text@ using 'text'
instance DecodeValue Text where
  decodeValue = text
  {-# INLINE decodeValue #-}

-- | Parse a postgres @int2@ using 'int2'
instance DecodeValue Int16 where
  decodeValue = int2
  {-# INLINE decodeValue #-}

-- | Parse a postgres @int4@ using 'int4'
instance DecodeValue Int32 where
  decodeValue = int4
  {-# INLINE decodeValue #-}

-- | Parse a postgres @int8@ using 'int8'
instance DecodeValue Int64 where
  decodeValue = int8
  {-# INLINE decodeValue #-}

-- | Parse a postgres @float4@ using 'float4'
instance DecodeValue Float where
  decodeValue = float4
  {-# INLINE decodeValue #-}

-- | Parse a postgres @float8@ using 'float8'
instance DecodeValue Double where
  decodeValue = float8
  {-# INLINE decodeValue #-}

-- | Parse a postgres @char@ using 'char'
instance DecodeValue Char where
  decodeValue = char
  {-# INLINE decodeValue #-}

-- | Parse a postgres @date@ using 'date'
instance DecodeValue Day where
  decodeValue = date
  {-# INLINE decodeValue #-}

-- | Parse a postgres @timestamp@ using 'timestamp'
instance DecodeValue LocalTime where
  decodeValue = timestamp
  {-# INLINE decodeValue #-}

-- | Parse a postgres @timestamptz@ using 'timestamptz'
instance DecodeValue UTCTime where
  decodeValue = timestamptz
  {-# INLINE decodeValue #-}

-- | Parse a postgres @numeric@ using 'numeric'
instance DecodeValue Scientific where
  decodeValue = numeric
  {-# INLINE decodeValue #-}

-- | Parse a postgres @interval@ using 'interval'
instance DecodeValue DiffTime where
  decodeValue = interval
  {-# INLINE decodeValue #-}

-- | Parse a postgres @uuid@ using 'uuid'
instance DecodeValue UUID where
  decodeValue = uuid
  {-# INLINE decodeValue #-}

-- | Overlappable instance for parsing non-nullable values
instance {-# OVERLAPPABLE #-} DecodeValue a => DecodeField a where
  decodeField = nonNullable decodeValue
  {-# INLINE decodeField #-}

-- | Instance for parsing nullable values
instance DecodeValue a => DecodeField (Maybe a) where
  decodeField = nullable decodeValue
  {-# INLINE decodeField #-}

-- | Parse a single column nullable row
instance DecodeField a => DecodeRow (Identity a) where
  decodeRow = Identity <$> column decodeField
  {-# INLINE decodeRow #-}

-- | Parse any number of rows into a list ('rowList')
instance DecodeRow a => DecodeResult [a] where
  decodeResult = rowList decodeRow
  {-# INLINE decodeResult #-}

-- | Parse any number of rows into a 'Vector' ('rowVector')
instance DecodeRow a => DecodeResult (Vector a) where
  decodeResult = rowVector decodeRow
  {-# INLINE decodeResult #-}

-- | Parse zero or one rows, throw 'Hasql.Errors.UnexpectedAmountOfRows' otherwise. ('rowMaybe')
instance DecodeRow a => DecodeResult (Maybe a) where
  decodeResult = rowMaybe decodeRow
  {-# INLINE decodeResult #-}

-- | Parse a single row result, throw 'Hasql.Errors.UnexpectedAmountOfRows' otherwise. ('singleRow')
instance DecodeRow a => DecodeResult (Identity a) where
  decodeResult = Identity <$> singleRow decodeRow
  {-# INLINE decodeResult #-}

-- | Parse the rows affected from the query result, as in an @insert@,
-- @update@, or @delete@ statement without a returning clause. ('rowsAffected')
instance DecodeResult Int64 where
  decodeResult = rowsAffected
  {-# INLINE decodeResult #-}

-- | Ignore the query response ('noResult')
instance DecodeResult () where
  decodeResult = noResult
  {-# INLINE decodeResult #-}

$(genDecodeRowInstances 8)
