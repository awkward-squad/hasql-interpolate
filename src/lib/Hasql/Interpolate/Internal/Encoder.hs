{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasql.Interpolate.Internal.Encoder
  ( EncodeValue (..),
    EncodeField (..),
  )
where

import Data.Int
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (Day, DiffTime, LocalTime, UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Encoders

-- | This type class determines which encoder we will apply to a field
-- by its type.
--
-- ==== __Example__
--
-- @
--
-- data ThreatLevel = None | Midnight
--
-- instance EncodeValue ThreatLevel where
--   encodeValue = enum \\case
--     None     -> "none"
--     Midnight -> "midnight"
-- @
class EncodeValue a where
  encodeValue :: Value a

-- | Encode a list as a postgres array using 'foldableArray'
instance EncodeField a => EncodeValue [a] where
  encodeValue = foldableArray encodeField

-- | Encode a 'Vector' as a postgres array using 'foldableArray'
instance EncodeField a => EncodeValue (Vector a) where
  encodeValue = foldableArray encodeField

-- | Encode a 'Bool' as a postgres @boolean@ using 'bool'
instance EncodeValue Bool where
  encodeValue = bool

-- | Encode a 'Text' as a postgres @text@ using 'text'
instance EncodeValue Text where
  encodeValue = text

-- | Encode a 'Int16' as a postgres @int2@ using 'int2'
instance EncodeValue Int16 where
  encodeValue = int2

-- | Encode a 'Int32' as a postgres @int4@ using 'int4'
instance EncodeValue Int32 where
  encodeValue = int4

-- | Encode a 'Int64' as a postgres @int8@ using 'int8'
instance EncodeValue Int64 where
  encodeValue = int8

-- | Encode a 'Float' as a postgres @float4@ using 'float4'
instance EncodeValue Float where
  encodeValue = float4

-- | Encode a 'Double' as a postgres @float8@ using 'float8'
instance EncodeValue Double where
  encodeValue = float8

-- | Encode a 'Char' as a postgres @char@ using 'char'
instance EncodeValue Char where
  encodeValue = char

-- | Encode a 'Day' as a postgres @date@ using 'date'
instance EncodeValue Day where
  encodeValue = date

-- | Encode a 'LocalTime' as a postgres @timestamp@ using 'timestamp'
instance EncodeValue LocalTime where
  encodeValue = timestamp

-- | Encode a 'UTCTime' as a postgres @timestamptz@ using 'timestamptz'
instance EncodeValue UTCTime where
  encodeValue = timestamptz

-- | Encode a 'Scientific' as a postgres @numeric@ using 'numeric'
instance EncodeValue Scientific where
  encodeValue = numeric

-- | Encode a 'DiffTime' as a postgres @interval@ using 'interval'
instance EncodeValue DiffTime where
  encodeValue = interval

-- | Encode a 'UUID' as a postgres @uuid@ using 'uuid'
instance EncodeValue UUID where
  encodeValue = uuid

-- | You do not need to define instances for this class; The two
-- instances exported here cover all uses. The class only exists to
-- lift 'Value' to hasql's 'NullableOrNot' GADT.
class EncodeField a where
  encodeField :: NullableOrNot Value a

-- | Overlappable instance for all non-nullable types.
instance {-# OVERLAPPABLE #-} EncodeValue a => EncodeField a where
  encodeField = nonNullable encodeValue

-- | Instance for all nullable types. 'Nothing' is encoded as @null@.
instance EncodeValue a => EncodeField (Maybe a) where
  encodeField = nullable encodeValue
