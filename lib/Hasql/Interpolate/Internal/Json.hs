{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hasql.Interpolate.Internal.Json
  ( Json (..),
    Jsonb (..),
    JsonBytes (..),
    JsonbBytes (..),
    AsJson (..),
    AsJsonb (..),
  )
where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Coerce
import Data.Functor.Contravariant
import qualified Data.Text as T
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Interpolate.Internal.Decoder
import Hasql.Interpolate.Internal.Encoder

-- | Newtype for 'Hasql.Interpolate.Decoder.DecodeValue' /
-- 'Hasql.Interpolate.Encoder.EncodeValue' instances that converts
-- between a postgres @jsonb@ and an Aeson 'Value'
newtype Jsonb = Jsonb Value

-- | Newtype for 'Hasql.Interpolate.Decoder.DecodeValue' /
-- 'Hasql.Interpolate.Encoder.EncodeValue' instances that converts
-- between a postgres @json@ and an Aeson 'Value'
newtype Json = Json Value

-- | Newtype for 'Hasql.Interpolate.Decoder.DecodeValue' /
-- 'Hasql.Interpolate.Encoder.EncodeValue' instances that converts
-- between a postgres @jsonb@ and a 'ByteString'
newtype JsonbBytes = JsonbBytes ByteString

-- | Newtype for 'Hasql.Interpolate.Decoder.DecodeValue' /
-- 'Hasql.Interpolate.Encoder.EncodeValue' instances that converts
-- between a postgres @json@ and a 'ByteString'
newtype JsonBytes = JsonBytes ByteString

-- | Newtype for 'Hasql.Interpolate.Decoder.DecodeValue' /
-- 'Hasql.Interpolate.Encoder.EncodeValue' instances that converts
-- between a postgres @json@ and anything that is an instance of
-- 'FromJSON' / 'ToJSON'
newtype AsJson a = AsJson a

-- | Newtype for 'Hasql.Interpolate.Decoder.DecodeValue' /
-- 'Hasql.Interpolate.Encoder.EncodeValue' instances that converts
-- between a postgres @jsonb@ and anything that is an instance of
-- 'FromJSON' / 'ToJSON'
newtype AsJsonb a = AsJsonb a

-- | Parse a postgres @jsonb@ using 'D.jsonb'
instance DecodeValue Jsonb where
  decodeValue = coerce D.jsonb

-- | Parse a postgres @json@ using 'D.json'
instance DecodeValue Json where
  decodeValue = coerce D.json

-- | Parse a postgres @jsonb@ using 'D.jsonbBytes'
instance DecodeValue JsonbBytes where
  decodeValue = coerce (D.jsonbBytes Right)

-- | Parse a postgres @json@ using 'D.jsonBytes'
instance DecodeValue JsonBytes where
  decodeValue = coerce (D.jsonBytes Right)

-- | Parse a postgres @json@ to anything that is an instance of
-- 'Aeson.FromJSON'
instance Aeson.FromJSON a => DecodeValue (AsJson a) where
  decodeValue = AsJson <$> D.jsonBytes (first T.pack . Aeson.eitherDecodeStrict)

-- | Parse a postgres @jsonb@ to anything that is an instance of
-- 'Aeson.FromJSON'
instance Aeson.FromJSON a => DecodeValue (AsJsonb a) where
  decodeValue = AsJsonb <$> D.jsonbBytes (first T.pack . Aeson.eitherDecodeStrict)

-- | Encode an Aeson 'Aeson.Value' to a postgres @json@ using 'E.json'
instance EncodeValue Json where
  encodeValue = coerce E.json

-- | Encode an Aeson 'Aeson.Value' to a postgres @jsonb@ using 'E.jsonb'
instance EncodeValue Jsonb where
  encodeValue = coerce E.jsonb

-- | Encode a 'ByteString' to a postgres @json@ using 'E.jsonBytes'
instance EncodeValue JsonBytes where
  encodeValue = coerce E.jsonbBytes

-- | Encode a 'ByteString' to a postgres @jsonb@ using 'E.jsonbBytes'
instance EncodeValue JsonbBytes where
  encodeValue = coerce E.jsonbBytes

-- | Encode anything that is an instance of 'Aeson.ToJSON' to a postgres @json@
instance Aeson.ToJSON a => EncodeValue (AsJson a) where
  encodeValue = BL.toStrict . Aeson.encode . coerce @_ @a >$< E.jsonBytes

-- | Encode anything that is an instance of 'Aeson.ToJSON' to a postgres @jsonb@
instance Aeson.ToJSON a => EncodeValue (AsJsonb a) where
  encodeValue = BL.toStrict . Aeson.encode . coerce @_ @a >$< E.jsonbBytes
