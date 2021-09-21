{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hasql.Interpolate.Internal.Json
  ( Json (..),
    Jsonb (..),
    AsJson (..),
    AsJsonb (..),
  )
where

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
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
-- between a postgres json type and an Aeson 'Value'
newtype Jsonb = Jsonb Value

-- | Newtype for 'Hasql.Interpolate.Decoder.DecodeValue' /
-- 'Hasql.Interpolate.Encoder.EncodeValue' instances that converts
-- between a postgres json type and an Aeson 'Value'
newtype Json = Json Value

-- | Newtype for 'Hasql.Interpolate.Decoder.DecodeValue' /
-- 'Hasql.Interpolate.Encoder.EncodeValue' instances that converts
-- between a postgres json type and anything that is an instance of
-- 'FromJSON' / 'ToJSON'
newtype AsJson a = AsJson a

-- | Newtype for 'Hasql.Interpolate.Decoder.DecodeValue' /
-- 'Hasql.Interpolate.Encoder.EncodeValue' instances that converts
-- between a postgres jsonb type and anything that is an instance of
-- 'FromJSON' / 'ToJSON'
newtype AsJsonb a = AsJsonb a

-- | Parse a postgres @jsonb@ using 'jsonb'
instance DecodeValue Jsonb where
  decodeValue = coerce D.jsonb

-- | Parse a postgres @json@ using 'json'
instance DecodeValue Json where
  decodeValue = coerce D.json

-- | Parse a postgres @json@ to anything that is an instance of
-- 'Aeson.FromJSON'
instance Aeson.FromJSON a => DecodeValue (AsJson a) where
  decodeValue = AsJson <$> D.jsonBytes (first T.pack . Aeson.eitherDecodeStrict)

-- | Parse a postgres @jsonb@ to anything that is an instance of
-- 'Aeson.FromJSON'
instance Aeson.FromJSON a => DecodeValue (AsJsonb a) where
  decodeValue = AsJsonb <$> D.jsonbBytes (first T.pack . Aeson.eitherDecodeStrict)

-- | Encode an Aeson 'Aeson.Value' to a postgres @json@ using 'json'
instance EncodeValue Json where
  encodeValue = coerce E.json

-- | Encode an Aeson 'Aeson.Value' to a postgres @jsonb@ using 'jsonb'
instance EncodeValue Jsonb where
  encodeValue = coerce E.jsonb

-- | Encode anything that is an instance of 'Aeson.ToJSON' to a postgres @json@
instance Aeson.ToJSON a => EncodeValue (AsJson a) where
  encodeValue = BL.toStrict . Aeson.encode . coerce @_ @a >$< E.jsonBytes

-- | Encode anything that is an instance of 'Aeson.ToJSON' to a postgres @jsonb@
instance Aeson.ToJSON a => EncodeValue (AsJsonb a) where
  encodeValue = BL.toStrict . Aeson.encode . coerce @_ @a >$< E.jsonbBytes
