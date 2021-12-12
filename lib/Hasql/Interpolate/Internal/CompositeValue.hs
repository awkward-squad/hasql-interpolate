{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasql.Interpolate.Internal.CompositeValue
  ( CompositeValue (..),
  )
where

import Data.Coerce
import GHC.Generics
import Hasql.Decoders
import Hasql.Interpolate.Internal.Decoder

-- | Useful with @DerivingVia@ to get a 'DecodeValue' instance for any
-- product type by parsing it as a composite.
--
-- ==== __Example__
--
-- @
-- data Point = Point Int64 Int64
--   deriving stock (Generic)
--   deriving (DecodeValue) via CompositeValue Point
-- @
newtype CompositeValue a
  = CompositeValue a

instance (Generic a, GToComposite (Rep a)) => DecodeValue (CompositeValue a) where
  decodeValue = coerce @(Value a) (composite (to <$> gtoComposite))

class GToComposite a where
  gtoComposite :: Composite (a p)

instance GToComposite a => GToComposite (M1 t i a) where
  gtoComposite = M1 <$> gtoComposite

instance (GToComposite a, GToComposite b) => GToComposite (a :*: b) where
  gtoComposite = (:*:) <$> gtoComposite <*> gtoComposite

instance DecodeValue a => GToComposite (K1 i a) where
  gtoComposite = K1 <$> field decodeField
