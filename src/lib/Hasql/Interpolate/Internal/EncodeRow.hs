{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasql.Interpolate.Internal.EncodeRow
  ( EncodeRow (..),
    GEncodeRow (..),
    toTable,
  )
where

import Control.Monad
import Data.Functor.Contravariant
import Data.List (intersperse)
import Data.Monoid
import GHC.Generics
import qualified Hasql.Encoders as E
import Hasql.Interpolate.Internal.EncodeRow.TH
import Hasql.Interpolate.Internal.Encoder
import Hasql.Interpolate.Internal.Sql
import Hasql.Interpolate.Internal.TH (addParam)

class EncodeRow a where
  -- | The continuation @(forall x. (a -> x -> x) -> x -> E.Params x
  -- -> Int -> r)@ is given cons @(a -> x -> x)@ and nil @(x)@ for some
  -- existential type @x@ and an encoder (@'E.Params' x@) for @x@. An
  -- Int is also given to tally up how many sql fields are in the
  -- unzipped structure.
  --
  -- ==== __Example__
  --
  -- Consider the following manually written instance:
  --
  -- @
  -- data Blerg = Blerg Int64 Bool Text Char
  --
  -- instance EncodeRow Blerg where
  --   unzipWithEncoder k = k cons nil enc 4
  --     where
  --       cons (Blerg a b c d) ~(as, bs, cs, ds) =
  --         (a : as, b : bs, c : cs, d : ds)
  --       nil = ([], [], [], [])
  --       enc =
  --              ((\(x, _, _, _) -> x) >$< param encodeField)
  --           <> ((\(_, x, _, _) -> x) >$< param encodeField)
  --           <> ((\(_, _, x, _) -> x) >$< param encodeField)
  --           <> ((\(_, _, _, x) -> x) >$< param encodeField)
  -- @
  --
  -- We chose @([Int64], [Bool], [Text], [Char])@ as our existential
  -- type. If we instead use the default instance based on
  -- 'GEncodeRow' then we would produce the same code as the
  -- instance below:
  --
  -- @
  -- instance EncodeRow Blerg where
  --   unzipWithEncoder k = k cons nil enc 4
  --     where
  --       cons (Blerg a b c d) ~(~(as, bs), ~(cs, ds)) =
  --         ((a : as, b : bs), (c : cs, d : ds))
  --       nil = (([], []), ([], []))
  --       enc =
  --              ((\((x, _),      _) -> x) >$< param encodeField)
  --           <> ((\((_, x),      _) -> x) >$< param encodeField)
  --           <> ((\(_     , (x, _)) -> x) >$< param encodeField)
  --           <> ((\(_     , (_, x)) -> x) >$< param encodeField)
  -- @
  --
  -- The notable difference being we don't produce a flat tuple, but
  -- instead produce a balanced tree of tuples isomorphic to the
  -- balanced tree of @':*:'@ from the generic 'Rep' of @Blerg@.
  unzipWithEncoder :: (forall x. (a -> x -> x) -> x -> E.Params x -> Int -> r) -> r
  default unzipWithEncoder ::
    (Generic a, GEncodeRow (Rep a)) =>
    (forall x. (a -> x -> x) -> x -> E.Params x -> Int -> r) ->
    r
  unzipWithEncoder k = gUnzipWithEncoder \cons nil enc fc ->
    k (cons . from) nil enc fc
  {-# INLINE unzipWithEncoder #-}

class GEncodeRow a where
  gUnzipWithEncoder :: (forall x. (a p -> x -> x) -> x -> E.Params x -> Int -> r) -> r

-- | 'toTable' takes some list of products into the corresponding
-- relation in sql. It is applying the @unnest@ based technique
-- described [in the hasql
-- documentation](https://hackage.haskell.org/package/hasql-1.4.5.1/docs/Hasql-Statement.html#g:2).
--
-- ==== __Example__
--
-- Here is a small example that takes a haskell list and inserts it
-- into a table @blerg@ which has columns @x@, @y@, and @z@ of type
-- @int8@, @boolean@, and @text@ respectively.
--
-- @
-- toTableExample :: [(Int64, Bool, Text)] -> Statement () ()
-- toTableExample rowsToInsert =
--   interp [sql| insert into blerg (x, y, z) select * from ^{toTable rowsToInsert} |]
-- @
--
-- This is driven by the 'EncodeRow' type class that has a
-- default implementation for product types that are an instance of
-- 'Generic'. So the following also works:
--
-- @
-- data Blerg
--   = Blerg Int64 Bool Text
--   deriving stock (Generic)
--   deriving anyclass (EncodeRow)
--
-- toTableExample :: [Blerg] -> Statement () ()
-- toTableExample blergs =
--   interp [sql| insert into blerg (x, y, z) select * from ^{toTable blergs} |]
-- @
toTable :: EncodeRow a => [a] -> Sql
toTable xs = unzipWithEncoder \cons nil enc i ->
  let unzippedEncoder = foldr cons nil xs >$ enc
      queryString = getAp $ pure "unnest(" <> (mconcat . intersperse ", " <$> Ap (replicateM i addParam)) <> pure ")"
   in Sql queryString unzippedEncoder
{-# INLINE toTable #-}

instance GEncodeRow x => GEncodeRow (M1 t i x) where
  gUnzipWithEncoder k = gUnzipWithEncoder \cons nil enc i ->
    k (\(M1 a) -> cons a) nil enc i
  {-# INLINE gUnzipWithEncoder #-}

instance (GEncodeRow a, GEncodeRow b) => GEncodeRow (a :*: b) where
  gUnzipWithEncoder k = gUnzipWithEncoder \consa nila enca ia -> gUnzipWithEncoder \consb nilb encb ib ->
    k
      ( \(a :*: b) ~(as, bs) ->
          (consa a as, consb b bs)
      )
      (nila, nilb)
      (contramap fst enca <> contramap snd encb)
      (ia + ib)
  {-# INLINE gUnzipWithEncoder #-}

instance EncodeField a => GEncodeRow (K1 i a) where
  gUnzipWithEncoder k =
    k (\(K1 a) b -> a : b) [] (E.param (E.nonNullable (E.foldableArray encodeField))) 1
  {-# INLINE gUnzipWithEncoder #-}

$(traverse genEncodeRowInstance [2 .. 8])
