{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasql.Interpolate.Internal.EncodeRow.TH where

import Control.Monad
import Data.Foldable (foldl')
import Data.Functor.Contravariant
import qualified Hasql.Encoders as E
import Hasql.Interpolate.Internal.Encoder (EncodeField (..), EncodeValue (..))
import Language.Haskell.TH

-- | Generate a single 'EncodeRow' instance for a tuple of size
-- @tupSize@
genEncodeRowInstance ::
  -- | tuple size
  Int ->
  Q Dec
genEncodeRowInstance tupSize
  | tupSize < 2 = fail "this is just for tuples, must specify a tuple size of 2 or greater"
  | otherwise = do
    tyVars <- replicateM tupSize (newName "x")
    context <- traverse (\x -> [t|EncodeValue $(varT x)|]) tyVars
    let unzipWithEncoderName = mkName "unzipWithEncoder"
    instanceHead <- [t|$(conT (mkName "EncodeRow")) $(pure $ foldl' AppT (TupleT tupSize) (map VarT tyVars))|]
    innerContName <- newName "k"
    cons <- [e|(:)|]
    kconsTailNames <- traverse (\_ -> newName "tail") tyVars
    let kconsPats :: [Pat]
        kconsPats =
          [ TupP (map VarP tyVars),
            TildeP (TupP (map VarP kconsTailNames))
          ]
        kconsTupBody :: [Exp]
        kconsTupBody =
          let vars = zipWith phi tyVars kconsTailNames
              phi headName tailName = foldl' AppE cons [VarE headName, VarE tailName]
           in vars
        kcons :: Exp
        kcons = LamE kconsPats (TupE (map Just kconsTupBody))
        knil :: Exp
        knil = TupE . map Just $ replicate tupSize (ListE [])
    kenc :: Exp <- do
      let listEncoder = [e|E.param (E.nonNullable (E.foldableArray encodeField))|]
          plucks = map (pluck tupSize) [0 .. tupSize - 1]
      encExps <- traverse (\getTupElem -> [e|contramap $getTupElem $listEncoder|]) plucks
      foldr (\a b -> [e|$(pure a) <> $(b)|]) [e|mempty|] encExps
    let kExp :: Exp
        kExp = foldl' AppE (VarE innerContName) [kcons, knil, kenc, LitE (IntegerL (fromIntegral tupSize))]
    let instanceBody = FunD unzipWithEncoderName [Clause [VarP innerContName] (NormalB kExp) []]
    pure (InstanceD Nothing context instanceHead [instanceBody])

-- | Generate tuple instances for 'EncodeRow' up to tuple size
-- @upTo@
genEncodeRowInstances :: Int -> Q [Dec]
genEncodeRowInstances upTo = do
  traverse genEncodeRowInstance [2 .. upTo]

pluck :: Int -> Int -> Q Exp
pluck 1 0 = [e|id|]
pluck tupSize idx = do
  matchName <- newName "match"
  let tupPat = TupP (map (\n -> if n == idx then VarP matchName else WildP) [0 .. tupSize - 1])
  pure $ LamE [tupPat] (VarE matchName)
