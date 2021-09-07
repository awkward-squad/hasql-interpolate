{-# LANGUAGE TemplateHaskell #-}

module Hasql.Interpolate.Internal.Decoder.TH
  ( genDecodeRowInstances,
  )
where

import Control.Monad
import Data.Foldable (foldl')
import Hasql.Decoders
import Language.Haskell.TH

-- | Generate a single 'DecodeRow' instance for a tuple of size
-- @tupSize@
genDecodeRowInstance ::
  -- | tuple size
  Int ->
  Q Dec
genDecodeRowInstance tupSize
  | tupSize < 2 = fail "this is just for tuples, must specify a tuple size of 2 or greater"
  | otherwise = do
    tyVars <- replicateM tupSize (newName "x")
    context <- traverse (\x -> [t|$(conT (mkName "DecodeField")) $(varT x)|]) tyVars
    instanceHead <- [t|$(conT (mkName "DecodeRow")) $(pure $ foldl' AppT (TupleT tupSize) (map VarT tyVars))|]
    let tupSection = TupE (replicate tupSize Nothing)
        go b _a = do
          [e|$(b) <*> column decodeField|]

    instanceBodyExp <- foldl' go [e|$(pure tupSection) <$> column decodeField|] (tail tyVars)
    let instanceBody = FunD (mkName "decodeRow") [Clause [] (NormalB instanceBodyExp) []]
    pure (InstanceD Nothing context instanceHead [instanceBody])

-- | Generate tuple instances for 'DecodeRow' up to tuple size
-- @upTo@
genDecodeRowInstances :: Int -> Q [Dec]
genDecodeRowInstances upTo = do
  traverse genDecodeRowInstance [2 .. upTo]
