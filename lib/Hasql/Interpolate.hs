module Hasql.Interpolate
  ( -- * QuasiQuoters
    sql,
    Sql,

    -- * Interpolators
    interp,
    interpFoldl,
    interpWith,

    -- * Decoders
    DecodeValue (..),
    DecodeField (..),
    DecodeRow (..),
    DecodeResult (..),

    -- * Encoders
    EncodeValue (..),
    EncodeField,

    -- * Newtypes for decoding/encoding
    OneRow (..),
    OneColumn (..),
    RowsAffected (..),
    Json (..),
    Jsonb (..),
    JsonBytes (..),
    JsonbBytes (..),
    AsJson (..),
    AsJsonb (..),
    CompositeValue (..),

    -- * toTable
    toTable,
    EncodeRow (..),
  )
where

import Control.Monad.Trans.State.Strict (evalState)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import Hasql.Decoders (Result, foldlRows)
import Hasql.Interpolate.Internal.CompositeValue
import Hasql.Interpolate.Internal.Decoder
import Hasql.Interpolate.Internal.EncodeRow
import Hasql.Interpolate.Internal.Encoder
import Hasql.Interpolate.Internal.Json
import Hasql.Interpolate.Internal.OneColumn
import Hasql.Interpolate.Internal.OneRow
import Hasql.Interpolate.Internal.RowsAffected
import Hasql.Interpolate.Internal.Sql
import Hasql.Interpolate.Internal.TH
import qualified Hasql.Statement as Statement

-- | Interpolate a 'Sql' into a 'Statement' using the 'DecodeResult'
-- type class to determine the appropriate decoder.
--
-- @
-- example :: Int64 -> Statement () [(Int64, Int64)]
-- example bonk = interp False [sql| select x, y from t where t.x > #{bonk} |]
-- @
interp ::
  (DecodeResult b) =>
  -- | 'True' if the 'Statement' should be prepared
  Bool ->
  Sql ->
  Statement.Statement () b
interp prepared = interpWith prepared decodeResult

-- | interpolate then consume with 'foldlRows'
interpFoldl :: (DecodeRow a) => Bool -> (b -> a -> b) -> b -> Sql -> Statement.Statement () b
interpFoldl prepared f z = interpWith prepared (foldlRows f z decodeRow)

-- | A more general version of 'interp' that allows for passing an
-- explicit decoder.
interpWith :: Bool -> Result b -> Sql -> Statement.Statement () b
interpWith prepare decoder (Sql bldr enc) =
  if prepare
    then Statement.preparable (TL.toStrict $ Builder.toLazyText $ evalState bldr 1) enc decoder
    else Statement.unpreparable (TL.toStrict $ Builder.toLazyText $ evalState bldr 1) enc decoder
