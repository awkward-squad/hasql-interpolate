{-# LANGUAGE OverloadedStrings #-}

module Hasql.Interpolate.Internal.Sql
  ( Sql (..),
  )
where

import Control.Monad.Trans.State.Strict
import Data.ByteString.Builder
import Data.String (IsString (..))
import Hasql.Encoders

-- | A SQL string with interpolated expressions.
data Sql = Sql
  { -- | The sql string. It is stateful over an 'Int' in order to
    -- assign the postgresql parameter placeholders (e.g. @$1@, @$2@)
    sqlTxt :: State Int Builder,
    -- | The encoders associated with the sql string. Already applied
    -- to their parameters.
    encoder :: Params ()
  }

instance IsString Sql where
  fromString str = Sql (pure (stringUtf8 str)) mempty

instance Semigroup Sql where
  a <> b =
    Sql
      { sqlTxt =
          ( (<>) <$> sqlTxt a <*> sqlTxt b
          ),
        encoder = encoder a <> encoder b
      }
  {-# INLINE (<>) #-}

instance Monoid Sql where
  mempty =
    Sql
      { sqlTxt = pure mempty,
        encoder = mempty
      }
  {-# INLINE mempty #-}
