
module Opal.Expand.Transformer
  ( -- * Transformers
    Transform (VarTfm, LamTfm, QteTfm, StxTfm, LetTfm, DtmTfm, StopTfm),
    unstop
  )
where

import Data.Kind (Type)
import Data.Data (Data)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)
import Opal.Expr (Datum)

-- Transformers ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Transform :: Type where
  -- | The constructor for a variable transformer. Variable transformers should
  -- only be references to a lambda argument, all other symbols appear as quoted
  -- 'Symbol' datums.
  VarTfm :: Symbol -> Transform
  -- | The constructor for a @lambda@ transformer.
  LamTfm :: Transform
  -- | The constructor for a @quote@ transformer.
  QteTfm :: Transform
  -- | The constructor for a @syntax@ transformer.
  StxTfm :: Transform
  -- | The constructor for a @let-syntax@ transformer.
  LetTfm :: Transform
  -- | The constructor for a 'Datum' transformer.
  DtmTfm :: Datum -> Transform
  -- | TODO
  StopTfm :: Transform -> Transform
  deriving (Data, Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
unstop :: Transform -> Transform 
unstop (StopTfm tfm) = tfm
unstop tfm = tfm
{-# INLINE unstop #-}