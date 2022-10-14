module Opal.Expand.Transform
  ( -- * Transformers
    Transform
      ( TfmLambda,
        TfmLetSyntax,
        TfmQuote,
        TfmSyntax,
        TfmVar,
        TfmDtm,
        TfmStop
      ),
    unstop,
  )
where

import Data.Data (Data)

--------------------------------------------------------------------------------

import Opal.Core.Datum (Datum)

import Opal.Expand.Syntax (StxIdt)

-- Transformers ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Transform
  = TfmLambda
  | TfmLetSyntax
  | TfmQuote
  | TfmSyntax
  | TfmVar {-# UNPACK #-} !StxIdt
  | TfmDtm Datum
  | TfmStop Transform
  deriving (Data, Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
unstop :: Transform -> Transform
unstop (TfmStop tfm) = tfm
unstop tfm = tfm
{-# INLINE unstop #-}