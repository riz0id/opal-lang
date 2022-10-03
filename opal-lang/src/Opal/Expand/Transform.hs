module Opal.Expand.Transform
  ( -- * Transformers
    Transform
      ( TfmLambda,
        TfmLetSyntax,
        TfmQuote,
        TfmSyntax,
        TfmVar,
        TfmDatum,
        TfmStop
      ),
    unstop,
  )
where

import Data.Data (Data)

--------------------------------------------------------------------------------

import Opal.Core.Datum (Datum)

import Opal.Expand.Syntax (Syntax)

-- Transformers ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Transform
  = TfmLambda
  | TfmLetSyntax
  | TfmQuote
  | TfmSyntax
  | TfmVar {-# UNPACK #-} !Syntax
  | TfmDatum Datum
  | TfmStop Transform
  deriving (Data, Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
unstop :: Transform -> Transform
unstop (TfmStop tfm) = tfm
unstop tfm = tfm
{-# INLINE unstop #-}