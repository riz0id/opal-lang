module Opal.Expand.Transform
  ( -- * Transformers
    Transform (TfmVar, TfmDtm, TfmStop),
    unstop,
    unstopEnvironment,
  )
where

import Data.Data (Data)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Core.Datum (Datum)

import Opal.Expand.Syntax (StxIdt)

-- Transformers ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Transform
  = TfmVar {-# UNPACK #-} !StxIdt
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

-- | TODO
--
-- @since 1.0.0
unstopEnvironment :: Map Name Transform -> Map Name Transform
unstopEnvironment = Map.map unstop