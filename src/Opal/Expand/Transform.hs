module Opal.Expand.Transform
  ( -- * Transformers
    Transform (..),
    toDatum,
    unstop,
    unstopEnvironment,
  )
where

import Data.Data (Data)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Core.Form (CoreForm)
import Opal.Core.Form qualified as Core.Form
import Opal.Core.Datum (Datum)
import Opal.Core.Datum qualified as Datum

import Opal.Expand.Syntax (StxIdt)

-- Transformers ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Transform
  = Var {-# UNPACK #-} !StxIdt
  | Dtm Datum
  | Core CoreForm
  | Stop Transform
  deriving (Data, Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
toDatum :: Transform -> Datum
toDatum (Var idt) = Datum.Stx idt.syntax
toDatum (Dtm val) = val
toDatum (Core form) = Datum.Atom (Core.Form.toSymbol form)
toDatum (Stop form) = toDatum form

-- | TODO
--
-- @since 1.0.0
unstop :: Transform -> Transform
unstop (Stop tfm) = tfm
unstop tfm = tfm
{-# INLINE unstop #-}

-- | TODO
--
-- @since 1.0.0
unstopEnvironment :: Map Name Transform -> Map Name Transform
unstopEnvironment = Map.map unstop