module Opal.Core.Atom
  ( -- * Atoms
    Atom (Atom, Core),
  )
where

import Data.Data (Data)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

import Opal.Core.CoreForm (CoreForm)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Atom 
  = Atom {-# UNPACK #-} !Symbol 
  | Core CoreForm 
  deriving (Data, Eq, Ord, Show)