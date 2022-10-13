module Opal.Core.Atom
  ( -- * Atoms
    Atom (Atom, Prim),
  )
where

import Data.Data (Data)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

import Opal.Core.Prim (Prim)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Atom 
  = Atom {-# UNPACK #-} !Symbol 
  | Prim Prim 
  deriving (Data, Eq, Ord, Show)