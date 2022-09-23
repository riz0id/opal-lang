{-# LANGUAGE OverloadedStrings #-}

module Opal.AST.Atom
  ( -- * TODO
    Atom (Name, Prim),

    -- * Patterns
    pattern PrimStxExp,
    pattern PrimStxNew,

    -- * TODO
    toName,
  )
where

import Data.Data (Data)

import GHC.Records (HasField, getField)

import Prettyprinter (Pretty, pretty)
import Prettyprinter qualified as Print

--------------------------------------------------------------------------------

import Opal.AST.Prim (Prim)
import Opal.AST.Prim qualified as Prim

import Opal.Common.Symbol (Symbol)

--------------------------------------------------------------------------------

data Atom
  = Name Symbol
  | Prim Prim
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance Show Atom where 
  show (Name x) = "'" ++ show x
  show (Prim x) = "(#%prim . " ++ shows x ")"
  {-# INLINE show #-}

-- | @since 1.0.0
instance HasField "symbol" Atom Symbol where
  getField (Name name) = name
  getField (Prim prim) = Prim.symbol prim
  {-# INLINE getField #-}

-- | @since 1.0.0
instance Pretty Atom where
  pretty (Name x) = pretty x
  pretty (Prim x) = pretty x 
  {-# INLINE pretty #-}

  prettyList xs = Print.parens (Print.hsep $ map pretty xs)
  {-# INLINE prettyList #-}

-- Patterns --------------------------------------------------------------------

pattern PrimStxExp :: Atom 
pattern PrimStxExp = Prim Prim.StxExp

pattern PrimStxNew :: Atom 
pattern PrimStxNew = Prim Prim.StxExp

-- TODO ------------------------------------------------------------------------

toName :: Atom -> Symbol
toName (Name name) = name
toName (Prim prim) = Prim.symbol prim