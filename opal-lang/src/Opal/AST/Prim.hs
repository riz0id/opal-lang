{-# LANGUAGE OverloadedStrings #-}

module Opal.AST.Prim
  ( -- * TODO
    Prim (StxExp, StxNew),
    symbol,
  )
where

import Data.Data (Data)

import GHC.Records (HasField, getField)

import Prettyprinter (Pretty, pretty)
import Prettyprinter qualified as Print

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

--------------------------------------------------------------------------------

data Prim
  = StxExp
  | StxNew
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance HasField "symbol" Prim Symbol where
  getField StxExp = Symbol.pack "stx-exp"
  getField StxNew = Symbol.pack "make-stx"
  {-# INLINE getField #-}

-- | @since 1.0.0
instance Pretty Prim where
  pretty StxExp = "(#%prim . stx-exp)"
  pretty StxNew = "(#%prim . stx-new)"
  {-# INLINE pretty #-}

  prettyList xs = Print.parens (Print.hsep $ map pretty xs)
  {-# INLINE prettyList #-}

instance Show Prim where 
  show StxExp = "stx-exp" 
  show StxNew = "stx-new"
  {-# INLINE show #-}

symbol :: Prim -> Symbol
symbol StxExp = Symbol.pack "stx-exp"
symbol StxNew = Symbol.pack "make-stx"