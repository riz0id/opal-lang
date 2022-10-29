{-# LANGUAGE OverloadedStrings #-}

module Opal.Core.Prim
  ( -- * Core Primitives
    CorePrim (..),
    
    -- ** Conversion
    toName,
    toSymbol,
  )
where

import Data.Data (Data)

import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

--------------------------------------------------------------------------------

-- | TODO
--
-- @since
data CorePrim
  = Apply 
  | Cons
  | DatumToSyntax
  | GenSym
  | Head
  | List 
  | Map
  | IsList 
  | IsProcedure
  | IsPair
  | IsSyntax 
  | IsSymbol
  | SyntaxToDatum
  | SyntaxLocalValue
  | SyntaxExpr
  | Tail
  deriving (Bounded, Data, Enum, Eq, Ord, Show, Lift)

-- Conversion ------------------------------------------------------------------

toName :: CorePrim -> Name
toName Apply = "apply"
toName Cons = "cons"
toName GenSym = "gensym"
toName List = "list"
toName Map = "map"
toName IsList = "list?"
toName Head = "head"
toName Tail = "tail"
toName IsProcedure = "procedure?"
toName IsSyntax = "syntax?"
toName IsPair = "pair?"
toName IsSymbol = "symbol?"
toName DatumToSyntax = "datum->syntax"
toName SyntaxToDatum = "syntax->datum"
toName SyntaxLocalValue = "syntax-local-value"
toName SyntaxExpr = "syntax-e"

toSymbol :: CorePrim -> Symbol
toSymbol prim = Symbol.Symbol (toName prim)
