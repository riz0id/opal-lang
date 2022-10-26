module Opal.Core.Form
  ( -- * Core Forms
    CoreForm (..),
    toSymbol,
    toName,
    primSourceSpan,
  )
where

import Data.Data (Data)

import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Name qualified as Name
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data CoreForm
  = DefineValue 
  | DefineSyntax
  | Lambda
  | Let
  | LetSyntax
  | Module
  | Quote
  | Require
  | Syntax
  | QuasiSyntax
  | Unsyntax
  deriving (Bounded, Data, Enum, Eq, Ord, Show, Lift)

-- | TODO
--
-- @since 1.0.0
toSymbol :: CoreForm -> Symbol
toSymbol prim = Symbol.Symbol (toName prim)

-- | TODO
--
-- @since 1.0.0
toName :: CoreForm -> Name
toName DefineValue = Name.pack "define-value"
toName DefineSyntax = Name.pack "define-syntax"
toName Lambda = Name.pack "lambda"
toName Let = Name.pack "let"
toName LetSyntax = Name.pack "let-syntax"
toName Module = Name.pack "module"
toName Quote = Name.pack "quote"
toName Require = Name.pack "#%require"
toName Syntax = Name.pack "syntax"
toName QuasiSyntax = Name.pack "quasisyntax"
toName Unsyntax = Name.pack "unsyntax"

-- | TODO
--
-- @since 1.0.0
primSourceSpan :: CoreForm -> Int
primSourceSpan prim = Symbol.length (toSymbol prim)