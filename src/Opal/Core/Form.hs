module Opal.Core.Form
  ( -- * Core Forms
    CoreForm (..),

    -- ** Conversion
    toSymbol,
    toName,

    -- ** Doc
    docCoreForm,
  )
where

import Data.Data (Data)

import Language.Haskell.TH.Syntax (Lift)

import Text.Emit (Doc, emit)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Name qualified as Name
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

-- CoreForm --------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data CoreForm
  = DefineValue 
  | DefineSyntaxValue
  | If
  | Lambda
  | Let
  | LetRec
  | LetSyntax
  | Module
  | Quote
  | Require
  | QuoteSyntax
  | QuasiSyntax
  | Unsyntax
  deriving (Bounded, Data, Enum, Eq, Ord, Show, Lift)

-- CoreForm - Conversion -------------------------------------------------------

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
toName DefineSyntaxValue = Name.pack "define-syntax-value"
toName If = Name.pack "if"
toName Lambda = Name.pack "lambda"
toName Let = Name.pack "let"
toName LetRec = Name.pack "let-rec"
toName LetSyntax = Name.pack "let-syntax"
toName Module = Name.pack "module"
toName Quote = Name.pack "quote"
toName Require = Name.pack "#%require"
toName QuoteSyntax = Name.pack "quote-syntax"
toName QuasiSyntax = Name.pack "quasisyntax"
toName Unsyntax = Name.pack "unsyntax"

-- CoreForm - Doc --------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docCoreForm :: CoreForm -> Doc a
docCoreForm form = emit (toName form)