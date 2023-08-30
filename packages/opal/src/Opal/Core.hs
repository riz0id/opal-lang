{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Core
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Core syntactic forms.
--
-- @since 1.0.0
module Opal.Core
  ( -- * CoreForm
    CoreForm (..)
    -- ** Basic Operations
  , coreFormIdentifier
  , coreFormSymbol
  , coreFormString
  )
where

import Data.Default (Default(..))
import Data.HashMap.Strict qualified as HashMap

import Opal.Common.Symbol (Symbol, stringToSymbol)
import Opal.Writer.Class (Display(..))
import Opal.Writer.Doc qualified as Doc
import Opal.Syntax (Identifier (..), SyntaxInfo (..))
import Opal.Syntax.ScopeInfo qualified as ScopeInfo

-- CoreForm --------------------------------------------------------------------

-- | 'CoreForm' is an enumeration of the Opal's core syntactic forms.
--
-- @since 1.0.0
data CoreForm
  = CoreApp
    -- ^ The enumeration for the "#%app" core syntactic form.
  | CoreBegin
    -- ^ The enumeration for the "begin" core syntactic form.
  | CoreBeginSyntax
    -- ^ The enumeration for the "begin-syntax" core syntactic form.
  | CoreDefine
    -- ^ The enumeration for the "define" core syntactic form.
  | CoreDefineSyntax
    -- ^ The enumeration for the "define-syntax" core syntactic form.
  | CoreLambda
    -- ^ The enumeration for the "lambda" core syntactic form.
  | CoreLetRec
    -- ^ The enumeration for the "letrec-syntaxes+values" core syntactic form.
  | CoreModule
    -- ^ The enumeration for the "module" core syntactic form.
  | CoreModuleBegin
    -- ^ The enumeration for the "module-begin" core syntactic form.
  | CoreImport
    -- ^ The enumeration for the "import" core syntactic form.
  | CoreExport
    -- ^ The enumeration for the "export" core syntactic form.
  | CoreQuote
    -- ^ The enumeration for the "quote" core syntactic form.
  | CoreSyntax
    -- ^ The enumeration for the "quote-syntax" core syntactic form.
  deriving (Bounded, Enum, Eq, Ord)

-- | @since 1.0.0
instance Display CoreForm where
  display = Doc.string . show

-- | @since 1.0.0
instance Show CoreForm where
  show x = '\'' : coreFormString x

-- CoreForm - Basic Operations -------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
coreFormIdentifier :: CoreForm -> Identifier
coreFormIdentifier form =
  Identifier
    { idt_symbol = coreFormSymbol form
    , idt_info   =
        SyntaxInfo
          { stx_info_source     = Nothing
          , stx_info_scopes     = ScopeInfo.insert Nothing def def
          , stx_info_properties = HashMap.empty
          }
    }

-- | Obtain the given core form's symbol.
--
-- @since 1.0.0
coreFormSymbol :: CoreForm -> Symbol
coreFormSymbol = stringToSymbol . coreFormString

-- | Obtain the string representation of the given 'CoreForm'.
--
-- @since 1.0.0
coreFormString :: CoreForm -> String
coreFormString CoreApp          = "#%app"
coreFormString CoreBegin        = "begin"
coreFormString CoreBeginSyntax  = "begin"
coreFormString CoreDefine       = "define"
coreFormString CoreDefineSyntax = "define-syntax"
coreFormString CoreLambda       = "lambda"
coreFormString CoreLetRec       = "letrec-syntaxes+values"
coreFormString CoreModule       = "module"
coreFormString CoreModuleBegin  = "module-begin"
coreFormString CoreExport       = "export"
coreFormString CoreImport       = "import"
coreFormString CoreQuote        = "quote"
coreFormString CoreSyntax       = "quote-syntax"
