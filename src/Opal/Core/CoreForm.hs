module Opal.Core.CoreForm
  ( -- * Core Forms
    CoreForm
      ( CoreFormLambda,
        CoreFormLetSyntax,
        CoreFormQuote,
        CoreFormSyntax
      ),
    primToSymbol,
    primToName,
    primSourceSpan,
  )
where

import Data.Data (Data)

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
  = CoreFormLambda
  | CoreFormLetSyntax
  | CoreFormQuote
  | CoreFormSyntax
  deriving (Data, Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
primToSymbol :: CoreForm -> Symbol
primToSymbol prim = Symbol.Symbol (primToName prim)

-- | TODO
--
-- @since 1.0.0
primToName :: CoreForm -> Name
primToName CoreFormLambda = Name.pack "lambda"
primToName CoreFormLetSyntax = Name.pack "let-syntax"
primToName CoreFormQuote = Name.pack "quote"
primToName CoreFormSyntax = Name.pack "syntax"

-- | TODO
--
-- @since 1.0.0
primSourceSpan :: CoreForm -> Int
primSourceSpan prim = Symbol.size (primToSymbol prim)