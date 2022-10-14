module Opal.Core.CoreForm
  ( -- * Core Forms
    CoreForm
      ( CoreFormLambda,
        CoreFormLetSyntax,
        CoreFormQuote,
        CoreFormSyntax
      ),
    toSymbol,
    toName,
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
toSymbol :: CoreForm -> Symbol
toSymbol prim = Symbol.Symbol (toName prim)

-- | TODO
--
-- @since 1.0.0
toName :: CoreForm -> Name
toName CoreFormQuote = Name.pack "quote"
toName CoreFormSyntax = Name.pack "syntax"
toName CoreFormLambda = Name.pack "lambda"
toName CoreFormLetSyntax = Name.pack "let-syntax"

-- | TODO
--
-- @since 1.0.0
primSourceSpan :: CoreForm -> Int
primSourceSpan prim = Symbol.size (toSymbol prim)