module Opal.Core.Prim
  ( -- * Primitives
    Prim
      ( PrimStxExpr,
        PrimMakeStx,
        PrimSetMut,
        PrimBoolTrue,
        PrimBoolFalse,
        PrimSyntax,
        PrimLambda,
        PrimLetSyntax,
        PrimQuote
      ),

    primToSymbol,
    primSourceSpan,
  )
where

import Data.Data (Data)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Prim
  = PrimStxExpr
  | PrimMakeStx
  | PrimSetMut
  | PrimBoolTrue
  | PrimBoolFalse
  | PrimSyntax 
  | PrimLambda
  | PrimLetSyntax
  | PrimQuote
  deriving (Data, Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
primToSymbol :: Prim -> Symbol
primToSymbol PrimStxExpr = Symbol.pack "stx-e"
primToSymbol PrimMakeStx = Symbol.pack "mk-stx"
primToSymbol PrimSetMut = Symbol.pack "set!"
primToSymbol PrimBoolFalse = Symbol.pack "#f"
primToSymbol PrimBoolTrue = Symbol.pack "#t"
primToSymbol PrimSyntax = Symbol.pack "syntax"
primToSymbol PrimLambda = Symbol.pack "lambda"
primToSymbol PrimLetSyntax = Symbol.pack "let-syntax"
primToSymbol PrimQuote = Symbol.pack "quote"

-- | TODO
--
-- @since 1.0.0
primSourceSpan :: Prim -> Int
primSourceSpan prim = Symbol.size (primToSymbol prim) 