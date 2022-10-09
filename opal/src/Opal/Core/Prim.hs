module Opal.Core.Prim
  ( -- * Primitives
    Prim
      ( PrimCase, 
        PrimClauseDef,
        PrimStxExpr,
        PrimMakeStx,
        PrimSetMut,
        PrimBoolTrue,
        PrimBoolFalse,
        PrimSyntax,
        PrimLambda,
        PrimLetSyntax,
        PrimQuote,
        PrimVoid
      ),

    primToSymbol,
    primToName,
    primSourceSpan,
  )
where

import Data.Data (Data)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol
import Opal.Common.Name (Name)
import Opal.Common.Name qualified as Name

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Prim
  = PrimCase
  | PrimClauseDef
  | PrimBoolFalse
  | PrimBoolTrue
  | PrimMakeStx
  | PrimLambda
  | PrimLetSyntax
  | PrimQuote
  | PrimSetMut
  | PrimStxExpr
  | PrimSyntax 
  | PrimVoid
  deriving (Data, Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
primToSymbol :: Prim -> Symbol
primToSymbol prim = Symbol.Symbol (primToName prim)

-- | TODO
--
-- @since 1.0.0
primToName :: Prim -> Name
primToName PrimCase = Name.pack "case"
primToName PrimClauseDef = Name.pack "def"
primToName PrimBoolFalse = Name.pack "#f"
primToName PrimBoolTrue = Name.pack "#t"
primToName PrimMakeStx = Name.pack "mk-stx"
primToName PrimLambda = Name.pack "lambda"
primToName PrimLetSyntax = Name.pack "let-syntax"
primToName PrimQuote = Name.pack "quote"
primToName PrimSetMut = Name.pack "set!"
primToName PrimStxExpr = Name.pack "stx-e"
primToName PrimSyntax = Name.pack "syntax"
primToName PrimVoid = Name.pack "#<void>"

-- | TODO
--
-- @since 1.0.0
primSourceSpan :: Prim -> Int
primSourceSpan prim = Symbol.size (primToSymbol prim) 