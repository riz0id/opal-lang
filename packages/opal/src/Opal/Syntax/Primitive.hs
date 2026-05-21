{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Syntax.Primitive
-- Copyright   :  (c) Jacob Leach, 2026
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Definition of the 'Primitive' record — a built-in function callable from
-- the transformer language. The accompanying table of primitive
-- implementations lives in "Opal.Primitives" (this module just defines the
-- type so it can be imported without a dependency on 'Opal.Syntax' or the
-- evaluator).
--
-- 'Datum' carries primitives indirectly via the 'DatumPrim' constructor,
-- which stores just the primitive's name. The evaluator resolves a name to
-- its 'Primitive' implementation via 'Opal.Primitives.lookupPrimitive' at
-- apply time.
--
-- @since 1.0.0
module Opal.Syntax.Primitive
  ( Primitive (..)
  )
where

import Opal.Common.Symbol (Symbol)
import Opal.Syntax (Datum)

-- Primitive -------------------------------------------------------------------

-- | A built-in function. 'prim_apply' is a pure Haskell function over already-
-- evaluated 'Datum' arguments; the evaluator handles arity checking using
-- 'prim_arity' and surfaces 'Left' errors as 'EvalError's.
--
-- For Stage 1 the apply function is pure — none of the initial primitives
-- need to touch the evaluator's state or config. If later primitives do
-- (e.g. @free-identifier=?@ needs the binding store), this field type can
-- be promoted to @[Datum] -> Eval Datum@.
--
-- @since 1.0.0
data Primitive = Primitive
  { prim_name  :: {-# UNPACK #-} !Symbol
    -- ^ Used for diagnostics and as the lookup key in
    -- 'Opal.Primitives.primitiveTable'.
  , prim_arity :: {-# UNPACK #-} !Int
    -- ^ Number of arguments the primitive expects. The evaluator
    -- throws an arity error before calling 'prim_apply' if mismatched.
  , prim_apply :: [Datum] -> Either String Datum
    -- ^ The actual implementation. Receives evaluated arguments;
    -- returns 'Right' on success, 'Left' with an error message on
    -- failure.
  }
