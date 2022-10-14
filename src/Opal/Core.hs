{-# LANGUAGE StandaloneKindSignatures #-}

module Opal.Core
  ( -- * Expressions
    Expr,

    -- * S-Expressions
    SExp (SExpVar, SExpVal, SExpApp),

    -- ** Deconstruction
    toVarRef,

    -- * Datums
    Datum
      ( DatumAtom,
        DatumCase,
        DatumCore,
        DatumPrim,
        DatumList,
        DatumProc,
        DatumStx
      ),

    -- ** Construction
    atomToDatum,
    syntaxToDatum,

    -- ** Deconstruction
    toSyntax,
    toAtom,
    toSymbol,

    -- * Procedures
    Procedure (Procedure, formals, body),

    -- * Procedures
    Clause (Clause, datum, body),

    -- * Atoms
    Atom (Atom, Core),

    -- * Primitives
    CoreForm (CoreFormLambda, CoreFormLetSyntax, CoreFormQuote, CoreFormSyntax),
    primToSymbol,

    -- ** Primitive Operations
    primStxExpr,
    primMakeStx,
  )
where

import Data.Kind (Type)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Core.Atom (Atom (Atom, Core))
import Opal.Core.CoreForm
  ( CoreForm (CoreFormLambda, CoreFormLetSyntax, CoreFormQuote, CoreFormSyntax),
    primToSymbol,
  )
import Opal.Core.Datum
  ( Clause (Clause, body, datum),
    Datum
      ( DatumAtom,
        DatumCase,
        DatumCore,
        DatumList,
        DatumPrim,
        DatumProc,
        DatumStx
      ),
    Procedure (Procedure, body, formals),
    atomToDatum,
    syntaxToDatum,
    toAtom,
    toSymbol,
    toSyntax,
  )
import Opal.Core.SExp (SExp (SExpApp, SExpVal, SExpVar))

import Opal.Expand.Syntax (Syntax (StxAtom, StxList, context))

-- Expressions -----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
type Expr :: Type
type Expr = SExp Datum

-- S-Expressions - Deconstruction ----------------------------------------------

-- | TODO
--
-- @since 1.0.0
toVarRef :: SExp a -> Maybe Name
toVarRef (SExpVar name) = Just name
toVarRef _ = Nothing
{-# INLINE toVarRef #-}

-- Primitives - Primitive Operations  ------------------------------------------

-- | TODO
--
-- @since 1.0.0
primStxExpr :: Syntax -> Datum
primStxExpr (StxAtom _ name) = DatumAtom name
primStxExpr (StxList _ stxs) = DatumList (map DatumStx stxs)
{-# INLINE primStxExpr #-}

-- | TODO
--
-- @since 1.0.0
primMakeStx :: Atom -> Syntax -> Syntax
primMakeStx (Atom name) stx = StxAtom stx.context name
primMakeStx (Core form) stx = StxAtom stx.context (primToSymbol form)
{-# INLINE primMakeStx #-}