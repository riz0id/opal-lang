{-# LANGUAGE DuplicateRecordFields #-}

module Opal.Core.Datum 
  ( -- * Datums
    Datum (DatumStx, DatumAtom, DatumProc, DatumCase, DatumPrim, DatumList),

    -- * Construction
    atomToDatum,
    syntaxToDatum,

    -- * Deconstruction
    toSyntax,
    toAtom,
    toSymbol,

    -- * Procedures 
    Procedure (Procedure, formals, body),

    -- * Case Clauses
    Clause (Clause, datum, body),
  )  
where

import Data.Data (Data)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)

import Opal.Core.SExp (SExp)
import Opal.Core.Atom (Atom (Atom, Prim))
import Opal.Core.Prim (Prim)

import Opal.Expand.Syntax (Syntax (StxAtom, StxList))

-- Datums ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Datum 
  = DatumStx {-# UNPACK #-} !Syntax
  | DatumAtom {-# UNPACK #-} !Symbol
  | DatumPrim Prim
  | DatumProc [Name] (SExp Datum)
  | DatumCase (SExp Datum) [Clause]
  | DatumList [Datum]
  deriving (Data, Eq, Ord, Show)

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
atomToDatum :: Atom -> Datum
atomToDatum (Atom name) = DatumAtom name
atomToDatum (Prim prim) = DatumPrim prim

-- | TODO
--
-- @since 1.0.0
syntaxToDatum :: Syntax -> Datum
syntaxToDatum (StxAtom _ name) = DatumAtom name
syntaxToDatum (StxList _ stxs) = DatumList (map syntaxToDatum stxs)

-- Deconstruction --------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
toSyntax :: Datum -> Maybe Syntax 
toSyntax (DatumStx stx) = Just stx
toSyntax _ = Nothing
{-# INLINE toSyntax #-}

-- | TODO
--
-- @since 1.0.0
toAtom :: Datum -> Maybe Atom
toAtom (DatumAtom name) = Just (Atom name)
toAtom (DatumPrim prim) = Just (Prim prim)
toAtom _ = Nothing
{-# INLINE toAtom #-}

-- | TODO
--
-- @since 1.0.0
toSymbol :: Datum -> Maybe Symbol
toSymbol val = do 
  Atom name <- toAtom val
  pure name
{-# INLINE toSymbol #-}

-- Procedures ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Procedure = Procedure 
  { formals :: [Name]
  , body :: SExp Datum
  }
  deriving (Data, Eq, Ord, Show)

-- Case Clauses ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Clause = Clause 
  { datum :: Datum 
  , body :: SExp Datum
  }
  deriving (Data, Eq, Ord, Show)