{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Core.Datum (
  -- * Datums
  Datum (..),

  -- ** Construction
  syntaxToDatum,
  syntaxExpr,

  -- ** Deconstruction
  -- datumToSyntax,

  -- ** Predicates
  isListDatum,
  isSyntaxDatum,
  isProcDatum,
) where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)

import Opal.Core.Prim (CorePrim)
import Opal.Core.SExp (SExp)

import Opal.Expand.Syntax (Syntax)
import Opal.Expand.Syntax qualified as Syntax

-- Datums ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Datum
  = Stx {-# UNPACK #-} !Syntax
  | Atom {-# UNPACK #-} !Symbol
  | Bool Bool
  | Prim CorePrim
  | Proc [Name] (NonEmpty (SExp Datum))
  | List [Datum]
  deriving (Data, Eq, Ord, Show)

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
syntaxToDatum :: Syntax -> Datum
syntaxToDatum (Syntax.StxBool _ bool) = Bool bool
syntaxToDatum (Syntax.StxAtom _ name) = Atom name
syntaxToDatum (Syntax.StxList _ stxs) = List (map syntaxToDatum stxs)

-- | TODO
--
-- @since 1.0.0
syntaxExpr :: Syntax -> Datum
syntaxExpr (Syntax.StxBool _ bool) = Bool bool
syntaxExpr (Syntax.StxAtom _ atom) = Atom atom
syntaxExpr (Syntax.StxList _ stxs) = List (map Stx stxs)

-- Deconstruction --------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
-- datumToSyntax :: Syntax -> Datum -> Syntax
-- datumToSyntax orig datum = case datum of
--   Stx stx -> stx
--   Atom atom -> Syntax.makeSyntax orig atom
--   Bool bool -> Syntax.makeSyntax orig (if bool then "#t" else "#f")
--   Prim prim -> Syntax.makeSyntax orig (Core.Prim.toSymbol prim)

-- Predicates ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
isListDatum :: Datum -> Bool
isListDatum List {} = True
isListDatum _ = False

-- | TODO
--
-- @since 1.0.0
isSyntaxDatum :: Datum -> Bool
isSyntaxDatum Stx {} = True
isSyntaxDatum _ = False

-- | TODO
--
-- @since 1.0.0
isProcDatum :: Datum -> Bool
isProcDatum Proc {} = True
isProcDatum _ = False