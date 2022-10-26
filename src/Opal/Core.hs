{-# LANGUAGE StandaloneKindSignatures #-}

module Opal.Core (
  -- * Declarations
  Decl(DeclDefn, DeclDefnStx, DeclSExp),

  -- * Expressions
  Expr,

  -- * S-Expressions
  SExp (..),

  -- ** Deconstruction
  toVarRef,

  -- * Primitives
  CoreForm (..),
  CorePrim (..),
) where

import Data.Kind (Type)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Core.Form (CoreForm (..))
import Opal.Core.Prim (CorePrim (..))
import Opal.Core.SExp (SExp (..))

import Opal.Expand.Syntax (StxIdt)
import Opal.Core.Datum (Datum)

-- Declarations ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Decl 
  = DeclDefn StxIdt Expr
  | DeclDefnStx StxIdt Expr
  | DeclSExp Expr

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
