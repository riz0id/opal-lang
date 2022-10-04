{-# LANGUAGE OverloadedRecordDot #-}

module Opal.Expand.Syntax.Binding
  ( -- * TODO
    Binder (BindPrim, BindName),

    -- * TODO
    Binding (Binding, binder, scopes),

    -- * Scope Set Operations
    overlaps,
    superset,

    -- * Folds
    maximum,
  )
where

import Control.Monad.ST.Strict (runST)

import Data.Data (Data)
import Data.Foldable (for_)
import Data.Primitive.MutVar (modifyMutVar', newMutVar, readMutVar, writeMutVar)
import Data.Set (Set)
import Data.Set qualified as Set

import Prelude hiding (maximum)

--------------------------------------------------------------------------------

import Opal.Core.Prim (Prim)
import Opal.Common.Name (Name)

import Opal.Expand.Syntax.ScopeSet (ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Binder 
  = BindPrim Prim 
  | BindName Name
  deriving (Data, Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Binding = Binding
  { scopes :: ScopeSet
  , binder :: Binder
  }
  deriving (Data, Eq, Ord, Show)

-- Scope Set Operations --------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
overlaps :: Binding -> Binding -> Bool
overlaps a b = ScopeSet.overlaps (scopes a) (scopes b)

-- | TODO
--
-- @since 1.0.0
superset :: Binding -> Binding -> Bool
superset a b = ScopeSet.superset (scopes a) (scopes b)

-- Folds -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
maximum :: Set Binding -> Set Binding
maximum bindings = runST do
  mutSize <- newMutVar 0
  mutMaxs <- newMutVar Set.empty

  for_ (Set.toList bindings) \binding -> do
    len <- readMutVar mutSize
    case compare len (ScopeSet.size binding.scopes) of
      GT -> pure ()
      EQ -> modifyMutVar' mutMaxs (Set.insert binding)
      LT -> do
        writeMutVar mutSize (ScopeSet.size binding.scopes)
        writeMutVar mutMaxs (Set.singleton binding)

  readMutVar mutMaxs