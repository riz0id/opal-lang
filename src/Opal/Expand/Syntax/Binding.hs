{-# LANGUAGE OverloadedRecordDot #-}

module Opal.Expand.Syntax.Binding
  ( -- * TODO
    Binding (Binding, binder, scopes),

    -- * Construction
    makeCoreBinding,

    -- * Scope Set Operations
    scope,
    overlaps,
    superset,

    -- * Predicates
    subset,

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

import Opal.Common.Name (Name)

import Opal.Expand.Syntax.ScopeSet (ScopeId, ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Binding = Binding
  { scopes :: ScopeSet
  , binder :: {-# UNPACK #-} !Name
  }
  deriving (Data, Eq, Ord, Show)

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
makeCoreBinding :: Name -> Binding
makeCoreBinding = Binding (ScopeSet.singleton 0) 

-- Scope Set Operations --------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
scope :: ScopeId -> Binding -> Binding
scope sc binding = binding {scopes = ScopeSet.insert sc binding.scopes}

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

-- Predicates ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
subset :: Binding -> ScopeSet -> Bool
subset binding = ScopeSet.subset (scopes binding)

-- Folds -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
maximum :: Set Binding -> Maybe Binding
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

  result <- readMutVar mutMaxs
  case Set.maxView result of
    Nothing -> pure Nothing
    Just rx -> pure (Just $ fst rx)
