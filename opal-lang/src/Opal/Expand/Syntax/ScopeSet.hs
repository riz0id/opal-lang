{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Opal.Expand.Syntax.ScopeSet
  ( -- * ScopeId 
    ScopeId (ScopeId),
  
    -- * ScopeSet 
    ScopeSet (ScopeSet),
    union,
    difference,
    intersect,
    flipscope,
    flipscopes,

    -- * Construction
    empty,
    singleton,

    -- * Comparison
    subset,
    superset,
    overlaps,

    -- * Query
    size,
    null,
    member,

    -- * Insert
    insert,

    -- * Delete
    delete,

    -- * Folds
    foldr'
  )
where

import Data.Data (Data)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Coerce (coerce)

import Prelude hiding (null)

-- ScopeId ---------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
newtype ScopeId = ScopeId Int 
  deriving (Enum, Eq, Integral, Ord, Num, Real, Show)

-- ScopeSet --------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
newtype ScopeSet :: Type where 
  ScopeSet :: IntSet -> ScopeSet 
  deriving (Data, Eq, Ord, Show)

-- | @since 1.0.0
instance Semigroup ScopeSet where 
  xs <> ys = coerce IntSet.union xs ys
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance Monoid ScopeSet where 
  mempty = ScopeSet IntSet.empty
  {-# INLINE mempty #-}

-- | TODO 
--
-- @since 1.0.0
union :: ScopeSet -> ScopeSet -> ScopeSet
union = coerce IntSet.union 

-- | TODO 
--
-- @since 1.0.0
difference :: ScopeSet -> ScopeSet -> ScopeSet
difference = coerce IntSet.difference

-- | TODO 
--
-- @since 1.0.0
intersect :: ScopeSet -> ScopeSet -> ScopeSet
intersect = coerce IntSet.intersection

-- | TODO 
--
-- @since 1.0.0
flipscope :: ScopeId -> ScopeSet -> ScopeSet 
flipscope sid set
  | member sid set = delete sid set
  | otherwise = insert sid set
{-# INLINE flipscope #-}

-- | TODO 
--
-- @since 1.0.0
flipscopes :: ScopeSet -> ScopeSet -> ScopeSet 
flipscopes set0 set1 = foldr' flipscope set1 set0
{-# INLINE flipscopes #-}

-- Construction ----------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
empty :: ScopeSet 
empty = ScopeSet IntSet.empty

-- | TODO 
--
-- @since 1.0.0
singleton :: ScopeId -> ScopeSet 
singleton = coerce IntSet.singleton

-- Comparison ------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
subset :: ScopeSet -> ScopeSet -> Bool
subset = coerce IntSet.isProperSubsetOf

-- | TODO 
--
-- @since 1.0.0
superset :: ScopeSet -> ScopeSet -> Bool
superset a b = coerce IntSet.isSubsetOf b a

-- | TODO 
--
-- @since 1.0.0
overlaps :: ScopeSet -> ScopeSet -> Bool
overlaps set0 set1 = 0 < size (set0 `intersect` set1)

-- Query -----------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
size :: ScopeSet -> Int
size = coerce IntSet.size

-- | TODO 
--
-- @since 1.0.0
null :: ScopeSet -> Bool
null = coerce IntSet.null

-- | TODO 
--
-- @since 1.0.0
member :: ScopeId -> ScopeSet -> Bool
member = coerce IntSet.member

-- Insert ----------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
insert :: ScopeId -> ScopeSet -> ScopeSet
insert = coerce IntSet.insert  

-- Delete ----------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
delete :: ScopeId -> ScopeSet -> ScopeSet
delete = coerce IntSet.delete

-- Folds -----------------------------------------------------------------------

foldr' :: forall a. (ScopeId -> a -> a) -> a -> ScopeSet -> a
foldr' = coerce @((Int -> a -> a) -> _) IntSet.foldr'
