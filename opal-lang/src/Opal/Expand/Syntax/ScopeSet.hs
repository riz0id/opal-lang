{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Opal.Expand.Syntax.ScopeSet
  ( -- * ScopeId
    ScopeId (ScopeId),

    -- * ScopeSet
    ScopeSet (ScopeSet),
    union,
    difference,
    intersect,
    flips,

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
    foldr',
    unions,
    intersects,
    maximum,
  )
where

import Control.Monad.ST (runST)

import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable (for_)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Primitive.MutVar (modifyMutVar', newMutVar, readMutVar, writeMutVar)
import Data.Set (Set)
import Data.Set qualified as Set

import GHC.Exts (IsList, Item, fromList, toList)

import Prelude hiding (maximum, null)

-- ScopeId ---------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype ScopeId = ScopeId Int
  deriving (Enum, Eq, Integral, Ord, Num, Real)

-- | @since 1.0.0
instance Show ScopeId where
  show (ScopeId x) = "sc:" ++ show x
  {-# INLINE show #-}

-- ScopeSet --------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype ScopeSet :: Type where
  ScopeSet :: IntSet -> ScopeSet
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance IsList ScopeSet where
  type Item ScopeSet = ScopeId

  fromList = coerce IntSet.fromList
  {-# INLINE fromList #-}

  toList = coerce IntSet.toList
  {-# INLINE toList #-}

-- | @since 1.0.0
instance Semigroup ScopeSet where
  xs <> ys = coerce IntSet.union xs ys
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance Monoid ScopeSet where
  mempty = ScopeSet IntSet.empty
  {-# INLINE mempty #-}

-- | @since 1.0.0
instance Show ScopeSet where
  show scopes
    | null scopes = "#set{}"
    | otherwise = "#set{" ++ unwords (foldr' ((:) . show) [] scopes) ++ "}"
  {-# INLINE show #-}

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
flips :: ScopeId -> ScopeSet -> ScopeSet
flips sid set
  | member sid set = delete sid set
  | otherwise = insert sid set
{-# INLINE flips #-}

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
subset = coerce IntSet.isSubsetOf

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

-- | TODO
--
-- @since 1.0.0
foldr' :: forall a. (ScopeId -> a -> a) -> a -> ScopeSet -> a
foldr' = coerce @((Int -> a -> a) -> _) IntSet.foldr'

-- | TODO
--
-- @since 1.0.0
unions :: Foldable t => t ScopeSet -> ScopeSet 
unions = foldr union empty 

-- | TODO
--
-- @since 1.0.0
intersects :: Foldable t => t ScopeSet -> ScopeSet 
intersects = foldr intersect empty 

-- | TODO
--
-- @since 1.0.0
maximum :: [ScopeSet] -> Set ScopeSet
maximum sets = runST do
  mutSize <- newMutVar 0
  mutMaxs <- newMutVar Set.empty

  for_ sets \set -> do
    len <- readMutVar mutSize
    case compare len (size set) of
      GT -> pure ()
      EQ -> modifyMutVar' mutMaxs (Set.insert set)
      LT -> do
        writeMutVar mutSize (size set)
        writeMutVar mutMaxs (Set.singleton set)

  readMutVar mutMaxs