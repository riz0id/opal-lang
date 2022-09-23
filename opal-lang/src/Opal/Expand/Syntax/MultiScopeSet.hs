{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opal.Expand.Syntax.MultiScopeSet
  ( -- * PhaseId
    Phase (Phase),

    -- * MultiScopeSet
    MultiScopeSet (MultiScopeSet),
    flipscope,

    -- * Construction
    empty,
    singleton,

    -- * Index
    member,

    -- * Index
    index,
    lookup,

    -- * Insert
    insert,

    -- * Delete
    delete,
    prune,
  )
where

import Data.Data (Data)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Maybe (fromMaybe)

import Prelude hiding (lookup, null)

--------------------------------------------------------------------------------

import Opal.Expand.Syntax.ScopeSet (ScopeId, ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet

-- MultiScopeSet ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype Phase :: Type where
  Phase :: Int -> Phase
  deriving (Data, Enum, Eq, Ord, Show)

-- MultiScopeSet ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype MultiScopeSet :: Type where
  MultiScopeSet :: IntMap ScopeSet -> MultiScopeSet
  deriving (Data, Eq, Ord, Show)

-- | TODO 
--
-- @since 1.0.0
flipscope :: Phase -> ScopeId -> MultiScopeSet -> MultiScopeSet 
flipscope ph sc scopes
  | member ph sc scopes = delete ph sc scopes
  | otherwise = insert ph sc scopes
{-# INLINE flipscope #-}

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
empty :: MultiScopeSet
empty = MultiScopeSet IntMap.empty

-- | TODO
--
-- @since 1.0.0
singleton :: Phase -> ScopeId -> MultiScopeSet
singleton (Phase ph) sc = MultiScopeSet (IntMap.singleton ph (ScopeSet.singleton sc))

-- Query -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
member :: Phase -> ScopeId -> MultiScopeSet -> Bool
member (Phase ph) sc (MultiScopeSet scopes) =
  case IntMap.lookup ph scopes of
    Nothing -> False
    Just set -> ScopeSet.member sc set

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
index :: Phase -> MultiScopeSet -> ScopeSet
index ph scopes = fromMaybe ScopeSet.empty (lookup ph scopes)

-- | TODO
--
-- @since 1.0.0
lookup :: Phase -> MultiScopeSet -> Maybe ScopeSet
lookup (Phase ph) (MultiScopeSet scopes) = IntMap.lookup ph scopes

-- Insert ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
insert :: Phase -> ScopeId -> MultiScopeSet -> MultiScopeSet
insert (Phase ph) sc (MultiScopeSet scopes) =
  MultiScopeSet (IntMap.alter alter ph scopes)
  where
    alter :: Maybe ScopeSet -> Maybe ScopeSet
    alter Nothing = Just (ScopeSet.singleton sc)
    alter (Just set) = Just (ScopeSet.insert sc set)

-- Delete ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
delete :: Phase -> ScopeId -> MultiScopeSet -> MultiScopeSet
delete (Phase ph) sc (MultiScopeSet scopes) = 
  MultiScopeSet (IntMap.adjust (ScopeSet.delete sc) ph scopes)

-- | TODO
--
-- @since 1.0.0
prune :: Phase -> ScopeSet -> MultiScopeSet -> MultiScopeSet
prune (Phase ph) set (MultiScopeSet scopes) = 
  MultiScopeSet (IntMap.adjust (`ScopeSet.difference` set) ph scopes)