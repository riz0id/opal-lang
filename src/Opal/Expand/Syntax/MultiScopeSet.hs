{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Opal.Expand.Syntax.MultiScopeSet
  ( -- * PhaseId
    Phase (Phase),

    -- * MultiScopeSet
    MultiScopeSet (MultiScopeSet),
    flipscope,

    -- * Construction
    empty,
    singleton,
    fromList,

    -- * Query
    member,

    -- * Index
    index,
    lookup,

    -- * Insert
    insert,
    inserts,

    -- * Delete
    delete,
    prune,

    -- * Update
    adjust,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Data.Data (Data)
import Data.IntMap.Internal (IntMap (..))
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Maybe (fromMaybe)

import Language.Haskell.TH.Syntax (Lift)

import Prelude hiding (lookup, null)

--------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Opal.Expand.Syntax.ScopeSet (ScopeId, ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import qualified Data.List as List

-- MultiScopeSet ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype Phase :: Type where
  Phase :: {getPhase :: Int} -> Phase
  deriving (Data, Enum, Eq, Ord, Lift)

-- | @since 1.0.0
instance Show Phase where 
  show (Phase n) = "#phase:" ++ show n
  {-# INLINE show #-}

-- MultiScopeSet ---------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype MultiScopeSet :: Type where
  MultiScopeSet :: IntMap ScopeSet -> MultiScopeSet
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance Show MultiScopeSet where 
  show (MultiScopeSet kxs) =  
    "#multiscope{" ++ List.intercalate ", " (IntMap.foldrWithKey' cons [] kxs) ++ "}" 
    where 
      cons :: Int -> ScopeSet -> [String] -> [String]
      cons ph scopes rest = (shows (Phase ph) " -> " ++ show scopes) : rest
  {-# INLINE show #-}

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
singleton :: Phase -> ScopeSet -> MultiScopeSet
singleton = coerce @(_ -> ScopeSet -> _) IntMap.singleton 

-- | TODO
--
-- @since 1.0.0
fromList :: [(Phase, ScopeSet)] -> MultiScopeSet
fromList = MultiScopeSet . IntMap.fromList . map (first getPhase)

-- Query -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
member :: Phase -> ScopeId -> MultiScopeSet -> Bool
member ph sc multiset = maybe False (ScopeSet.member sc) (lookup ph multiset)

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
index :: Phase -> MultiScopeSet -> ScopeSet
index ph multiset = fromMaybe ScopeSet.empty (lookup ph multiset)

-- | TODO
--
-- @since 1.0.0
lookup :: Phase -> MultiScopeSet -> Maybe ScopeSet
lookup = coerce @(_ -> _ -> Maybe ScopeSet) IntMap.lookup

-- Insert ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
insert :: Phase -> ScopeId -> MultiScopeSet -> MultiScopeSet
insert ph sc = adjust ph (ScopeSet.insert sc)

-- | TODO
--
-- @since 1.0.0
inserts :: Phase -> ScopeSet -> MultiScopeSet -> MultiScopeSet
inserts ph scopes multiset = ScopeSet.foldr' (insert ph) multiset scopes

-- Delete ----------------------------------------------------------------------

-- | Removes the scope from a set of scopes at the phase specified, if it is 
-- present. Otherwise, the 'MultiScopeSet' is left unchanged.
--
-- @since 1.0.0
delete :: Phase -> ScopeId -> MultiScopeSet -> MultiScopeSet
delete ph sc = adjust ph (ScopeSet.delete sc) 

-- | TODO
--
-- @since 1.0.0
prune :: Phase -> ScopeSet -> MultiScopeSet -> MultiScopeSet
prune ph scopes = adjust ph (`ScopeSet.difference` scopes)

-- Delete ----------------------------------------------------------------------

-- | Adjusts the set of scopes at a particular phase. 
--
-- @since 1.0.0
adjust :: Phase -> (ScopeSet -> ScopeSet) -> MultiScopeSet -> MultiScopeSet
adjust ph f = coerce @(_ -> _ -> IntMap ScopeSet -> _) IntMap.alter update ph
  where
    update :: Maybe ScopeSet -> Maybe ScopeSet
    update scopes = Just (maybe (f ScopeSet.empty) f scopes)
