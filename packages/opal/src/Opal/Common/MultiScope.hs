{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.MultiScope
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Opal.Common.MultiScope
  ( -- * MultiScope
    MultiScope (..)
    -- ** Basic Operations
  , empty
  , singleton
  , insert
  , inserts
  , flipScope
  , delete
  , deletes
  , lookup
  , lookup'
  , map
  , union
  , intersection
    -- ** Update
  , alter
    -- ** Query
  , phases
  , member
  , null
  , size
  )
where

import Control.DeepSeq (NFData)

import Control.Lens (Index, Ixed (..), IxValue)

import Control.Monad (guard)

import GHC.Generics (Generic)

import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)

import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.Phase (Phase (..))
import Opal.Common.Scope (Scope (..))
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.ScopeSet qualified as ScopeSet
import Opal.Writer.Class (Display (..))
import Opal.Writer.Doc qualified as Doc

import Prelude hiding (lookup, map, null)

-- MultiScope ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype MultiScope = MultiScope (Map Phase ScopeSet)
  deriving newtype (Eq, Ord, Show)
  deriving (Generic, Lift)

-- | 'MultiScope' is indexed by 'Phase'.
--
-- @since 1.0.0
type instance Index MultiScope = Phase

-- | 'MultiScope' indexes 'ScopeSet'.
--
-- @since 1.0.0
type instance IxValue MultiScope = ScopeSet

-- | 'MultiScope' defaults to 'empty'.
--
-- @since 1.0.0
instance Default MultiScope where
  def = empty

-- | @since 1.0.0
instance Display MultiScope where
  display = Doc.string . show

-- | @since 1.0.0
instance Ixed MultiScope where
  ix ph f mscp = case lookup ph mscp of
    Nothing   -> pure mscp
    Just scps -> fmap (\scps' -> inserts ph scps' mscp) (f scps)

-- | @since 1.0.0
instance Monoid MultiScope where
  mempty = empty

-- | @since 1.0.0
instance Semigroup MultiScope where
  (<>) = union

-- | @since 1.0.0
instance NFData MultiScope

-- MultiScope - Basic Operations -----------------------------------------------

-- | The empty 'MultiScope'.
--
-- >>> empty
-- fromList []
--
-- @since 1.0.0
empty :: MultiScope
empty = coerce @(Map _ ScopeSet) Map.empty

-- | TODO: docs
--
-- @since 1.0.0
singleton :: Phase -> ScopeSet -> MultiScope
singleton = coerce @(_ -> ScopeSet -> _) Map.singleton

-- | TODO: docs
--
-- @since 1.0.0
insert :: Phase -> Scope -> MultiScope -> MultiScope
insert ph sc = alter ph \case
  Nothing   -> Just (ScopeSet.singleton sc)
  Just scps -> Just (ScopeSet.insert sc scps)

-- | TODO: docs
--
-- @since 1.0.0
inserts :: Phase -> ScopeSet -> MultiScope -> MultiScope
inserts ph scps = alter ph \case
  Nothing    -> Just scps
  Just scps' -> Just (ScopeSet.union scps scps')

-- | TODO: docs
--
-- @since 1.0.0
flipScope :: Phase -> Scope -> MultiScope -> MultiScope
flipScope ph sc = alter ph \case
  Nothing   -> Just (ScopeSet.singleton sc)
  Just scps -> Just (ScopeSet.flipScope sc scps)

-- | TODO: docs
--
-- @since 1.0.0
delete :: Maybe Phase -> Scope -> MultiScope -> MultiScope
delete mph sc mscp = case mph of
  Nothing -> foldr (\ph -> delete (Just ph) sc) empty (phases mscp)
  Just ph -> alter ph (>>= update) mscp
  where
    update :: ScopeSet -> Maybe ScopeSet
    update scps = do
      let result = ScopeSet.delete sc scps
      guard (not (ScopeSet.null result))
      pure result

-- | TODO: docs
--
-- @since 1.0.0
deletes :: Maybe Phase -> ScopeSet -> MultiScope -> MultiScope
deletes mph scps mscp
  | ScopeSet.null scps = mscp
  | otherwise          = case mph of
    Nothing -> foldr (\ph -> deletes (Just ph) scps) empty (phases mscp)
    Just ph -> alter ph (>>= update) mscp
  where
    update :: ScopeSet -> Maybe ScopeSet
    update scps' = do
      let result = ScopeSet.difference scps' scps
      guard (not (ScopeSet.null result))
      pure result

-- | TODO: docs
--
-- @since 1.0.0
lookup :: Phase -> MultiScope -> Maybe ScopeSet
lookup = coerce @(_ -> Map _ ScopeSet -> _) Map.lookup

-- | TODO: docs
--
-- @since 1.0.0
lookup' :: Phase -> MultiScope -> ScopeSet
lookup' ph = fromMaybe ScopeSet.empty . lookup ph

-- | TODO: docs
--
-- @since 1.0.0
map :: (ScopeSet -> ScopeSet) -> MultiScope -> MultiScope
map = coerce @((_ -> ScopeSet) -> Map _ ScopeSet -> _) Map.map

-- | TODO: docs
--
-- @since 1.0.0
union :: MultiScope -> MultiScope -> MultiScope
union = coerce @((ScopeSet -> _) -> _) Map.unionWith ScopeSet.union

-- | TODO: docs
--
-- @since 1.0.0
intersection :: MultiScope -> MultiScope -> MultiScope
intersection = coerce @((ScopeSet -> ScopeSet -> ScopeSet) -> _) Map.intersectionWith ScopeSet.intersection

-- MultiScope - Update ---------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
alter :: Phase -> (Maybe ScopeSet -> Maybe ScopeSet) -> MultiScope -> MultiScope
alter ph f = coerce @((Maybe ScopeSet -> _) -> _) Map.alter f ph

-- MultiScope - Query ----------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
phases :: MultiScope -> Set Phase
phases = coerce @(Map _ ScopeSet -> _) Map.keysSet

-- | TODO: docs
--
-- @since 1.0.0
member :: Phase -> Scope -> MultiScope -> Bool
member ph sc mscp =
  case lookup ph mscp of
    Nothing   -> False
    Just scps -> ScopeSet.member sc scps

-- | TODO: docs
--
-- @since 1.0.0
null :: MultiScope -> Bool
null = coerce @(Map _ ScopeSet -> _) Map.null

-- | TODO: docs
--
-- @since 1.0.0
size :: MultiScope -> Int
size = coerce @(Map _ ScopeSet -> _) Map.size
