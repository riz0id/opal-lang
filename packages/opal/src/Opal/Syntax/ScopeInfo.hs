{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Syntax.ScopeInfo
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
module Opal.Syntax.ScopeInfo
  ( -- * ScopeInfo
    ScopeInfo (..)
    -- ** Basic Operations
  , empty
  , singleton
  , insert
  , inserts
  , flipScope
  , deletes
  , lookup
  , union
    -- ** Query
  , null
  )
where

import Control.DeepSeq (NFData)

import Control.Lens (Index, Ixed (..), IxValue)

import Data.Default (Default (..))

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.MultiScope (MultiScope (..))
import Opal.Common.MultiScope qualified as MultiScope
import Opal.Common.Scope (Scope (..))
import Opal.Common.ScopeSet (ScopeSet (..))
import Opal.Common.ScopeSet qualified as ScopeSet
import Opal.Common.Phase (Phase)

import Prelude hiding (lookup, null)

-- ScopeInfo -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ScopeInfo = ScopeInfo ScopeSet MultiScope
  deriving (Eq, Generic, Lift, Ord, Show)

-- | @since 1.0.0
type instance Index ScopeInfo = Maybe Phase

-- | @since 1.0.0
type instance IxValue ScopeInfo = ScopeSet

-- | 'ScopeInfo' defaults to 'empty'.
--
-- @since 1.0.0
instance Default ScopeInfo where
  def = empty

-- | @since 1.0.0
instance Ixed ScopeInfo where
  ix ph f info = fmap (\x -> inserts ph x info) (f (lookup ph info))

-- | @since 1.0.0
instance Monoid ScopeInfo where
  mempty = empty

-- | @since 1.0.0
instance Semigroup ScopeInfo where
  (<>) = union

-- | @since 1.0.0
instance NFData ScopeInfo

-- ScopeInfo - Basic Operations ------------------------------------------------

-- | The empty 'ScopeInfo'. This 'ScopeInfo' has no global scopes or
-- phase-specific scopes.
--
-- >>> empty
-- ScopeInfo (fromList []) (fromList [])
--
-- @since 1.0.0
empty :: ScopeInfo
empty = ScopeInfo ScopeSet.empty MultiScope.empty

-- | TODO: docs
--
-- @since 1.0.0
singleton :: Maybe Phase -> ScopeSet -> ScopeInfo
singleton Nothing   sc = ScopeInfo sc MultiScope.empty
singleton (Just ph) sc = ScopeInfo ScopeSet.empty (MultiScope.singleton ph sc)

-- | TODO: docs
--
-- @since 1.0.0
insert :: Maybe Phase -> Scope -> ScopeInfo -> ScopeInfo
insert Nothing   sc (ScopeInfo gscps mscps) =
  let gscps' = ScopeSet.insert sc gscps
      mscps' = MultiScope.delete Nothing sc mscps
   in ScopeInfo gscps' mscps'
insert (Just ph) sc (ScopeInfo gscps mscps)
  | ScopeSet.member sc gscps = ScopeInfo gscps mscps
  | otherwise = ScopeInfo gscps (MultiScope.insert ph sc mscps)

-- | TODO: docs
--
-- @since 1.0.0
inserts :: Maybe Phase -> ScopeSet -> ScopeInfo -> ScopeInfo
inserts Nothing   scps (ScopeInfo gscps mscps)
  | ScopeSet.null scps = ScopeInfo gscps mscps
  | otherwise =
    let gscps' = ScopeSet.union scps gscps
        mscps' = MultiScope.deletes Nothing scps mscps
     in ScopeInfo gscps' mscps'
inserts (Just ph) scps (ScopeInfo gscps mscps)
  | ScopeSet.null scps = ScopeInfo gscps mscps
  | otherwise =
    let scps' :: ScopeSet
        scps' = ScopeSet.intersection scps gscps
     in if ScopeSet.null scps'
          then ScopeInfo gscps mscps
          else ScopeInfo gscps (MultiScope.inserts ph scps' mscps)

-- | TODO: docs
--
-- @since 1.0.0
flipScope :: Phase -> Scope -> ScopeInfo -> ScopeInfo
flipScope ph sc (ScopeInfo gscps mscps) = ScopeInfo gscps (MultiScope.flipScope ph sc mscps)

-- | TODO: docs
--
-- @since 1.0.0
deletes :: Phase -> ScopeSet -> ScopeInfo -> ScopeInfo
deletes ph scps (ScopeInfo gscps mscps) = ScopeInfo gscps (MultiScope.deletes (Just ph) scps mscps)

-- | TODO: docs
--
-- @since 1.0.0
lookup :: Maybe Phase -> ScopeInfo -> ScopeSet
lookup Nothing   (ScopeInfo gscps _)     = gscps
lookup (Just ph) (ScopeInfo gscps mscps) = gscps `ScopeSet.union` MultiScope.lookup' ph mscps

-- | TODO: docs
--
-- @since 1.0.0
union :: ScopeInfo -> ScopeInfo -> ScopeInfo
union (ScopeInfo gscps1 mscps1) (ScopeInfo gscps2 mscps2) =
  let gscps   = ScopeSet.union gscps1 gscps2
      mscps1' = MultiScope.deletes Nothing gscps2 mscps1
      mscps2' = MultiScope.deletes Nothing gscps1 mscps2
   in ScopeInfo gscps (mscps1' <> mscps2')

-- ScopeInfo - Query -----------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
null :: ScopeInfo -> Bool
null (ScopeInfo gscps mscps) = ScopeSet.null gscps && MultiScope.null mscps

