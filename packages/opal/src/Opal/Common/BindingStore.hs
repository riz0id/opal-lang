{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.BindingStore
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
module Opal.Common.BindingStore
  ( -- * BindingStore
    BindingStore (..)
    -- ** Basic Operations
  , emptyBindingStore
  , coreBindingStore
  , insertBindingStore
  , lookupBindingStore
    -- ** Update
  , alterBindingStore
    -- * BindingSet
  , BindingSet (..)
    -- ** Basic Operations
  , emptyBindingSet
  , singletonBindingSet
  , largestSubset
  , canidatesBindingSet
  , insertBindingSet
  , lookupMaxBindingSet
  )
where

import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import GHC.Generics (Generic)

import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.ScopeSet qualified as ScopeSet
import Opal.Common.Symbol (Symbol)
import qualified Debug.Trace as Debug

-- BindingStore ----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype BindingStore = BindingStore
  { getBindingStore :: Map Symbol BindingSet }
  deriving (Eq, Generic, Ord, Show)

-- | 'BindingStore' defaults to 'emptyBindingStore'.
--
-- @since 1.0.0
instance Default BindingStore where
  def = emptyBindingStore

-- BindingStore - Basic Operations ---------------------------------------------

-- | The empty 'BindingStore'.
--
-- @since 1.0.0
emptyBindingStore :: BindingStore
emptyBindingStore = BindingStore Map.empty

-- | TODO: docs
--
-- @since 1.0.0
coreBindingStore :: BindingStore
coreBindingStore =
  emptyBindingStore
    & insertBindingStore "lambda" (ScopeSet.singleton def) "#%core-lambda"
    & insertBindingStore "quote"  (ScopeSet.singleton def) "#%core-quote"

-- | TODO: docs
--
-- @since 1.0.0
insertBindingStore :: Symbol -> ScopeSet -> Symbol -> BindingStore -> BindingStore
insertBindingStore s scps bind = alterBindingStore (Just . update) s
  where
    update :: Maybe BindingSet -> BindingSet
    update Nothing         = singletonBindingSet scps bind
    update (Just bindings) = insertBindingSet scps bind bindings

-- | TODO: docs
--
-- @since 1.0.0
lookupBindingStore :: Symbol -> BindingStore -> Maybe BindingSet
lookupBindingStore = coerce @(_ -> Map _ BindingSet -> _) Map.lookup

-- BindingStore - Update -------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
alterBindingStore :: (Maybe BindingSet -> Maybe BindingSet) -> Symbol -> BindingStore -> BindingStore
alterBindingStore = coerce @((Maybe BindingSet -> _) -> _) Map.alter

-- BindingSet ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype BindingSet = BindingSet
  { getBindingSet :: Map ScopeSet Symbol }
  deriving (Eq, Generic, Ord, Show)

-- | 'BindingSet' defaults to 'emptyBindingSet'.
--
-- @since 1.0.0
instance Default BindingSet where
  def = emptyBindingSet

-- BindingSet - Basic Operations -----------------------------------------------

-- | The empty 'BindingSet'.
--
-- @since 1.0.0
emptyBindingSet :: BindingSet
emptyBindingSet = BindingSet Map.empty

-- | The empty 'BindingSet'.
--
-- @since 1.0.0
singletonBindingSet :: ScopeSet -> Symbol -> BindingSet
singletonBindingSet = coerce @(_ -> Symbol -> _) Map.singleton

-- | TODO: docs
--
-- @since 1.0.0
largestSubset :: ScopeSet -> BindingSet -> Maybe (ScopeSet, Symbol)
largestSubset ref bindings = do
  let !_ = Debug.trace (show ref) ()
  let canidates :: BindingSet
      canidates = canidatesBindingSet ref bindings
   in lookupMaxBindingSet canidates

-- | TODO: docs
--
-- @since 1.0.0
canidatesBindingSet :: ScopeSet -> BindingSet -> BindingSet
canidatesBindingSet ref = filterBindingSet \scps _ -> scps `ScopeSet.isSubsetOf` ref

-- | TODO: docs
--
-- @since 1.0.0
filterBindingSet :: (ScopeSet -> Symbol -> Bool) -> BindingSet -> BindingSet
filterBindingSet = coerce @((_ -> Symbol -> _) -> _) Map.filterWithKey

-- | TODO: docs
--
-- @since 1.0.0
insertBindingSet :: ScopeSet -> Symbol -> BindingSet -> BindingSet
insertBindingSet = coerce @(_ -> Symbol -> _) Map.insert

-- | TODO: docs
--
-- @since 1.0.0
lookupMaxBindingSet :: BindingSet -> Maybe (ScopeSet, Symbol)
lookupMaxBindingSet = coerce @(Map _ Symbol -> _) Map.lookupMax
