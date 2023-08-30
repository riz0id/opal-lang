{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Binding.BindingStore
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
module Opal.Binding.BindingStore
  ( -- * BindingStore
    BindingStore (..)
    -- ** Construct
  , empty
  , singleton
  , coreBindingStore
    -- ** Insert
  , insert
    -- ** Delete
  , delete
    -- ** Lookup
  , lookup
    -- ** Filter
  , restrictBindings
    -- ** Query
  , null
  , size
  )
where

import Control.DeepSeq (NFData)

import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import GHC.Exts (IsList (..))
import GHC.Generics (Generic)

import Opal.Binding (Binding (..))
import Opal.Common.Symbol (Symbol)
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.ScopeSet qualified as ScopeSet
import Opal.Core (coreFormSymbol)
import Opal.Writer.Class (Display (..))
import Opal.Writer.Doc qualified as Doc

import Prelude hiding (filter, lookup, null)

-- BindingStore ----------------------------------------------------------------

-- | 'BindingStore' is a map from bindings to generated symbols.
--
-- @since 1.0.0
newtype BindingStore = BindingStore
  { getBindingStore :: Map Symbol (Map ScopeSet Symbol) }
  deriving stock   (Generic)
  deriving newtype (Eq, Ord, Show)

-- | 'BindingStore' defaults to 'empty'.
--
-- @since 1.0.0
instance Default BindingStore where
  def = empty

-- | @since 1.0.0
instance Display BindingStore where
  display = Doc.string . show

-- | @since 1.0.0
instance IsList BindingStore where
  type Item BindingStore = (Symbol, Binding)

  fromList = listToBindingStore

  toList = bindingStoreToList

-- | @since 1.0.0
instance NFData BindingStore

-- BindingStore - Construct ----------------------------------------------------

-- | Construct an empty 'BindingStore'.
--
-- @since 1.0.0
empty :: BindingStore
empty = BindingStore Map.empty

-- | Construct a 'BindingStore' with a single binding.
--
-- @since 1.0.0
singleton :: Symbol -> Binding -> BindingStore
singleton s (Binding scps b) = BindingStore (Map.singleton s (Map.singleton scps b))

-- | TODO: docs
--
-- @since 1.0.0
bindingStoreToList :: BindingStore -> [(Symbol, Binding)]
bindingStoreToList = Map.foldrWithKey run [] . getBindingStore
  where
    run :: Symbol -> Map ScopeSet Symbol -> [(Symbol, Binding)] -> [(Symbol, Binding)]
    run s = mappend . Map.foldrWithKey (\scps b xs -> (s, Binding scps b) : xs) []

-- | Construct a 'BindingStore' from a list of identifier's symbols and the
-- binding for that symbol.
--
-- @since 1.0.0
listToBindingStore :: [(Symbol, Binding)] -> BindingStore
listToBindingStore = foldr (uncurry insert) empty

-- | TODO: docs
--
-- @since 1.0.0
coreBindingStore :: BindingStore
coreBindingStore = listToBindingStore coreBindings
  where
    coreBindings :: [(Symbol, Binding)]
    coreBindings = [
        let s = coreFormSymbol x
            b = Binding (ScopeSet.singleton def) s
         in (s, b) | x <- [minBound .. maxBound]
      ]

-- BindingStore - Insert -------------------------------------------------------

-- | \(O(\log n)\). Associate a 'Binding' with a 'Symbol' in the given
-- 'BindingStore'. If the 'Binding' is already present in the 'BindingStore',
-- then the associated 'Symbol is replaced with the new 'Symbol'.
--
-- @since 1.0.0
insert :: Symbol -> Binding -> BindingStore -> BindingStore
insert s (Binding scps b) = coerce (Map.alter (Just . update) s)
  where
    update :: Maybe (Map ScopeSet Symbol) -> Map ScopeSet Symbol
    update Nothing   = Map.singleton scps b
    update (Just bs) = Map.insert scps b bs

-- BindingStore - Delete -------------------------------------------------------

-- | \(O(\log n)\). Delete a 'Binding' from the given 'BindingStore'. If the
-- given is not present in the 'BindingStore', then the 'BindingStore' is
-- unchanged.
--
-- @since 1.0.0
delete :: Symbol -> BindingStore -> BindingStore
delete = coerce @(_ -> Map _ (Map _ Symbol) -> _) Map.delete

-- BindingStore - Lookup -------------------------------------------------------

-- | \(O(\log n)\). Lookup the 'Symbol' associated with a given 'Binding' in
-- the 'BindingStore'. If no 'Binding' is present in the 'BindingStore', then
-- 'Nothing' is returned.
--
-- @since 1.0.0
lookup :: Symbol -> ScopeSet -> BindingStore -> Maybe Symbol
lookup s scps (BindingStore bs) = do
  bs' <- Map.lookup s bs
  Map.lookup scps bs'

-- BindingStore - Filter -------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
restrictBindings :: Symbol -> ScopeSet -> BindingStore -> Set Binding
restrictBindings s ref bs =
  case Map.lookup s (getBindingStore bs) of
    Nothing       -> Set.empty
    Just bindings -> Map.foldrWithKey run Set.empty bindings
  where
    run :: ScopeSet -> Symbol -> Set Binding -> Set Binding
    run scps b
      | isCanidate scps = Set.insert (Binding scps b)
      | otherwise       = id

    isCanidate :: ScopeSet -> Bool
    isCanidate scps = ScopeSet.isSubsetOf scps ref

-- BindingStore - Query --------------------------------------------------------

-- | \(O(1)\). Is the 'BindingStore' empty?
--
-- @since 1.0.0
null :: BindingStore -> Bool
null = coerce @(Map _ (Map _ Symbol) -> _) Map.null

-- | \(O(1)\). Is the 'BindingStore' empty?
--
-- @since 1.0.0
size :: BindingStore -> Int
size = Map.foldrWithKey (\_ -> (+) . Map.size) 0 . getBindingStore