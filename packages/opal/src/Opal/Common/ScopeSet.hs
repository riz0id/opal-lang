{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.ScopeSet
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
module Opal.Common.ScopeSet
  ( -- * ScopeSet
    ScopeSet (..)
    -- ** Basic Operations
  , empty
  , singleton
  , insert
  , delete
    -- ** Query
  , member
  , null
  , size
    -- ** Set Operations
  , isSubsetOf
  , difference
  , union
  , intersection
  )
where

import Control.DeepSeq (NFData)

import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Set (Set)
import Data.Set qualified as Set

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.Scope (Scope (..))

import Prelude hiding (null)

-- ScopeSet --------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype ScopeSet = ScopeSet (Set Scope)
  deriving newtype (Eq, Ord, Show)
  deriving (Generic, Lift)

-- | 'ScopeSet' defaults to 'empty'.
--
-- @since 1.0.0
instance Default ScopeSet where
  def = empty

-- | @since 1.0.0
instance Monoid ScopeSet where
  mempty = empty

-- | @since 1.0.0
instance Semigroup ScopeSet where
  (<>) = union

-- | @since 1.0.0
instance NFData ScopeSet

-- ScopeSet - Basic Operations -------------------------------------------------

-- | The empty set of scopes.
--
-- >>> empty
-- fromList []
--
-- @since 1.0.0
empty :: ScopeSet
empty = coerce Set.empty

-- | Construct a singleton 'ScopeSet' from the given 'Scope'.
--
-- @since 1.0.0
singleton :: Scope -> ScopeSet
singleton = coerce Set.singleton

-- | TODO: docs
--
-- @since 1.0.0
insert :: Scope -> ScopeSet -> ScopeSet
insert = coerce Set.insert

-- | TODO: docs
--
-- @since 1.0.0
delete :: Scope -> ScopeSet -> ScopeSet
delete = coerce Set.delete

-- ScopeSet - Query ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
member :: Scope -> ScopeSet -> Bool
member = coerce Set.member

-- | Is the given 'ScopeSet' empty?
--
-- @since 1.0.0
null :: ScopeSet -> Bool
null = coerce Set.null

-- | Obtain the size of the given 'ScopeSet'.
--
-- @since 1.0.0
size :: ScopeSet -> Int
size = coerce Set.size

-- ScopeSet - Set Operations ---------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
isSubsetOf :: ScopeSet -> ScopeSet -> Bool
isSubsetOf = coerce isSubsetOf

infixl 9 `difference`

-- | TODO: docs
--
-- @since 1.0.0
difference :: ScopeSet -> ScopeSet -> ScopeSet
difference = coerce Set.difference

-- | TODO: docs
--
-- @since 1.0.0
union :: ScopeSet -> ScopeSet -> ScopeSet
union = coerce Set.union

-- | TODO: docs
--
-- @since 1.0.0
intersection :: ScopeSet -> ScopeSet -> ScopeSet
intersection = coerce Set.intersection
