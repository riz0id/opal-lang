{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Resolve
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
module Opal.Resolve
  ( -- * MonadResolve
    MonadResolve (..)
    -- * ResolveError
  , ResolveError (..)
    -- * Resolve
  , resolveId
  )
where

import Control.Lens ((^.))

import Control.Monad.Except (MonadError(..))

import Data.Set qualified as Set

import Opal.Binding (Binding (..), bindingScopes, bindingSymbol)
import Opal.Binding.BindingStore (BindingStore, restrictBindings)
import Opal.Common.Phase (Phase)
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.ScopeSet qualified as ScopeSet
import Opal.Common.Symbol (Symbol)
import Opal.Error (ErrorAmbiguous (..), ErrorNotInScope (..))
import Opal.Syntax (Identifier, idtScopes, idtSymbol)
import Opal.Syntax.ScopeInfo qualified as ScopeInfo

import Prelude hiding (id)

-- MonadResolve ----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
class MonadResolve m where
  -- | TODO: docs
  --
  -- @since 1.0.0
  resolve :: Phase -> Identifier -> m Symbol

-- ResolveError ----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ResolveError
  = ResolveAmbiguous {-# UNPACK #-} !ErrorAmbiguous
    -- ^ TODO: docs
  | ResolveNotInScope {-# UNPACK #-} !ErrorNotInScope
    -- ^ TODO: docs
  deriving stock (Eq, Ord, Show)

-- Binding Resolution ----------------------------------------------------------

-- | Two scope sets are /incomparable/ under the subset order when neither is
-- a subset of the other. Per Flatt's scope-sets model — and Racket's
-- @resolve@ in @racket/src/expander/syntax/scope.rkt@ — two candidate
-- bindings are ambiguous precisely when their scope sets are incomparable in
-- this sense, not merely when they fail to intersect.
--
-- @since 1.0.0
incomparable :: ScopeSet -> ScopeSet -> Bool
incomparable a b =
  not (a `ScopeSet.isSubsetOf` b) && not (b `ScopeSet.isSubsetOf` a)

-- | Accumulator for 'resolveId', mirroring Racket's @for*/fold@ over
-- @[best-scopes best-binding]@:
--
-- * 'Best' 'Nothing' is the initial state, before any candidate has been
--   seen.
--
-- * 'Best' ('Just' b) is the current "best so far" candidate: every
--   candidate seen so far has scope set that is a subset of @b@'s.
--
-- * 'Ambiguous' bs is a non-empty list of pairwise-incomparable candidates
--   that a later candidate may yet dominate. Only finalized as an ambiguity
--   error if no later candidate is a superset of every element of @bs@.
--
-- @since 1.0.0
data Acc
  = Best !(Maybe Binding)
  | Ambiguous ![Binding]

-- | TODO: docs
--
-- @since 1.0.0
-- TODO: rename the local @canidates@ -> @candidates@ typo at some point.
resolveId :: Phase -> Identifier -> BindingStore -> Either ResolveError Symbol
resolveId ph id store = do
  let s         = id ^. idtSymbol
  let scps      = ScopeInfo.lookup (Just ph) (id ^. idtScopes)
  let canidates = restrictBindings s scps store
  case Set.foldl' step (Best Nothing) canidates of
    Best Nothing  -> throwError (ResolveNotInScope (ErrorNotInScope id))
    Best (Just b) -> pure (b ^. bindingSymbol)
    Ambiguous bs  -> throwError (ResolveAmbiguous (ErrorAmbiguous id (reverse bs)))
  where
    step :: Acc -> Binding -> Acc
    step (Best Nothing)     cand = Best (Just cand)
    step (Best (Just best)) cand
      | candScps `ScopeSet.isSubsetOf` bestScps = Best (Just best)
      | bestScps `ScopeSet.isSubsetOf` candScps = Best (Just cand)
      | incomparable bestScps candScps          = Ambiguous [cand, best]
      | otherwise                               = Best (Just best)
      where
        bestScps = best ^. bindingScopes
        candScps = cand ^. bindingScopes
    step (Ambiguous ambs)   cand
      | all (\b -> (b ^. bindingScopes) `ScopeSet.isSubsetOf` candScps) ambs
                  = Best (Just cand)
      | otherwise = Ambiguous (cand : ambs)
      where
        candScps = cand ^. bindingScopes
