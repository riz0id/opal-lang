{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Expander.DefinitionContext
-- Copyright   :  (c) Jacob Leach, 2026
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Bookkeeping for an internal-definition context. Mirrors Racket's
-- @internal-definition-context@ record: each such context owns
--
-- * a mutable accumulator for the use-site scopes that nested macro
--   calls have produced — read at @define@\/@define-syntax@ binders to
--   prune the use-site scopes off them; and
--
-- * a single immutable inside-edge scope, attached to every macro
--   output that lands in the context so that subsequent definitions
--   look up against it.
--
-- The plain @Reader@\/@State@ idioms don't suffice here:
--
-- * @State@ alone would have sibling macro expansions clobber each
--   other's accumulators.
--
-- * @Reader@ alone can't propagate the use-site additions back up to
--   the @define@ pruning step.
--
-- A per-context mutable cell (a @MutVar@) carried through the
-- @Reader@ is the standard fix, and is what Racket uses (@box@).
--
-- @since 1.0.0
module Opal.Expander.DefinitionContext
  ( -- * DefinitionContext
    DefinitionContext (..)
    -- ** Basic Operations
  , newDefinitionContext
  , readUseSiteScopes
  , insertUseSiteScope
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Primitive (PrimMonad, PrimState)

import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, modifyMutVar')

import GHC.Exts (RealWorld)

import Opal.Common.Scope (MonadScope (..), Scope)
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.ScopeSet qualified as ScopeSet

-- DefinitionContext -----------------------------------------------------------

-- | A definition context's per-expansion bookkeeping.
--
-- @since 1.0.0
data DefinitionContext = DefinitionContext
  { defctx_use_site_scopes    :: {-# UNPACK #-} !(MutVar RealWorld ScopeSet)
    -- ^ Accumulator for use-site scopes produced by every macro call
    -- inside this context. Mutable so nested macros share the same
    -- box.
  , defctx_inside_edge_scope  :: {-# UNPACK #-} !Scope
    -- ^ A single fresh scope, allocated when the context is entered.
    -- Attached to every macro output landing in this context — and
    -- to the binders that get registered against this context.
  , defctx_outside_edge_scope :: {-# UNPACK #-} !Scope
    -- ^ A single fresh scope, allocated when the context is entered.
    -- Attached to every input form of this context before pre-pass
    -- processing, so that macros expanded inside see it.
  }

-- DefinitionContext - Basic Operations ----------------------------------------

-- | Allocate a fresh 'DefinitionContext': a new empty use-site scope
-- accumulator and a new inside-edge scope.
--
-- @since 1.0.0
newDefinitionContext ::
  (MonadIO m, MonadScope m, PrimMonad m, PrimState m ~ RealWorld) =>
  m DefinitionContext
newDefinitionContext = do
  uses    <- newMutVar ScopeSet.empty
  inside  <- newScope
  outside <- newScope
  pure (DefinitionContext uses inside outside)

-- | Read the current accumulated use-site scopes from a context.
--
-- @since 1.0.0
readUseSiteScopes :: MonadIO m => DefinitionContext -> m ScopeSet
readUseSiteScopes = liftIO . readMutVar . defctx_use_site_scopes

-- | Push a new use-site scope into a context's accumulator.
--
-- @since 1.0.0
insertUseSiteScope :: MonadIO m => Scope -> DefinitionContext -> m ()
insertUseSiteScope sc dc =
  liftIO (modifyMutVar' (defctx_use_site_scopes dc) (ScopeSet.insert sc))
