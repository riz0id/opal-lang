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

import Control.Monad (foldM)
import Control.Monad.Except (MonadError(..))

import Data.Set qualified as Set

import Opal.Binding (Binding (..), bindingSymbol)
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

-- | TODO: docs
--
-- @since 1.0.0
ambiguous :: ScopeSet -> ScopeSet -> Bool
ambiguous a b = not (a `ScopeSet.intersects` b)

-- | TODO: docs
--
-- @since 1.0.0
resolveId :: Phase -> Identifier -> BindingStore -> Either ResolveError Symbol
resolveId ph id store = do
  let s         = id ^. idtSymbol
  let scps      = ScopeInfo.lookup (Just ph) (id ^. idtScopes)
  let canidates = restrictBindings s scps store
  case Set.maxView canidates of
    Nothing          -> throwError (ResolveNotInScope (ErrorNotInScope id))
    Just (elt, rest) -> do
      result <- foldM run elt rest
      pure (result ^. bindingSymbol)
  where
    run :: Binding -> Binding -> Either ResolveError Binding
    run b2@(Binding scps2 _) b1@(Binding scps1 _)
      | scps1 `ambiguous` scps2 = throwError (ResolveAmbiguous (ErrorAmbiguous id [b1, b2]))
      | otherwise               = pure (bestBinding b1 b2)

    bestBinding :: Binding -> Binding -> Binding
    bestBinding b1@(Binding scps1 _) b2@(Binding scps2 _)
      | scps1 `ScopeSet.isSubsetOf` scps2 = b2
      | otherwise                         = b1
