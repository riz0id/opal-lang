{-# LANGUAGE MultiParamTypeClasses #-}

module Opal.Expand.Resolve
  ( -- * Monad
    Resolve (Resolve, unResolve),

    -- * Resolver Context
    ResolveCtx (ResolveCtx, symbol, scopes),

    -- * Resolver Errors
    ResolveError (ExnAmbiguous, ExnUnbound),

    -- * Resolvers
    runResolveId,
    runResolve,

    -- * TODO
    resolve,
    canidates,
  )
where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, asks, local)

import Data.Data (Data)
import Data.Set (Set)
import Data.Set qualified as Set

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

import Opal.Expand.Syntax (StxIdt)
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.BindStore (BindStore)
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.Binding (Binding)
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.MultiScopeSet (Phase)
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Syntax.ScopeSet (ScopeSet)

-- Monad -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype Resolve a = Resolve
  {unResolve :: ResolveCtx -> (# ResolveError| a #)}

-- | @since 1.0.0
instance Functor Resolve where
  fmap f (Resolve k) =
    Resolve \ctx -> case k ctx of
      (# e | #) -> (# e | #)
      (# | x #) -> (# | f x #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative Resolve where
  pure x = Resolve \_ -> (# | x #)
  {-# INLINE pure #-}

  Resolve f <*> Resolve g =
    Resolve \ctx -> case f ctx of
      (# e | #) -> (# e | #)
      (# | k #) -> case g ctx of
        (# e | #) -> (# e | #)
        (# | x #) -> (# | k x #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad Resolve where
  Resolve k >>= f =
    Resolve \ctx -> case k ctx of
      (# e | #) -> (# e | #)
      (# | x #) -> unResolve (f x) ctx
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadError ResolveError Resolve where
  throwError e = Resolve \_ -> (# e | #)
  {-# INLINE throwError #-}

  catchError (Resolve k) f =
    Resolve \ctx -> case k ctx of
      (# e | #) -> unResolve (f e) ctx
      (# | x #) -> (# | x #)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadReader ResolveCtx Resolve where
  ask = Resolve \ctx -> (# | ctx #)
  {-# INLINE ask #-}

  local f (Resolve k) = Resolve \ctx -> k (f ctx)
  {-# INLINE local #-}

-- Resolver Context ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ResolveCtx = ResolveCtx
  { symbol :: {-# UNPACK #-} !Symbol
  , scopes :: ScopeSet
  }
  deriving (Data, Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
fromStxIdt :: Phase -> StxIdt -> ResolveCtx
fromStxIdt ph idt =
  let scps :: ScopeSet
      scps = MultiScopeSet.index ph idt.context.multiscope
   in ResolveCtx idt.symbol scps

-- Resolver Errors -------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ResolveError
  = ExnAmbiguous (Set Binding)
  | ExnUnbound {-# UNPACK #-} !Name ScopeSet
  deriving (Data, Eq, Ord, Show)

-- Resolvers -------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runResolveId :: Phase -> StxIdt -> BindStore -> Either ResolveError Binding
runResolveId ph idt store = runResolve (fromStxIdt ph idt) (resolve store)
{-# INLINE runResolveId #-}

-- | TODO
--
-- @since 1.0.0
runResolve :: ResolveCtx -> Resolve a -> Either ResolveError a
runResolve ctx mx =
  case unResolve mx ctx of
    (# e | #) -> Left e
    (# | x #) -> Right x
{-# INLINE runResolve #-}

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
resolve :: BindStore -> Resolve Binding
resolve store = do
  binds <- canidates store
  case Binding.maximum binds of
    Nothing -> do
      name <- asks (Symbol.toName . symbol)
      scps <- asks scopes
      throwError (ExnUnbound name scps)
    Just best ->
      -- Filter any bindings from the set of canidates @binds@ that are not a
      -- subset of the resolved binding @best@. If the set @ambiguous@ is
      -- nonempty, then there exists some bindings that have a disjoint set of
      -- scopes.
      let ambiguous :: Set Binding
          ambiguous = Set.filter (not . (`Binding.subset` best.scopes)) binds
       in if Set.null ambiguous
            then pure best
            else throwError (ExnAmbiguous ambiguous)

-- | TODO
--
-- @since 1.0.0
canidates :: BindStore -> Resolve (Set Binding)
canidates store = do
  scps <- asks scopes
  name <- asks (Symbol.toName . symbol)
  let bindings :: Set Binding
      bindings = BindStore.index name store
   in pure (Set.filter (`Binding.subset` scps) bindings)