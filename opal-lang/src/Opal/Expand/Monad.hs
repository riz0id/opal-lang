{-# LANGUAGE MultiParamTypeClasses #-}

module Opal.Expand.Monad
  ( -- * TODO
    ExpError (..),

    -- * TODO
    evalExpand,
    runExpand,
    Expand (Expand, unExpand),
  )
where

import Control.Applicative (liftA2)

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.State.Strict (MonadState, get, put, state)

import Data.Data (Data)
import Data.Kind (Type)

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.GenSym (MonadGenSym, newGenSym, newGenSymWith)

import Opal.Expand.Context (Context, ctxCoreScope)
import Opal.Expand.Context qualified as Context
import Opal.Expand.Resolve.Class
  ( MonadResolve,
    newBind,
    newScopeId,
    resolve,
    resolveBind,
  )
import Opal.Expand.Resolve.Monad (ResolveM, runResolveM)
import Opal.Expand.Syntax (Syntax)
import Opal.Expand.Syntax.BindTable (BindTable)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

newtype ExpError
  = LitStxAppError [Syntax]
  deriving (Data, Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalExpand :: Expand a -> Either ExpError a
evalExpand ex = snd (runExpand Context.newExpanderContext ex)

-- | TODO
--
-- @since 1.0.0
runExpand :: Context -> Expand a -> (BindTable, Either ExpError a)
runExpand ctx (Expand k) = runResolveM mempty (succ $ ctxCoreScope ctx) (k ctx)

-- | TODO
--
-- @since 1.0.0
newtype Expand (a :: Type) :: Type where
  Expand :: {unExpand :: Context -> ResolveM (Either ExpError a)} -> Expand a

-- | @since 1.0.0
instance Functor Expand where
  fmap f (Expand k) = Expand (fmap (fmap f) . k)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative Expand where
  pure x = Expand \_ -> pure (Right x)
  {-# INLINE pure #-}

  Expand f <*> Expand g = Expand \ctx -> liftA2 (<*>) (f ctx) (g ctx)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad Expand where
  Expand k >>= f =
    Expand \ctx ->
      k ctx >>= \case
        Left exn -> pure (Left exn)
        Right rx -> unExpand (f rx) ctx
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadError ExpError Expand where
  throwError e = Expand \_ -> pure (Left e)
  {-# INLINE throwError #-}

  catchError (Expand k) f =
    Expand \ctx ->
      k ctx >>= \case
        Left exn -> unExpand (f exn) ctx
        Right x -> pure (Right x)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadReader Context Expand where
  ask = Expand (pure . Right)
  {-# INLINE ask #-}

  local f (Expand k) = Expand (k . f)
  {-# INLINE local #-}

-- | @since 1.0.0
instance MonadState BindTable Expand where
  get = Expand \_ -> fmap Right get
  {-# INLINE get #-}

  put st = Expand \_ -> fmap Right (put st)
  {-# INLINE put #-}

  state f = Expand \_ -> fmap Right (state f)
  {-# INLINE state #-}

-- | @since 1.0.0
instance MonadGenSym Expand where
  newGenSym = Expand \_ -> fmap Right newGenSym
  {-# INLINE newGenSym #-}

  newGenSymWith idt = Expand \_ -> fmap Right (newGenSymWith idt)
  {-# INLINE newGenSymWith #-}

-- | @since 1.0.0
instance MonadResolve Expand where
  newScopeId = Expand \_ -> fmap Right newScopeId
  {-# INLINE newScopeId #-}

  newBind phase idt = Expand \_ -> fmap Right (newBind phase idt)
  {-# INLINE newBind #-}

  resolveBind phase idt = Expand \_ -> fmap Right (resolveBind phase idt)
  {-# INLINE resolveBind #-}

  resolve phase idt = Expand \_ -> fmap Right (resolve phase idt)
  {-# INLINE resolve #-}