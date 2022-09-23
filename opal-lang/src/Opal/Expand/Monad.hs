{-# LANGUAGE MultiParamTypeClasses #-}

module Opal.Expand.Monad
  ( -- * TODO
    ExpError (..),

    -- * TODO
    ExpM (ExpM, unExpM),
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

import Opal.Expand.Context (Context)
import Opal.Expand.Resolve
  ( MonadResolve,
    ResolveM,
    newBind,
    newScopeId,
    resolve,
    resolveBind,
  )
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
newtype ExpM (a :: Type) :: Type where
  ExpM :: {unExpM :: Context -> ResolveM (Either ExpError a)} -> ExpM a

-- | @since 1.0.0
instance Functor ExpM where
  fmap f (ExpM k) = ExpM (fmap (fmap f) . k)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative ExpM where
  pure x = ExpM \_ -> pure (Right x)
  {-# INLINE pure #-}

  ExpM f <*> ExpM g = ExpM \ctx -> liftA2 (<*>) (f ctx) (g ctx)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad ExpM where
  ExpM k >>= f =
    ExpM \ctx ->
      k ctx >>= \case 
        Left exn -> pure (Left exn)
        Right rx -> unExpM (f rx) ctx 
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadError ExpError ExpM where
  throwError e = ExpM \_ -> pure (Left e)
  {-# INLINE throwError #-}

  catchError (ExpM k) f =
    ExpM \ctx ->
      k ctx >>= \case
        Left exn -> unExpM (f exn) ctx
        Right x -> pure (Right x)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadReader Context ExpM where
  ask = ExpM (pure . Right)
  {-# INLINE ask #-}

  local f (ExpM k) = ExpM (k . f)
  {-# INLINE local #-}

-- | @since 1.0.0
instance MonadState BindTable ExpM where
  get = ExpM \_ -> fmap Right get
  {-# INLINE get #-}

  put st = ExpM \_ -> fmap Right (put st)
  {-# INLINE put #-}

  state f = ExpM \_ -> fmap Right (state f)
  {-# INLINE state #-}

-- | @since 1.0.0
instance MonadGenSym ExpM where
  newGenSym = ExpM \_ -> fmap Right newGenSym
  {-# INLINE newGenSym #-}

  newGenSymWith idt = ExpM \_ -> fmap Right (newGenSymWith idt)
  {-# INLINE newGenSymWith #-}

-- | @since 1.0.0
instance MonadResolve ExpM where
  newScopeId = ExpM \_ -> fmap Right newScopeId
  {-# INLINE newScopeId #-}

  newBind phase idt = ExpM \_ -> fmap Right (newBind phase idt)
  {-# INLINE newBind #-}

  resolveBind phase idt = ExpM \_ -> fmap Right (resolveBind phase idt)
  {-# INLINE resolveBind #-}

  resolve phase idt = ExpM \_ -> fmap Right (resolve phase idt)
  {-# INLINE resolve #-}