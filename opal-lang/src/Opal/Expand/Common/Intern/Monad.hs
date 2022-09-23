{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Opal.Expand.Common.Intern.Monad
  ( -- * TODO
    InternIO,

    -- * TODO
    Intern (I, unI),
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Control.Monad.State.Strict (MonadState, get, put, state)

import Data.Kind (Type)
import Data.Set (Set)

import GHC.Exts (MutVar#, RealWorld, State#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
type InternIO :: Type -> Type
type InternIO = Intern RealWorld

-- | TODO
--
-- @since 1.0.0
newtype Intern (s :: Type) (a :: Type) :: Type where
  I :: {unI :: MutVar# s (Set Symbol) -> State# s -> (# State# s, a #)} -> Intern s a

-- | @since 1.0.0
instance Functor (Intern s) where
  fmap f (I k) =
    I \mut# st0# -> case k mut# st0# of
      (# st1#, x #) -> (# st1#, f x #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative (Intern s) where
  pure x = I \_ st# -> (# st#, x #)
  {-# INLINE pure #-}

  I f <*> I g =
    I \mut# st0# ->
      let !(# st1#, k #) = f mut# st0#
          !(# st2#, x #) = g mut# st1#
       in (# st2#, k x #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad (Intern s) where
  I k >>= f =
    I \mut# st0# -> case k mut# st0# of
      (# st1#, x #) -> unI (f x) mut# st1#
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance PrimMonad (Intern s) where
  type PrimState (Intern s) = s

  primitive k = I \_ -> k
  {-# INLINE primitive #-}

-- | @since 1.0.0
instance MonadState (Set Symbol) (Intern s) where
  get = I GHC.readMutVar#
  {-# INLINE get #-}

  put x = I \mut# st0# -> (# GHC.writeMutVar# mut# x st0#, () #)
  {-# INLINE put #-}

  state k = I (`GHC.atomicModifyMutVar#` k)
  {-# INLINE state #-}