module Control.Monad.Cont
  ( -- * TODO
    ContM (ContM, runContM),
    evalContM,

    -- * TODO
    callCC,
    shiftM,
    resetM,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Kind (Type)

--------------------------------------------------------------------------------

newtype ContM (r :: Type) (m :: Type -> Type) (a :: Type) :: Type where
  ContM :: {runContM :: (a -> m r) -> m r} -> ContM r m a

instance Functor m => Functor (ContM r m) where
  fmap f (ContM k) = ContM \c -> k (c . f)
  {-# INLINE fmap #-}

instance Applicative m => Applicative (ContM r m) where
  pure x = ContM \c -> c x
  {-# INLINE pure #-}

  ContM f <*> ContM g = ContM \c -> f \k -> g \x -> c (k x)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (ContM r m) where
  ContM k >>= f = ContM \c -> k \x -> runContM (f x) c
  {-# INLINE (>>=) #-}

instance MonadIO m => MonadIO (ContM r m) where
  liftIO io = ContM \c -> liftIO io >>= c
  {-# INLINE liftIO #-}

evalContM :: Applicative m => ContM r m r -> m r
evalContM (ContM k) = k pure
{-# INLINE evalContM #-}

callCC :: ((a -> ContM r m b) -> ContM r m a) -> ContM r m a
callCC k = ContM \c -> runContM (k \x -> ContM \_ -> c x) c
{-# INLINE callCC #-}

shiftM :: Monad m => ((a -> m r) -> ContM r m r) -> ContM r m a
shiftM f = ContM (evalContM . f)
{-# INLINE shiftM #-}

resetM :: Monad m => ContM r m r -> ContM x m r
resetM (ContM k) = ContM \c -> k pure >>= c
{-# INLINE resetM #-}