{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Parse.Monad
  ( Parse (P#, unP#),
  )
where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.State (MonadState, get, put, state)

import Data.Kind (Type)
import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc
import Data.SrcLoc.Prim (SrcLoc#)

--------------------------------------------------------------------------------

import Data.Parse.Context (ParseCtx (ParseCtx), ParseCtx#)
import Data.Parse.Error (Error)

--------------------------------------------------------------------------------

newtype Parse (a :: Type) :: Type where
  P# :: {unP# :: ParseCtx# -> SrcLoc# -> (# SrcLoc#, (# Error| a #) #)} -> Parse a

instance Functor Parse where
  fmap f (P# k) =
    P# \ctx# loc0# -> case k ctx# loc0# of
      (# loc1#, (# e | #) #) -> (# loc1#, (# e | #) #)
      (# loc1#, (# | x #) #) -> (# loc1#, (# | f x #) #)
  {-# INLINE fmap #-}

instance Applicative Parse where
  pure x = P# \_ loc# -> (# loc#, (# | x #) #)
  {-# INLINE pure #-}

  P# f <*> P# g =
    P# \ctx# loc0# -> case f ctx# loc0# of
      (# loc1#, (# e | #) #) -> (# loc1#, (# e | #) #)
      (# loc1#, (# | k #) #) -> case g ctx# loc1# of
        (# loc2#, (# e | #) #) -> (# loc2#, (# e | #) #)
        (# loc2#, (# | x #) #) -> (# loc2#, (# | k x #) #)
  {-# INLINE (<*>) #-}

instance Monad Parse where
  P# k >>= f =
    P# \ctx# loc0# -> case k ctx# loc0# of
      (# loc1#, (# e | #) #) -> (# loc1#, (# e | #) #)
      (# loc1#, (# | x #) #) -> unP# (f x) ctx# loc1#
  {-# INLINE (>>=) #-}

instance MonadError Error Parse where
  throwError e = P# \_ loc# -> (# loc#, (# e | #) #)
  {-# INLINE throwError #-}

  catchError (P# k) f =
    P# \ctx# loc0# -> case k ctx# loc0# of
      (# loc1#, (# e | #) #) -> unP# (f e) ctx# loc1#
      (# loc1#, (# | x #) #) -> (# loc1#, (# | x #) #)
  {-# INLINE catchError #-}

instance MonadReader ParseCtx Parse where
  ask = P# \ctx# loc# -> (# loc#, (# | ParseCtx ctx# #) #)
  {-# INLINE ask #-}

  local f (P# k) =
    P# \ctx0# loc# ->
      let !(ParseCtx ctx1#) = f (ParseCtx ctx0#)
       in k ctx1# loc#
  {-# INLINE local #-}

instance MonadState SrcLoc Parse where
  get = P# \_ loc# -> (# loc#, (# | SrcLoc.box loc# #) #)
  {-# INLINE get #-}

  put loc = P# \_ _ -> (# SrcLoc.unbox loc, (# | () #) #)
  {-# INLINE put #-}

  state k =
    P# \_ loc0# -> case k (SrcLoc.box loc0#) of
      (x, loc1) -> (# SrcLoc.unbox loc1, (# | x #) #)
  {-# INLINE state #-}
