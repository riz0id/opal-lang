{-# LANGUAGE MultiParamTypeClasses #-}

module Opal.Expand.Monad (
  Expand (Expand, unExpand),
  ExpandError (..),
  module Opal.Expand.Monad.ExpandContext,
  module Opal.Expand.Monad.ExpandStore,
) where

--------------------------------------------------------------------------------

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.State (MonadState, get, put, state)

import Data.Map.Strict (Map)

--------------------------------------------------------------------------------

import Opal.Common.GenSym (MonadGenSym, newGenSymWith)
import Opal.Common.GenSym qualified as GenSym
import Opal.Common.Name (Name)

import Opal.Core.Datum (Datum)

import Opal.Parse (ParseError)

import Opal.Expand.Monad.ExpandContext
import Opal.Expand.Monad.ExpandStore
import Opal.Expand.Resolve (MonadResolve, ResolveError, resolveBind)
import Opal.Expand.Resolve qualified as Resolve
import Opal.Expand.Syntax (Syntax)
import Opal.Expand.Transform (Transform)

-- Expand Monad ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype Expand a = Expand
  { unExpand ::
      ExpandContext ->
      ExpandStore ->
      (# ExpandStore, (# ExpandError | a #) #)
  }

-- | @since 1.0.0
instance Functor Expand where
  fmap f (Expand k) =
    Expand \ctx st0 -> case k ctx st0 of
      (# st1, (# e | #) #) -> (# st1, (# e | #) #)
      (# st1, (# | x #) #) -> (# st1, (# | f x #) #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative Expand where
  pure x = Expand \_ st -> (# st, (# | x #) #)
  {-# INLINE pure #-}

  Expand f <*> Expand g =
    Expand \ctx st0 -> case f ctx st0 of
      (# st1, (# e | #) #) -> (# st1, (# e | #) #)
      (# st1, (# | k #) #) -> case g ctx st1 of
        (# st2, (# e | #) #) -> (# st2, (# e | #) #)
        (# st2, (# | x #) #) -> (# st2, (# | k x #) #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad Expand where
  Expand k >>= f =
    Expand \ctx st0 -> case k ctx st0 of
      (# st1, (# e | #) #) -> (# st1, (# e | #) #)
      (# st1, (# | x #) #) -> unExpand (f x) ctx st1
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadError ExpandError Expand where
  throwError e = Expand \_ st -> (# st, (# e | #) #)
  {-# INLINE throwError #-}

  catchError (Expand k) f =
    Expand \ctx st0 -> case k ctx st0 of
      (# st1, (# e | #) #) -> unExpand (f e) ctx st1
      (# st1, (# | x #) #) -> (# st1, (# | x #) #)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadReader ExpandContext Expand where
  ask = Expand \ctx st -> (# st, (# | ctx #) #)
  {-# INLINE ask #-}

  local f (Expand k) = Expand \ctx st -> k (f ctx) st
  {-# INLINE local #-}

-- | @since 1.0.0
instance MonadState ExpandStore Expand where
  get = Expand \_ st -> (# st, (# | st #) #)
  {-# INLINE get #-}

  put st = Expand \_ _ -> (# st, (# | () #) #)
  {-# INLINE put #-}

  state k =
    Expand \_ st0 -> case k st0 of
      (x, st1) -> (# st1, (# | x #) #)
  {-# INLINE state #-}

-- | @since 1.0.0
instance MonadGenSym Expand where
  newGenSymWith symbol = do
    Expand \_ st0 ->
      let gen = GenSym.GenSym symbol st0.state'next'genId
          st1 = st0 {state'next'genId = succ st0.state'next'genId}
       in (# st1, (# | gen #) #)
  {-# INLINE newGenSymWith #-}

-- | @since 1.0.0
instance MonadResolve Expand where
  resolveBind idt =
    Expand \ctx env ->
      let ph = ctx'phase ctx
          bs = state'bindstore env
       in case Resolve.runResolveId ph idt bs of
            Left exn -> (# env, (# ExnResolveError exn | #) #)
            Right bx -> (# env, (# | bx #) #)

-- Expand Monad - Expander Errors ----------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ExpandError
  = ExnResolveError ResolveError
  | ExnParseError ParseError
  | ExnRecievedNotSyntax Datum
  | ExnUnboundTransformer Name (Map Name Transform)
  | ExnApplicationToValue Datum [Syntax]
  deriving (Eq, Ord, Show)