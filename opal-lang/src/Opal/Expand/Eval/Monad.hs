{-# LANGUAGE MultiParamTypeClasses #-}
module Opal.Expand.Eval.Monad
  ( -- * TODO
    EvalExn (ExnUnbound, ExnAppToDatum),

    -- * TODO
    EvalCtx (EvalCtx, evalPhase, evalScope, evalEnv),
    valueBind,
    indexLocalSyntax,

    -- * TODO
    EvalState (EvalState, evalBindings, evalUseScopes, evalIntroScopes),

    -- * TODO
    Eval (Eval, unEval),
  )
where

import Control.Monad.State (MonadState, get, put, state)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.Except (MonadError, throwError, catchError)

import Data.Kind (Type)
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------

import Opal.Expand.Syntax.ScopeSet ( ScopeSet, ScopeId )
import Opal.Expand.Syntax.BindTable (BindTable)
import Opal.Expand.Syntax.MultiScopeSet (Phase)
import Opal.Expr (Datum, Expr)
import Opal.Common.Symbol (Symbol)
import qualified Data.Map.Strict as Map
import Opal.Expand.Transformer (Transform)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data EvalExn :: Type where 
  ExnUnbound :: Symbol -> EvalExn
  ExnAppToDatum :: Datum -> [Expr] -> EvalExn
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data EvalCtx = EvalCtx
  { evalPhase :: Phase
  , evalScope :: Maybe ScopeId
  , evalEnv :: Map Symbol Datum
  , evalBindEnv :: Map Symbol Transform
  }
  deriving (Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
valueBind :: Symbol -> Datum -> EvalCtx -> EvalCtx 
valueBind var val ctx = ctx{evalEnv = Map.insert var val ctx.evalEnv}
{-# INLINE valueBind #-}

-- | TODO
--
-- @since 1.0.0
indexLocalSyntax :: Symbol -> EvalCtx -> Maybe Transform
indexLocalSyntax idt ctx = Map.lookup idt ctx.evalBindEnv 

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data EvalState = EvalState
  { evalBindings :: BindTable
  , evalUseScopes :: ScopeSet
  , evalIntroScopes :: ScopeSet
  }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype Eval (a :: Type) :: Type where
  Eval :: {unEval :: EvalCtx -> EvalState -> (# EvalState, (# EvalExn | a #) #)} -> Eval a

-- | @since 1.0.0
instance Functor Eval where 
  fmap f (Eval k) = 
    Eval \ctx st0 -> case k ctx st0 of 
      (# st1, (# e | #) #) -> (# st1, (# e | #) #)
      (# st1, (# | x #) #) -> (# st1, (# | f x #) #)
  {-# INLINe fmap #-}

-- | @since 1.0.0
instance Applicative Eval where 
  pure x = Eval \_ st -> (# st, (# | x #) #)
  {-# INLINE pure #-}

  Eval f <*> Eval g = 
    Eval \ctx st0 -> case f ctx st0 of 
      (# st1, (# e | #) #) -> (# st1, (# e | #) #) 
      (# st1, (# | k #) #) -> case g ctx st1 of 
        (# st2, (# e | #) #) -> (# st2, (# e | #) #) 
        (# st2, (# | x #) #) -> (# st2, (# | k x #) #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad Eval where 
  Eval k >>= f =
    Eval \ctx st0 -> case k ctx st0 of 
      (# st1, (# e | #) #) -> (# st1, (# e | #) #)
      (# st1, (# | x #) #) -> unEval (f x) ctx st1 
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadError EvalExn Eval where 
  throwError e = Eval \_ st -> (# st, (# e | #) #)
  {-# INLINE throwError #-}

  catchError (Eval k) f = 
    Eval \ctx st0 -> case k ctx st0 of 
      (# st1, (# e | #) #) -> unEval (f e) ctx st1 
      (# st1, (# | x #) #) -> (# st1, (# | x #) #)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadReader EvalCtx Eval where 
  ask = Eval \ctx st -> (# st, (# | ctx #) #)
  {-# INLINE ask #-}
  
  local f (Eval k) = Eval \ctx st0 -> k (f ctx) st0 
  {-# INLINE local #-}

-- | @since 1.0.0
instance MonadState EvalState Eval where 
  get = Eval \_ st -> (# st, (# | st #) #)
  {-# INLINE get #-}

  put st = Eval \_ _ -> (# st, (# | () #) #)
  {-# INLINE put #-}

  state k = Eval \_ st0 -> let !(x, st1) = k st0 in (# st1, (# | x #) #)
  {-# INLINE state #-}
