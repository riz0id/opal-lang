{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Opal.Evaluate
  ( -- * Evaluation
    runKernalEval,

    -- * Eval Monad
    EvalIO,
    runEvalIO,
    Eval (Eval, unEval),
    runEvalST,

    -- ** Evaluator Errors
    EvalExn (EvalExnUnbound, EvalExnCallLit, EvalExnArity),

    -- ** Evaluator Context
    EvalCtx (EvalCtx, phase, scope, bindEnv, value'env),

    -- ** Evaluator Store
    EvalStore (EvalStore, bindstore, prune'intros, prune'uses),

    -- ** Operations
    eval,
  )
where

import Control.Exception (Exception, displayException, toException)

import Control.Monad (unless, (<=<))
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Control.Monad.Reader (MonadReader, ask, asks, local)
import Control.Monad.ST (ST, runST)
import Control.Monad.State (MonadState, get, gets, put, state)

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)

import GHC.Exts (RealWorld, State#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Core
  ( CoreForm,
    Datum (DatumCore, DatumPrim, DatumProc, DatumStx),
    Expr,
    SExp (SExpApp, SExpVal, SExpVar),
  )

import Opal.Core.CorePrim (CorePrim (CorePrimSyntaxLocalValue))
import Opal.Expand.Resolve (ResolveError)
import Opal.Expand.Resolve qualified as Resolve
import Opal.Expand.Syntax (StxIdt (StxIdt), Syntax (StxAtom, StxList))
import Opal.Expand.Syntax.BindStore (BindStore)
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.Binding (Binding)
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))
import Opal.Expand.Syntax.ScopeSet (ScopeId, ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Transform (Transform (TfmDtm))
import qualified Debug.Trace as Debug

-- Evaluation ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runKernalEval :: Expr -> Either EvalExn Datum
runKernalEval expr = runST do
  let ctx = EvalCtx (Phase 0) Nothing Map.empty Map.empty
  let env = EvalStore BindStore.coreSyntax ScopeSet.empty ScopeSet.empty
  (_, result) <- runEvalST ctx env (eval expr)
  pure result

-- Evaluator Monad -------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runEvalIO ::
  EvalCtx RealWorld ->
  EvalStore ->
  EvalIO a ->
  IO (EvalStore, a)
runEvalIO ctx env0 evalM =
  primitive \st0# ->
    case unEval evalM ctx env0 st0# of
      (# st1#, _, (# e | #) #) -> GHC.raiseIO# (toException e) st1#
      (# st1#, env1, (# | x #) #) -> (# st1#, (env1, x) #)

-- | TODO
--
-- @since 1.0.0
type EvalIO :: Type -> Type
type EvalIO = Eval RealWorld

-- | TODO
--
-- @since 1.0.0
runEvalST ::
  EvalCtx s ->
  EvalStore ->
  Eval s a ->
  ST s (EvalStore, Either EvalExn a)
runEvalST ctx env0 evalM =
  primitive \st0# ->
    case unEval evalM ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, (env1, Left e) #)
      (# st1#, env1, (# | x #) #) -> (# st1#, (env1, Right x) #)
{-# INLINE runEvalST #-}

-- | TODO
--
-- @since 1.0.0
newtype Eval s a = Eval
  { unEval ::
      EvalCtx s ->
      EvalStore ->
      State# s ->
      (# State# s, EvalStore, (# EvalExn| a #) #)
  }

-- | @since 1.0.0
instance Functor (Eval s) where
  fmap f (Eval k) =
    Eval \ctx env0 st0# -> case k ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | f x #) #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative (Eval s) where
  pure x = Eval \_ env st# -> (# st#, env, (# | x #) #)
  {-# INLINE pure #-}

  Eval f <*> Eval g =
    Eval \ctx env0 st0# -> case f ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | k #) #) -> case g ctx env1 st1# of
        (# st2#, env2, (# e | #) #) -> (# st2#, env2, (# e | #) #)
        (# st2#, env2, (# | x #) #) -> (# st2#, env2, (# | k x #) #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad (Eval s) where
  Eval k >>= f =
    Eval \ctx env0 st0# -> case k ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> (# st1#, env1, (# e | #) #)
      (# st1#, env1, (# | x #) #) -> unEval (f x) ctx env1 st1#
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadError EvalExn (Eval s) where
  throwError e = Eval \_ env st# -> (# st#, env, (# e | #) #)
  {-# INLINE throwError #-}

  catchError (Eval k) f =
    Eval \ctx env0 st0# -> case k ctx env0 st0# of
      (# st1#, env1, (# e | #) #) -> unEval (f e) ctx env1 st1#
      (# st1#, env1, (# | x #) #) -> (# st1#, env1, (# | x #) #)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadReader (EvalCtx s) (Eval s) where
  ask = Eval \ctx env st0# -> (# st0#, env, (# | ctx #) #)
  {-# INLINE ask #-}

  local f (Eval k) = Eval \ctx env -> k (f ctx) env
  {-# INLINE local #-}

-- | @since 1.0.0
instance MonadState EvalStore (Eval s) where
  get = Eval \_ env st# -> (# st#, env, (# | env #) #)
  {-# INLINE get #-}

  put env = Eval \_ _ st# -> (# st#, env, (# | () #) #)
  {-# INLINE put #-}

  state k =
    Eval \_ env0 st# -> case k env0 of
      (x, env1) -> (# st#, env1, (# | x #) #)
  {-# INLINE state #-}

-- | @since 1.0.0
instance PrimMonad (Eval s) where
  type PrimState (Eval s) = s

  primitive k =
    Eval \_ env st0# -> case k st0# of
      (# st1#, x #) -> (# st1#, env, (# | x #) #)
  {-# INLINE primitive #-}

-- Eval Monad - Evaluator Errors -----------------------------------------------

-- | TODO
--
-- @since 1.0.0
data EvalExn
  = EvalExnUnbound {-# UNPACK #-} !Name (Map Name Datum)
  | EvalExnResolve ResolveError
  | EvalExnCallLit Expr
  | EvalExnArity {-# UNPACK #-} !Int {-# UNPACK #-} !Int Expr
  | EvalExnCallPrim CoreForm [Datum]
  deriving (Eq, Ord, Show)

-- @since 1.0.0
instance Exception EvalExn where
  displayException = show
  {-# INLINE displayException #-}

-- Eval Monad - Evaluator Context ----------------------------------------------

-- | TODO
--
-- @since 1.0.0
data EvalCtx s = EvalCtx
  { phase :: {-# UNPACK #-} !Phase
  -- ^ TODO
  , scope :: {-# UNPACK #-} !(Maybe ScopeId)
  -- ^ TODO
  , bindEnv :: Map Name Transform
  -- ^ TODO
  , value'env :: Map Name (MutVar s Datum)
  -- ^ TODO
  }
  deriving (Eq)

-- | TODO
--
-- @since 1.0.0
-- newEvalCtx :: PrimMonad m => Phase -> Maybe ScopeId -> Map Name Transform -> m (EvalCtx (PrimState m))
-- newEvalCtx ph sc env = fmap (EvalCtx ph sc env) (newMutVar Map.empty)
-- {\-# INLINE newEvalCtx #-\}

-- | TODO
--
-- @since 1.0.0
extEvalEnv ::
  PrimMonad m =>
  Name ->
  Datum ->
  EvalCtx (PrimState m) ->
  m (EvalCtx (PrimState m))
extEvalEnv name val ctx = do
  let valEnv = value'env ctx
  case Map.lookup name valEnv of
    Nothing -> do
      mut <- newMutVar val
      pure ctx {value'env = Map.insert name mut valEnv}
    Just mut -> do
      writeMutVar mut val
      pure ctx
{-# INLINE extEvalEnv #-}

-- | TODO
--
-- @since 1.0.0
extsEvalEnv ::
  PrimMonad m =>
  [Name] ->
  [Datum] ->
  EvalCtx (PrimState m) ->
  m (EvalCtx (PrimState m))
extsEvalEnv [] _ ctx = pure ctx
extsEvalEnv _ [] ctx = pure ctx
extsEvalEnv (name : names) (val : vals) ctx = do
  extEvalEnv name val =<< extsEvalEnv names vals ctx
{-# INLINE extsEvalEnv #-}

-- Eval Monad - Evaluator Store ------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data EvalStore = EvalStore
  { bindstore :: {-# UNPACK #-} !BindStore
  -- ^ TODO
  , prune'intros :: ScopeSet
  -- ^ TODO
  , prune'uses :: ScopeSet
  -- ^ TODO
  }
  deriving (Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
resolve :: StxIdt -> Eval s Binding
resolve idt = do
  ph <- asks phase
  binds <- gets bindstore
  case Resolve.runResolveId ph idt binds of
    Left exn -> throwError (EvalExnResolve exn)
    Right bind -> pure bind

-- Eval Monad - Operations -----------------------------------------------------

-- | TODO
--
-- @since 1.0.0
eval :: Expr -> Eval s Datum
eval (SExpVal dtm) = evalDatum dtm
eval (SExpVar var) = getVariable var
eval (SExpApp fun args) = evalCall fun args
{-# INLINE eval #-}

-- | TODO
--
-- @since 1.0.0
evalDatum :: Datum -> Eval s Datum
evalDatum dtm = pure dtm

-- | TODO
--
-- @since 1.0.0
evalCall :: Expr -> [Expr] -> Eval s Datum
evalCall (SExpVar var) args = do
  val <- getVariable var
  evalCallProc val args
evalCall (SExpVal val) args =
  evalCallProc val args
evalCall (SExpApp fun args') args = do
  val <- evalCall fun args'
  evalCallProc val args
{-# INLINE evalCall #-}

-- | TODO
--
-- @since 1.0.0
evalCallProc :: Datum -> [Expr] -> Eval s Datum
evalCallProc fun@(DatumProc vars body) args = do
  checkCallArity (length vars) fun args
  vals <- traverse eval args
  ctx' <- extsEvalEnv vars vals =<< ask
  local (const ctx') (eval body)
evalCallProc (DatumCore form) args =
  undefined
evalCallProc (DatumPrim prim) args = do
  evallCallPrimProc prim args
evalCallProc val args = do
  let expr = SExpApp (SExpVal val) args
  throwError (EvalExnCallLit expr)
{-# INLINE evalCallProc #-}

-- | TODO
--
-- @since 1.0.0
evallCallPrimProc :: CorePrim -> [Expr] -> Eval s Datum
evallCallPrimProc CorePrimSyntaxLocalValue args = do
  let !_ = Debug.trace (show args) ()
  args' <- traverse eval args
  case args' of
    [DatumStx (StxList _ [_, StxAtom ctx atom])] -> do
      store <- gets bindstore
      let !_ = Debug.trace (show store) ()
      binding <- resolve (StxIdt ctx atom)
      env <- asks bindEnv
      undefined
      -- let name = Binding.toName binding
      -- case Map.lookup name env of
      --   Nothing -> error ("evaluation error: syntax-local-value missing transformer to " ++ show atom)
      --   Just (TfmDtm val) -> pure val
      --   Just tfm -> undefined
    _ -> do
      let expr = SExpApp (SExpVal $ DatumPrim $ CorePrimSyntaxLocalValue) args
      error ("evaluation error: bad arguments to syntax-local-value " ++ show expr)

-- | TODO
--
-- @since 1.0.0
checkCallArity :: Int -> Datum -> [Expr] -> Eval s ()
checkCallArity n fun args =
  let numArgs :: Int
      numArgs = length args
   in unless (n == numArgs) do
        let expr = SExpApp (SExpVal fun) args
        throwError (EvalExnArity n numArgs expr)
{-# INLINE checkCallArity #-}

-- | TODO
--
-- @since 1.0.0
getVariable :: Name -> Eval s Datum
getVariable = readMutVar <=< getMutRef
{-# INLINE getVariable #-}

-- | TODO
--
-- @since 1.0.0
getMutRef :: Name -> Eval s (MutVar s Datum)
getMutRef name = do
  valEnv <- asks value'env
  case Map.lookup name valEnv of
    Nothing -> do
      valEnv' <- traverse readMutVar valEnv
      throwError (EvalExnUnbound name valEnv')
    Just mut ->
      pure mut
{-# INLINE getMutRef #-}