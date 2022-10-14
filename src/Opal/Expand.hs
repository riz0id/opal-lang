{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Opal.Expand
  ( -- * Expand Monad
    Expand (Expand, unExpand),

    -- ** Expander Errors
    ExpandError (ExnResolveError),

    -- ** Expander Context
    ExpandCtx (ExpandCtx, phase, environment),

    -- ** Expander Store
    ExpandStore (ExpandStore, bindstore, intro'scopes, usage'scopes),

    -- * TODO
    runExpandSyntax,
    runExpand,

    -- * TODO
  )
where

import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, asks, local)
import Control.Monad.ST (runST)
import Control.Monad.State (MonadState, get, gets, modify, put, state)

import Data.Data (Data)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.GenSym (MonadGenSym, newGenSymWith)
import Opal.Common.Name (Name)
import Opal.Common.Symbol qualified as Symbol

import Opal.Core.CoreForm
  ( CoreForm (CoreFormLambda, CoreFormLetSyntax, CoreFormSyntax),
  )

import Opal.Expand.Resolve (ResolveError)
import Opal.Expand.Resolve qualified as Resolve
import Opal.Expand.Syntax
  ( StxCtx,
    StxIdt (StxIdt, context),
    Syntax (StxAtom, StxList),
  )
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.BindStore (BindStore)
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.Binding
  ( Binding (Binding),
  )
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId), ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Transform
  ( Transform (TfmDtm, TfmVar),
  )

import Debug.Trace qualified as Debug
import Opal.Common.GenSym qualified as GenSym
import Opal.Common.Symbol (Symbol)
import Opal.Core
  ( Datum (..),
    Expr,
    SExp (SExpApp, SExpVal),
  )
import Opal.Core.CoreForm (CoreForm (CoreFormQuote))
import Opal.Core.CorePrim (CorePrim (CorePrimSyntaxLocalValue))
import Opal.Core.CorePrim qualified as CorePrim
import Opal.Core.Datum (Datum (DatumBool))
import Opal.Evaluate (EvalCtx (EvalCtx), EvalExn, EvalStore (EvalStore))
import Opal.Evaluate qualified as Eval
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Parse (Parse, ParseError (ExnParseLambda))
import Opal.Parse qualified as Parse

-- Expand Monad ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype Expand a = Expand
  { unExpand ::
      ExpandCtx ->
      ExpandStore ->
      (# ExpandStore, (# ExpandError| a #) #)
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
instance MonadReader ExpandCtx Expand where
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
      let gen = GenSym.GenSym symbol st0.next'genId
          st1 = st0 {next'genId = succ st0.next'genId}
       in (# st1, (# | gen #) #)
  {-# INLINE newGenSymWith #-}

-- Expand Monad - Expander Errors ----------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ExpandError
  = ExnResolveError ResolveError
  | ExnParseError ParseError
  | ExnEvalError EvalExn
  | ExnRecievedNotSyntax Datum
  | ExnUnboundTransformer Name (Map Name Transform)
  deriving (Eq, Ord, Show)

-- Expand Monad -Expander Contexts ---------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ExpandCtx = ExpandCtx
  { phase :: {-# UNPACK #-} !Phase
  , environment :: Map Name Transform
  }
  deriving (Data, Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
newExpandCtx :: ExpandCtx
newExpandCtx =
  ExpandCtx (Phase 0) Map.empty

-- | TODO
--
-- @since 1.0.0
extend :: Name -> Transform -> ExpandCtx -> ExpandCtx
extend name tfm ctx = ctx {environment = Map.insert name tfm ctx.environment}

-- | TODO
--
-- @since 1.0.0
bulkExtend :: [(Name, Transform)] -> ExpandCtx -> ExpandCtx
bulkExtend exts ctx = foldr (uncurry extend) ctx exts

-- Expand Monad - Expander Store -----------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ExpandStore = ExpandStore
  { bindstore :: BindStore
  , next'scope :: {-# UNPACK #-} !ScopeId
  , next'genId :: {-# UNPACK #-} !Int
  , usage'scopes :: ScopeSet
  , intro'scopes :: ScopeSet
  }
  deriving (Eq, Ord, Show)

-- | TODO
--
-- @since 1.0.0
emptyExpandStore :: ExpandStore
emptyExpandStore = ExpandStore BindStore.empty (ScopeId 1) 0 mempty mempty

-- | TODO
--
-- @since 1.0.0
modifyExpandBinds' :: (BindStore -> BindStore) -> Expand ()
modifyExpandBinds' f =
  modifyExpandStore' \store ->
    store {bindstore = f (bindstore store)}

-- | TODO
--
-- @since 1.0.0
modifyExpandIntroScopes' :: (ScopeSet -> ScopeSet) -> Expand ()
modifyExpandIntroScopes' f =
  modifyExpandStore' \store ->
    store {intro'scopes = f (intro'scopes store)}

-- | TODO
--
-- @since 1.0.0
modifyExpandUsageScopes' :: (ScopeSet -> ScopeSet) -> Expand ()
modifyExpandUsageScopes' f =
  modifyExpandStore' \store ->
    store {usage'scopes = f (usage'scopes store)}

-- | TODO
--
-- @since 1.0.0
modifyExpandStore' :: (ExpandStore -> ExpandStore) -> Expand ()
modifyExpandStore' f = modify (f $!)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runExpandSyntax :: Syntax -> Either ExpandError Syntax
runExpandSyntax stx =
  runExpand newExpandCtx emptyExpandStore do
    let coreScope = ScopeId 0

    -- Introduce bindings to core syntax
    introPrimBinding "quote" coreScope
    introPrimBinding "syntax" coreScope
    introPrimBinding "lambda" coreScope
    introPrimBinding "let-syntax" coreScope

    -- Introduce bindings to core primitives
    introPrimBinding "#t" coreScope
    introPrimBinding "#f" coreScope
    introPrimBinding "syntax-local-value" coreScope

    let corePrims :: [(Name, Transform)]
        corePrims =
          [ ("quote", TfmDtm (DatumCore CoreFormQuote))
          , ("syntax", TfmDtm (DatumCore CoreFormSyntax))
          , ("lambda", TfmDtm (DatumCore CoreFormLambda))
          , ("let-syntax", TfmDtm (DatumCore CoreFormLetSyntax))
          , ("#t", TfmDtm (DatumBool True))
          , ("#f", TfmDtm (DatumBool False))
          , ("syntax-local-value", TfmDtm (DatumPrim CorePrimSyntaxLocalValue))
          ]
     in local (bulkExtend corePrims) do
          stx' <- scopeSyntaxM coreScope stx
          syntaxExpand stx'

-- | TODO
--
-- @since 1.0.0
runExpand :: ExpandCtx -> ExpandStore -> Expand a -> Either ExpandError a
runExpand ctx st0 mx =
  case unExpand mx ctx st0 of
    (# _, (# e | #) #) -> Left e
    (# _, (# | x #) #) -> Right x
{-# INLINE runExpand #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
parse :: Parse a -> Expand a
parse px = do
  ph <- asks phase
  binds <- gets bindstore
  case Parse.runParse ph binds px of
    Left exn -> throwError (ExnParseError exn)
    Right sexp -> pure sexp
{-# INLINE parse #-}

-- | TODO
--
-- @since 1.0.0
indexEnvironment :: Name -> Expand Transform
indexEnvironment name = do
  env <- asks environment
  case Map.lookup name env of
    Nothing -> throwError (ExnUnboundTransformer name env)
    Just rx -> pure rx

-- | TODO
--
-- @since 1.0.0
resolve :: StxIdt -> Expand Binding
resolve idt = do
  ph <- asks phase
  binds <- gets bindstore
  case Resolve.runResolveId ph idt binds of
    Left exn -> throwError (ExnResolveError exn)
    Right bind -> pure bind

-- | TODO
--
-- @since 1.0.0
evaluate :: Expr -> Expand Datum
evaluate exp = do
  ph <- asks phase
  ve <- asks environment
  env <- makeEvalStore
  case runST (Eval.runEvalST (EvalCtx ph Nothing ve Map.empty) env (Eval.eval exp)) of
    (_, Left exn) -> throwError (ExnEvalError exn)
    (_, Right dtm) -> pure dtm

-- | TODO
--
-- @since 1.0.0
evaluateWithScope :: ScopeId -> Expr -> Expand Datum
evaluateWithScope sc exp = do
  ph <- asks phase
  ve <- asks environment
  env <- makeEvalStore
  case runST (Eval.runEvalST (EvalCtx ph (Just sc) ve Map.empty) env (Eval.eval exp)) of
    (_, Left exn) -> throwError (ExnEvalError exn)
    (_, Right dtm) -> pure dtm

-- | TODO
--
-- @since 1.0.0
makeEvalStore :: Expand EvalStore
makeEvalStore = do
  EvalStore
    <$> gets bindstore
    <*> gets intro'scopes
    <*> gets usage'scopes

-- | TODO
--
-- @since 1.0.0
newScopeId :: Expand ScopeId
newScopeId = do
  state \store ->
    let store' :: ExpandStore
        store' = store {next'scope = succ store.next'scope}
     in (store.next'scope, store')

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
syntaxExpand :: Syntax -> Expand Syntax
syntaxExpand stx = do
  ph <- asks phase
  let coreScope :: ScopeId
      coreScope = ScopeId 0
   in case Syntax.scope ph coreScope stx of
        StxAtom ctx atom -> stxIdtExpand (StxIdt ctx atom)
        StxList ctx stxs -> stxListExpand ctx stxs

-- | TODO
--
-- @since 1.0.0
stxIdtExpand :: StxIdt -> Expand Syntax
stxIdtExpand idt = do
  bind <- resolve idt
  env <- asks environment
  case Map.lookup bind.binder env of
    Nothing -> do
      pure $ error ("unbound transformer: expandStxAtom: " ++ show bind)
    Just (TfmVar idt') -> pure idt'.syntax
    Just (TfmDtm (DatumBool bool)) -> do
      let symbol :: Symbol
          symbol = if bool then "#t" else "#f"
       in pure (StxAtom idt.context symbol)
    Just (TfmDtm datum) -> do
      let macro = idt.syntax
      applyTransformer (SExpVal datum) [macro]
    Just tfm -> do
      error ("stxIdtExpand: " ++ show tfm)

stxListExpand :: StxCtx -> [Syntax] -> Expand Syntax
stxListExpand ctx (StxAtom ctx' atom : stxs) = do
  idtApplicationExpand ctx (StxIdt ctx' atom) stxs
stxListExpand ctx stxs = do
  stxs' <- traverse syntaxExpand stxs
  pure (StxList ctx stxs')

--------------------------------------------------------------------------------

idtApplicationExpand :: StxCtx -> StxIdt -> [Syntax] -> Expand Syntax
idtApplicationExpand ctx idt stxs = do
  bind <- resolve idt
  form <- indexEnvironment bind.binder
  case form of
    TfmDtm (DatumCore CoreFormQuote) -> do
      let quote = StxAtom idt.context "quote"
      pure (StxList ctx (quote : stxs))
    TfmDtm (DatumCore CoreFormSyntax) -> do
      let syntax = StxAtom idt.context "syntax"
      stxs' <- traverse pruneSyntaxM stxs
      pure (StxList ctx (syntax : stxs'))
    TfmDtm (DatumCore CoreFormLambda)
      | [stx, body] <- stxs -> do
          let lambda = StxAtom idt.context "lambda"
          args <- parse (Parse.pStxFormalIdts stx)
          (sc, body') <- lambdaBodyExpand args body
          scopeSyntaxM sc (StxList ctx [lambda, stx, body'])
      | otherwise -> do
          throwError (ExnParseError (ExnParseLambda stxs))
    TfmDtm (DatumCore CoreFormLetSyntax) -> do
      (binds, body) <- parse (Parse.pStxLetSyntax stxs)
      letSyntaxExpand binds body
    TfmDtm (DatumPrim prim) -> do
      let func = StxIdt idt.context (CorePrim.toSymbol prim)
      applicationExpand ctx func stxs
    TfmDtm (DatumProc args body) -> do
      let macro = StxList ctx (idt.syntax : stxs)
      applyTransformer (SExpVal (DatumProc args body)) [macro]
    TfmDtm dtm -> do
      error ("name application expand: " ++ show dtm)
    TfmVar func -> do
      applicationExpand ctx func stxs

applicationExpand :: StxCtx -> StxIdt -> [Syntax] -> Expand Syntax
applicationExpand ctx func stxs = do
  stxs' <- traverse syntaxExpand stxs
  pure (StxList ctx (func.syntax : stxs'))

--------------------------------------------------------------------------------

lambdaBodyExpand :: [StxIdt] -> Syntax -> Expand (ScopeId, Syntax)
lambdaBodyExpand args body = do
  scope <- newScopeId
  args' <- traverse (scopeStxIdtM scope) args
  forms <- traverse makeArgTransform args'
  body' <- scopeSyntaxM scope body
  local (bulkExtend forms) do
    modifyExpandIntroScopes' (ScopeSet.insert scope)
    final <- syntaxExpand body'
    pure (scope, final)
  where
    makeArgTransform :: StxIdt -> Expand (Name, Transform)
    makeArgTransform arg = do
      name <- makeBinding arg
      pure (name, TfmVar arg)

--------------------------------------------------------------------------------

letSyntaxExpand :: [(StxIdt, Syntax)] -> Syntax -> Expand Syntax
letSyntaxExpand vars body = do
  scope <- newScopeId
  names <- traverse (makeLetSyntaxBind scope) vars
  forms <- traverse makeDatumTransform names
  body' <- scopeSyntaxM scope body
  local (bulkExtend forms) do
    modifyExpandIntroScopes' (ScopeSet.insert scope)
    final <- syntaxExpand body'
    pure final
  where
    makeLetSyntaxBind :: ScopeId -> (StxIdt, Syntax) -> Expand (Name, Syntax)
    makeLetSyntaxBind sc (idt, stx) = do
      ph <- asks phase
      name <- makeBinding (Syntax.scopeIdt ph sc idt)
      pure (name, stx)

    makeDatumTransform :: (Name, Syntax) -> Expand (Name, Transform)
    makeDatumTransform (name, stx) = do
      value <- letSyntaxBindExpand stx
      pure (name, TfmDtm value)

letSyntaxBindExpand :: Syntax -> Expand Datum
letSyntaxBindExpand stx = do
  prev'prunes <- state \env ->
    (intro'scopes env, env {intro'scopes = ScopeSet.empty})

  final <- local (\ctx -> ctx {phase = succ ctx.phase}) do
    stx' <- syntaxExpand stx
    expr <- parse (Parse.pSyntax stx')
    let !_ = Debug.trace (show expr) ()
    evaluate expr

  modifyExpandIntroScopes' (const prev'prunes)
  pure final

--------------------------------------------------------------------------------

scopeSyntaxM :: ScopeId -> Syntax -> Expand Syntax
scopeSyntaxM sc stx = asks \ctx -> Syntax.scope (phase ctx) sc stx

scopeStxIdtM :: ScopeId -> StxIdt -> Expand StxIdt
scopeStxIdtM sc idt = asks \ctx -> Syntax.scopeIdt (phase ctx) sc idt

pruneSyntaxM :: Syntax -> Expand Syntax
pruneSyntaxM stx = do
  ph <- asks phase
  sc <- gets intro'scopes
  let !_ = Debug.trace ("pruning syntax: " ++ show stx ++ ", of scopes: " ++ show sc) ()
  pure (Syntax.prune ph sc stx)

flipsSyntaxM :: ScopeId -> Syntax -> Expand Syntax
flipsSyntaxM sc stx = asks \ctx -> Syntax.flips (phase ctx) sc stx

applyTransformer :: Expr -> [Syntax] -> Expand Syntax
applyTransformer func stxs = do
  intro <- newScopeId
  usage <- newScopeId
  stx' <- traverse (flipsSyntaxM intro <=< scopeSyntaxM usage) stxs

  modifyExpandIntroScopes' (ScopeSet.insert usage)
  modifyExpandUsageScopes' (ScopeSet.insert intro)

  let macro = SExpApp func (map (SExpVal . DatumStx) stx')
  evaluateWithScope intro macro >>= \case
    DatumStx result -> do
      stx'e <- flipsSyntaxM intro result
      final <- syntaxExpand stx'e
      pure final
    other -> do
      throwError (ExnRecievedNotSyntax other)

makeBinder :: Symbol -> Expand Name
makeBinder symbol = fmap GenSym.toName (newGenSymWith symbol)

introPrimBinding :: Symbol -> ScopeId -> Expand ()
introPrimBinding symbol sc = do
  let name = Symbol.toName symbol
  let bind = Binding (ScopeSet.singleton sc) name
  modifyExpandBinds' (BindStore.insert name bind)

makeBinding :: StxIdt -> Expand Name
makeBinding idt = do
  ph <- asks phase
  binder <- makeBinder idt.symbol
  let scps = MultiScopeSet.index ph idt.context.multiscope
  let bind = Binding scps binder
  modifyExpandBinds' (BindStore.insert (Symbol.toName idt.symbol) bind)
  pure bind.binder
