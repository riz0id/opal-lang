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
    ExpandStore (ExpandStore, bindstore, intro'scopes, use'scopes),

    -- * TODO
    runExpandSyntax,
    runExpand,

    -- * TODO
  )
where

import Control.Monad.Except (MonadError, catchError, liftEither, replicateM, throwError)
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
import Opal.Core.CoreForm qualified as CoreForm

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
  ( Binder (BindCore, BindName),
    Binding (Binding),
  )
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId), ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Transform
  ( Transform (TfmDtm, TfmLambda, TfmLetSyntax, TfmQuote, TfmSyntax, TfmVar),
  )

import Control.Monad (foldM, zipWithM, (<=<))
import Debug.Trace qualified as Debug
import GHC.Exts (fromList)
import Opal.Common.GenSym qualified as GenSym
import Opal.Common.Symbol (Symbol)
import Opal.Core
  ( CoreForm (CoreFormQuote),
    Datum (DatumAtom, DatumStx, DatumPrim, DatumProc),
    Expr,
    SExp (SExpApp, SExpVal),
  )
import Opal.Core.Datum (Datum (DatumBool))
import Opal.Evaluate (EvalCtx (EvalCtx), EvalExn, EvalStore (EvalStore))
import Opal.Evaluate qualified as Eval
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Parse (Parse, ParseError (ExnParseLambda))
import Opal.Parse qualified as Parse
import Opal.Core.CorePrim (CorePrim(CorePrimSyntaxLocalValue))

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
  , use'scopes :: ScopeSet
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
modifyExpandUseScopes' :: (ScopeSet -> ScopeSet) -> Expand ()
modifyExpandUseScopes' f =
  modifyExpandStore' \store ->
    store {use'scopes = f (intro'scopes store)}

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
    introPrimBinds

    -- Introduce bindings to core primitives
    primTrue <- introPrimBinding "#t" coreScope
    primFalse <- introPrimBinding "#f" coreScope
    primSyntaxLocalValue <- introPrimBinding "syntax-local-value" coreScope

    let corePrims :: [(Name, Transform)]
        corePrims =
          [ (Binding.toName primTrue, TfmDtm (DatumBool True))
          , (Binding.toName primFalse, TfmDtm (DatumBool False))
          , (Binding.toName primSyntaxLocalValue, TfmDtm (DatumPrim CorePrimSyntaxLocalValue))
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
    Left exn ->
      let !_ = Debug.trace (show idt) ()
       in throwError (ExnResolveError exn)
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
    <*> gets use'scopes

-- | TODO
--
-- @since 1.0.0
newScopeId :: Expand ScopeId
newScopeId = do
  state \store ->
    let store' :: ExpandStore
        store' = store {next'scope = succ store.next'scope}
     in (store.next'scope, store')

-- | TODO
--
-- @since 1.0.0
introPrimBind :: CoreForm -> Expand ()
introPrimBind form = do
  let name = Symbol.toName (CoreForm.primToSymbol form)
  let bind = Binding (ScopeSet.singleton $ ScopeId 0) (BindCore form)
  modifyExpandBinds' (BindStore.insert name bind)

-- | TODO
--
-- @since 1.0.0
introPrimBinds :: Expand ()
introPrimBinds = do
  introPrimBind CoreFormLambda
  introPrimBind CoreFormLetSyntax
  introPrimBind CoreFormSyntax
  introPrimBind CoreFormQuote

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
  case bind.binder of
    BindCore prim -> do
      ph <- asks phase
      let ctx = Syntax.makeStxCtx ph bind.scopes idt.context
      let sym = CoreForm.primToSymbol prim
      pure (StxAtom ctx sym)
    BindName name -> do
      env <- asks environment
      case Map.lookup name env of
        Nothing -> do
          let !_ = Debug.trace (show env) ()
          pure $ error ("unbound transformer: expandStxAtom: " ++ show name)
        Just (TfmVar idt') -> pure idt'.syntax
        Just (TfmDtm (DatumBool bool)) -> do
          let symbol :: Symbol
              symbol = if bool then "#t" else "#f"
           in pure (StxAtom idt.context symbol)
        Just (TfmDtm (DatumStx stx)) -> do
          let !_ = Debug.trace ("syntax: " ++ show stx) ()
          pure stx
        Just (TfmDtm datum) -> do
          let !_ = Debug.trace ("in expandSyntaxAtom: ") ()
          throwError (ExnRecievedNotSyntax datum)
        Just tfm -> do
          let !_ = Debug.trace ("in expandSyntaxAtom: " ++ show tfm) ()
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
  binding <- resolve idt
  case binding.binder of
    BindCore CoreFormQuote -> do
      pure (StxList ctx (idt.syntax : stxs))
    BindCore CoreFormSyntax -> do
      stxs' <- traverse pruneSyntaxM stxs
      pure (StxList ctx (idt.syntax : stxs'))
    BindCore CoreFormLambda
      | [stx, stx'] <- stxs -> do
          args <- parse (Parse.pStxFormalIdts stx)
          body <- lambdaBodyExpand args stx'
          pure (StxList ctx [idt.syntax, stx, body])
      | otherwise -> do
          throwError (ExnParseError (ExnParseLambda stxs))
    BindCore CoreFormLetSyntax -> do
      (binds, body) <- parse (Parse.pStxLetSyntax stxs)
      letSyntaxExpand binds body
    BindName name -> do
      nameApplicationExpand ctx idt.context name stxs 

nameApplicationExpand :: StxCtx -> StxCtx -> Name -> [Syntax] -> Expand Syntax
nameApplicationExpand ctx ctxProc name stxs = do
  indexEnvironment name >>= \case
    TfmQuote -> do
      error ("nameApplicationExpand: TfmQuote: " ++ show name)
    TfmSyntax -> do
      error ("nameApplicationExpand: TfmSyntax: " ++ show name)
    TfmLambda -> do
      error ("nameApplicationExpand: TfmLambda: " ++ show name)
    TfmLetSyntax -> do
      error ("nameApplicationExpand: TfmLetSyntax: " ++ show name)
    TfmDtm (DatumPrim prim) -> do
      applyTransformer (SExpVal (DatumPrim prim)) stxs
    TfmDtm (DatumProc args body) -> do
      let macro = StxList ctx (StxAtom ctxProc (Symbol.fromName name) : stxs)
      applyTransformer (SExpVal (DatumProc args body)) [macro]
    TfmDtm dtm -> do
      let !_ = Debug.trace (show dtm) ()
      error ("name application expand: " ++ show dtm)
    TfmVar func -> do
      applicationExpand ctx func stxs

applicationExpand :: StxCtx -> StxIdt -> [Syntax] -> Expand Syntax
applicationExpand ctx func stxs = do
  stxs' <- traverse syntaxExpand stxs
  pure (StxList ctx (func.syntax : stxs'))

--------------------------------------------------------------------------------

lambdaBodyExpand :: [StxIdt] -> Syntax -> Expand Syntax
lambdaBodyExpand args body = do
  scps <- replicateM (length args) newScopeId
  tfms <- zipWithM makeArgTransform scps args
  local (bulkExtend tfms) do
    modifyExpandIntroScopes' (ScopeSet.union (fromList scps))
    body' <- foldM (flip scopeSyntaxM) body scps
    syntaxExpand body'
  where
    makeArgTransform :: ScopeId -> StxIdt -> Expand (Name, Transform)
    makeArgTransform sc arg = do
      let name = Symbol.toName arg.symbol
      binding <- makeBinding arg sc
      modifyExpandBinds' (BindStore.insert name binding)
      pure (Binding.toName binding, TfmVar arg)

--------------------------------------------------------------------------------

letSyntaxExpand :: [(StxIdt, Syntax)] -> Syntax -> Expand Syntax
letSyntaxExpand binds body = do
  scps <- replicateM (length binds) newScopeId
  gens <- zipWithM makeLetSyntaxBind scps binds
  tfms <- traverse makeDatumTransform gens

  local (bulkExtend tfms) do
    modifyExpandIntroScopes' (ScopeSet.union (fromList scps))
    body' <- foldM (flip scopeSyntaxM) body scps
    syntaxExpand body'
  where
    makeLetSyntaxBind :: ScopeId -> (StxIdt, Syntax) -> Expand (Binding, Syntax)
    makeLetSyntaxBind sc (idt, stx) = do
      let name = Symbol.toName idt.symbol
      binding <- makeBinding idt sc
      modifyExpandBinds' (BindStore.insert name binding)
      pure (binding, stx)
    
    makeDatumTransform :: (Binding, Syntax) -> Expand (Name, Transform)
    makeDatumTransform (binding, stx) = do
      value <- letSyntaxBindExpand stx
      pure (Binding.toName binding, TfmDtm value)

letSyntaxBindExpand :: Syntax -> Expand Datum
letSyntaxBindExpand stx = do
  ctx <- asks \ctx -> ctx {phase = succ (phase ctx)}
  env <- gets \env -> env {intro'scopes = mempty}
  liftEither $ runExpand ctx env do
    stx' <- syntaxExpand stx
    expr <- parse (Parse.pSyntax stx')
    evaluate expr

--------------------------------------------------------------------------------

scopeSyntaxM :: ScopeId -> Syntax -> Expand Syntax
scopeSyntaxM sc stx = asks \ctx -> Syntax.scope (phase ctx) sc stx

pruneSyntaxM :: Syntax -> Expand Syntax
pruneSyntaxM stx = do
  ph <- asks phase
  sc <- gets intro'scopes
  pure (Syntax.prune ph sc stx)

flipsSyntaxM :: ScopeId -> Syntax -> Expand Syntax
flipsSyntaxM sc stx = asks \ctx -> Syntax.flips (phase ctx) sc stx

newIntroScopeId :: Expand ScopeId
newIntroScopeId = do
  sc <- newScopeId
  modifyExpandIntroScopes' (ScopeSet.insert sc)
  pure sc

newUseScopeId :: Expand ScopeId
newUseScopeId = do
  sc <- newScopeId
  modifyExpandUseScopes' (ScopeSet.insert sc)
  pure sc

applyPrimitive :: CorePrim -> [Syntax] -> Expand Syntax 
applyPrimitive prim stxs = do 
  i'sc <- newIntroScopeId
  u'sc <- newUseScopeId
  stx' <- traverse (flipsSyntaxM i'sc <=< scopeSyntaxM u'sc) stxs
  let application :: Expr
      application = SExpApp (SExpVal (DatumPrim prim)) (map (SExpVal . DatumStx) stx')
   in evaluateWithScope i'sc application >>= \case
        DatumStx result -> do
          final <- flipsSyntaxM i'sc result
          syntaxExpand final
        other -> do
          let !_ = Debug.trace ("in applyTransformer: ") ()
          throwError (ExnRecievedNotSyntax other)

applyTransformer :: Expr -> [Syntax] -> Expand Syntax
applyTransformer func stxs = do
  i'sc <- newIntroScopeId
  u'sc <- newUseScopeId
  stx' <- traverse (flipsSyntaxM i'sc <=< scopeSyntaxM u'sc) stxs
  let macro :: Expr
      macro = SExpApp func (map (SExpVal . DatumStx) stx')
   in evaluateWithScope i'sc macro >>= \case
        DatumStx result -> do
          final <- flipsSyntaxM i'sc result
          syntaxExpand final
        other -> do
          let !_ = Debug.trace ("in applyTransformer: ") ()
          throwError (ExnRecievedNotSyntax other)

makeBinder :: Symbol -> Expand Binder
makeBinder symbol = do
  gen <- newGenSymWith symbol
  let name :: Name
      name = GenSym.toName gen
   in pure (BindName name)

introPrimBinding :: Symbol -> ScopeId -> Expand Binding
introPrimBinding symbol sc = do
  let name = Symbol.toName symbol
  binding <- makePrimBinding symbol sc
  modifyExpandBinds' (BindStore.insert name binding)
  pure binding

makePrimBinding :: Symbol -> ScopeId -> Expand Binding
makePrimBinding symbol sc = do
  binder <- makeBinder symbol
  let scopes :: ScopeSet
      scopes = ScopeSet.singleton sc
   in pure (Binding scopes binder)

makeBinding :: StxIdt -> ScopeId -> Expand Binding
makeBinding idt sc = do
  ph <- asks phase
  binder <- makeBinder idt.symbol
  let scopes :: ScopeSet
      scopes = MultiScopeSet.index ph idt.context.multiscope
   in pure (Binding.scope sc (Binding scopes binder))