{-# LANGUAGE MultiParamTypeClasses #-}

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

import Control.Monad.Except (MonadError, catchError, replicateM, throwError)
import Control.Monad.Reader (MonadReader, ask, asks, local)
import Control.Monad.ST (runST)
import Control.Monad.State (MonadState, get, gets, modify, put, state)

import Data.Data (Data)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (for)

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.GenSym (GenSym, MonadGenSym (newGenSymWith))
import Opal.Common.Name (Name)
import Opal.Common.Symbol qualified as Symbol

import Opal.Core.Prim (Prim (PrimBoolFalse, PrimBoolTrue, PrimLambda, PrimLetSyntax, PrimSyntax))
import Opal.Core.Prim qualified as Prim

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
  ( Binder (BindName, BindPrim),
    Binding (Binding),
  )
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId), ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Transform (Transform (TfmDatum, TfmStop, TfmVar))

import Data.Foldable (for_)
import Opal.Common.GenSym qualified as GenSym
import Opal.Core (Datum (DatumStx), Expr, SExp (SExpApp, SExpVal))
import Opal.Evaluate (EvalCtx (EvalCtx), EvalExn, EvalStore (EvalStore))
import Opal.Evaluate qualified as Eval
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Parse (Parse, ParseError)
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
emptyExpandCtx :: ExpandCtx
emptyExpandCtx = ExpandCtx (Phase 0) Map.empty

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
emptyExpandStore = ExpandStore BindStore.empty (ScopeId 0) 0 mempty mempty

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runExpandSyntax :: Syntax -> Either ExpandError Syntax
runExpandSyntax stx =
  runExpand emptyExpandCtx emptyExpandStore do
    let stx' = Syntax.scope (Phase 0) (ScopeId 0) stx
    introPrimBinds
    expandSyntax stx'

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
  env <- EvalStore <$> gets bindstore <*> gets intro'scopes <*> gets use'scopes
  case runST (Eval.runEvalST (EvalCtx ph Nothing ve Map.empty) env (Eval.eval exp)) of
    (_, Left exn) -> throwError (ExnEvalError exn)
    (_, Right dtm) -> pure dtm

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
introPrimBind :: Prim -> Expand ()
introPrimBind prim =
  let name = Symbol.toName (Prim.primToSymbol prim)
      bind = Binding (ScopeSet.singleton $ ScopeId 0) (BindPrim prim)
   in modify \store ->
        store {bindstore = BindStore.insert name bind store.bindstore}

-- | TODO
--
-- @since 1.0.0
introBind :: StxIdt -> Expand GenSym
introBind idt = do
  ph <- asks phase
  gen <- newGenSymWith idt.symbol
  let name = Symbol.toName idt.symbol
      bind = Binding (MultiScopeSet.index ph idt.context.multiscope) (BindName $ GenSym.toName gen)
   in modify \store ->
        store {bindstore = BindStore.insert name bind store.bindstore}
  pure gen

-- | TODO
--
-- @since 1.0.0
introPrimBinds :: Expand ()
introPrimBinds = do
  introPrimBind PrimBoolFalse
  introPrimBind PrimBoolTrue
  introPrimBind PrimLambda
  introPrimBind PrimLetSyntax
  introPrimBind PrimSyntax

-- | TODO
--
-- @since 1.0.0
expandSyntax :: Syntax -> Expand Syntax
expandSyntax (StxAtom ctx atom) = expandStxAtom (StxIdt ctx atom)
expandSyntax (StxList ctx stxs) = expandStxList ctx stxs

-- | TODO
--
-- @since 1.0.0
expandStxAtom :: StxIdt -> Expand Syntax
expandStxAtom idt = do
  bind <- resolve idt
  case bind.binder of
    BindPrim prim -> do
      ph <- asks phase
      let ctx = Syntax.makeStxCtx ph bind.scopes idt.context
      let sym = Prim.primToSymbol prim
      pure (StxAtom ctx sym)
    BindName name -> do
      env <- asks environment
      case Map.lookup name env of
        Nothing -> undefined
        Just (TfmVar idt') -> pure (StxAtom idt'.context idt'.symbol)
        Just tfm -> undefined

-- | TODO
--
-- @since 1.0.0
expandStxList :: StxCtx -> [Syntax] -> Expand Syntax
expandStxList ctx [] = pure (StxList ctx [])
expandStxList ctx (StxAtom ctx' atom : stxs) = do
  bind <- resolve (StxIdt ctx atom)
  case bind.binder of
    BindPrim PrimLambda -> do
      let stx0 = stxs List.!! 0
      let stx1 = stxs List.!! 1

      ph <- asks phase

      vars <- parse (Parse.pStxFormalIdts stx0)
      scps <- replicateM (length vars) newScopeId
      gens <- traverse introBind vars

      let idts :: [StxIdt]
          idts = foldr ((:) . uncurry (Syntax.scopeIdt ph)) [] (zip scps vars)

          tfms :: [(Name, Transform)]
          tfms = foldr (\(gen, idt) rest -> (GenSym.toName gen, TfmVar idt) : rest) [] (zip gens idts)
       in local (bulkExtend tfms) do
            body <- expandSyntax (foldr (Syntax.scope ph) stx1 scps)
            pure (StxList ctx [StxAtom ctx' atom, stx0, body])
    BindPrim PrimLetSyntax -> do
      let stx0 = stxs List.!! 0
      let stx1 = stxs List.!! 1

      ph <- asks phase

      vars <- parse (Parse.pStxLetSyntaxBinds stx0)
      scps <- replicateM (length vars) newScopeId
      gens <- traverse (introBind . fst) vars

      for_ (zip3 scps gens vars) \(sc, gen, (idt, _)) -> do
        let binder = BindName (GenSym.toName gen)
        let scopes = MultiScopeSet.index ph idt.context.multiscope
        let binding = Binding (ScopeSet.insert sc scopes) binder
        modify \store ->
          store {bindstore = BindStore.insert (Symbol.toName idt.symbol) binding store.bindstore}

      vals <- for (zip gens vars) \(gen, (_, stx)) -> do
        exp <- parse (Parse.pSyntax stx)
        val <- evaluate exp
        pure (GenSym.toName gen, TfmDatum val)

      local (bulkExtend vals) do
        expandSyntax (foldr (Syntax.scope ph) stx1 scps)
    BindPrim PrimSyntax ->
      case stxs of
        [stx] -> pure stx
        _ -> undefined
    BindPrim prim -> undefined
    BindName name -> do
      transformer <- asks (Map.lookup name . environment)
      case transformer of
        Nothing -> undefined
        Just (TfmDatum dtm) -> do
          ph <- asks phase

          intro'scp <- newScopeId
          use'scp <- newScopeId

          let macapp = Syntax.flips ph intro'scp $ Syntax.scope ph use'scp $ StxList ctx (StxAtom ctx' atom : stxs)
          result <- evaluate (SExpApp (SExpVal dtm) [SExpVal $ DatumStx macapp])
          case result of
            DatumStx stx -> do
              expandSyntax (Syntax.flips ph intro'scp stx)
            _ -> undefined
        Just tfm -> undefined
expandStxList ctx (StxList ctx' stx : stxs) = do
  stxs' <- traverse expandSyntax (StxList ctx' stx : stxs)
  pure (StxList ctx stxs')
