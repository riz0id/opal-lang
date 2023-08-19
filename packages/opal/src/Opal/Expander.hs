{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Expander
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Opal.Expander
  ( -- * Expand
    Expand (..)
    -- ** Basic Operations
  , runExpand
  , runExpandFile
  , runExpandAndParseFile
  , runExpandSyntax
  , runExpandAndParseSyntax
    -- * ExpandConfig
  , ExpandConfig (..)
    -- * ExpandState
  , ExpandState (..)
  )
where

import Control.Lens (over, use, view, (^.), (%~), (.=), (%=), preview, review, (&), (.~))

import Control.Monad (unless, foldM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..))

import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.IORef (IORef, readIORef, newIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Primitive.MutVar (MutVar, newMutVar, modifyMutVar', readMutVar)
import Data.Text (Text)
import Data.Traversable (for)

import GHC.Exts (RealWorld)

import Opal.Binding (Binding (..))
import Opal.Binding.BindingStore qualified as BindingStore
import Opal.Binding.Environment (Environment)
import Opal.Binding.Environment qualified as Environment
import Opal.Common.Phase (phasePlus, Phase, PhaseShift)
import Opal.Common.Scope (MonadScope (..), Scope)
import Opal.Common.ScopeSet qualified as ScopeSet
import Opal.Common.Symbol (MonadGenSym (..), Symbol, symbolToString)
import Opal.Core (CoreForm (..))
import Opal.Error (ErrorNotBound(..))
import Opal.Evaluator (EvalConfig (..), EvalError (..), EvalState (..), runEvalSExp)
import Opal.Evaluator.Monad (evalBindingStore)
import Opal.Expander.Monad
import Opal.Parser
  ( CoreParseError (..)
  , ParseConfig (..)
  , ParseError (..)
  , runParseSyntax
  )
import Opal.Reader (runFileReader)
import Opal.Syntax
import Opal.Syntax.Definition
import Opal.Syntax.ScopeInfo qualified as ScopeInfo
import Opal.Syntax.TH (syntax)
import Opal.Syntax.Transformer
import Opal.Writer (Display (..), putDocLn, putDoc)

import Prelude hiding (id, mod)

import System.Exit (exitFailure)

import Text.Megaparsec (errorBundlePretty)
import Opal.Module

-- Expand - Basic Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
runExpandFile :: FilePath -> IO Syntax
runExpandFile filepath = do
  runFileReader filepath >>= \case
    Left  exn -> fail (errorBundlePretty exn)
    Right stx -> do
      (m, st) <- runExpandSyntax stx
      putStrLn (show st)
      pure m

-- | TODO: docs
--
-- @since 1.0.0
runExpandAndParseFile :: FilePath -> IO SExp
runExpandAndParseFile filepath = do
  runFileReader filepath >>= \case
    Left  exn -> fail (errorBundlePretty exn)
    Right stx -> do
      (expr, st) <- runExpandAndParseSyntax stx
      putStrLn (show st)
      pure expr

-- | TODO: docs
--
-- @since 1.0.0
runExpandSyntax :: Syntax -> IO (Syntax, ExpandState)
runExpandSyntax stx = do
  let stx' = syntaxScope Nothing def stx
  (result, logs) <- runExpand def def (expand stx')

  putDocLn 80 (display logs)

  case result of
    Left  exn -> do
      putDocLn 80 (display exn)
      exitFailure
    Right rx  -> pure rx

-- | TODO: docs
--
-- @since 1.0.0
runExpandAndParseSyntax :: Syntax -> IO (SExp, ExpandState)
runExpandAndParseSyntax stx = do
  let stx' = syntaxScope Nothing def stx
  (result, logs) <- runExpand def def (expandAndParseSyntax stx')

  putDocLn 80 (display logs)

  case result of
    Left  exn -> do
      putDocLn 80 (display exn)
      exitFailure
    Right rx  -> pure rx

-- Expand - Config Operations --------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
guardModuleContext :: Syntax -> Expand ()
guardModuleContext stx = do
  actual <- view expandContext
  let expected :: [ExpansionContext]
      expected = [ContextTopLevel, ContextModuleBegin]
   in unless (any (actual ==) expected) do
        throwError (ErrorBadContext stx expected actual)

-- | TODO: docs
--
-- @since 1.0.0
guardExpressionContext :: Syntax -> Expand ()
guardExpressionContext stx = do
  actual <- view expandContext
  let expected :: [ExpansionContext]
      expected = [ContextExpression, ContextDefinition]
   in unless (any (actual ==) expected) do
        throwError (ErrorBadContext stx expected actual)

-- | TODO: docs
--
-- @since 1.0.0
guardDefinitionContext :: Syntax -> Expand ()
guardDefinitionContext stx = do
  actual <- view expandContext
  let expected :: [ExpansionContext]
      expected = [ContextDefinition, ContextTopLevel, ContextModuleBegin]
   in unless (any (actual ==) expected) do
        throwError (ErrorBadContext stx expected actual)

-- | TODO: docs
--
-- @since 1.0.0
withTransformers :: [(Symbol, Transformer)] -> Expand a -> Expand a
withTransformers transExprs next = do
  prev <- use expandEnvironment
  expandEnvironment %= update
  result <- catchError next \exn -> do
    expandEnvironment .= prev
    throwError exn
  expandEnvironment .= prev
  pure result
  where
    update :: Environment -> Environment
    update env = foldr (uncurry Environment.insert) env transExprs

-- | TODO: docs
--
-- @since 1.0.0
withVarTransformers :: [(Symbol, Identifier)] -> Expand a -> Expand a
withVarTransformers = withValTransformers . map (fmap (DatumStx . review syntaxId))

-- | TODO: docs
--
-- @since 1.0.0
withValTransformers :: [(Symbol, Datum)] -> Expand a -> Expand a
withValTransformers binds next = do
  binds' <- for binds \(id, val) -> do
    ref <- liftIO (newIORef val)
    pure (id, TfmVal ref)

  withTransformers binds' next

-- | TODO: docs
--
-- @since 1.0.0
nextPhase :: Expand a -> Expand a
nextPhase = local (expandCurrentPhase %~ (`phasePlus` 1))

-- Expand - Binding Operations -------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newBinding :: Identifier -> Expand Symbol
newBinding (Identifier s info) = do
  phase  <- view expandCurrentPhase
  binder <- newGenSym

  let scopes  = ScopeInfo.lookup (Just phase) (info ^. stxInfoScopes)
  let binding = Binding scopes binder
  expandBindingStore %= BindingStore.insert s binding

  pure binder

-- Expand - Scoping Operations -------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newIntroScope :: Expand Scope
newIntroScope = do
  sc <- newScope
  expandIntroScopes %= ScopeSet.insert sc
  pure sc

-- | TODO: docs
--
-- @since 1.0.0
newUsageScope :: Expand Scope
newUsageScope = do
  sc <- newScope
  expandUsageScopes %= ScopeSet.insert sc
  pure sc

-- | TODO: docs
--
-- @since 1.0.0
scopeId :: Bool -> Scope -> Identifier -> Expand Identifier
scopeId False sc id = pure (identifierScope Nothing sc id)
scopeId True  sc id = do
  ph <- view expandCurrentPhase
  pure (identifierScope (Just ph) sc id)

-- | TODO: docs
--
-- @since 1.0.0
scopeSyntax :: Bool -> Scope -> Syntax -> Expand Syntax
scopeSyntax False sc id = pure (syntaxScope Nothing sc id)
scopeSyntax True  sc id = do
  ph <- view expandCurrentPhase
  pure (syntaxScope (Just ph) sc id)

-- | TODO: docs
--
-- @since 1.0.0
flipSyntax :: Scope -> Syntax -> Expand Syntax
flipSyntax sc id = do
  ph <- view expandCurrentPhase
  pure (syntaxFlipScope ph sc id)

-- Expand - Expand Operations --------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expanderReadFile :: Identifier -> Expand Syntax
expanderReadFile id = do
  let filepath = "../../lib/" ++ symbolToString (id ^. idtSymbol) ++ ".opal"
  result <- liftIO (runFileReader filepath)
  case result of
    Left  exn -> throwError (ExpandReaderError exn)
    Right stx -> pure stx

-- | TODO: docs
--
-- @since 1.0.0
expanderEval :: Maybe Scope -> SExp -> Expand Datum
expanderEval sc expr = do
  logExpand (LogEnterEval expr)

  config <- viewEvalConfig
  st0   <- useEvalState

  result <- liftIO (runEvalSExp config st0 expr)

  case result of
    Left  exn        -> throwError (evalToExpandError exn)
    Right (val, st1) -> do
      expandBindingStore .= st1 ^. evalBindingStore
      val <$ logExpand (LogExitEval val)
  where
    useEvalState :: Expand EvalState
    useEvalState =
      EvalState
        <$> use expandBindingStore
        <*> use expandIntroScopes
        <*> use expandUsageScopes

    viewEvalConfig :: Expand EvalConfig
    viewEvalConfig = do
      env <- use expandEnvironment
      ph  <- view expandCurrentPhase
      pure (EvalConfig env ph sc)

    evalToExpandError :: EvalError -> ExpandError
    evalToExpandError (EvalNotBound exn) = ExpandNotBound exn

-- | TODO: docs
--
-- @since 1.0.0
expanderParse :: Syntax -> Expand SExp
expanderParse stx = do
  logExpand (LogEnterParse stx)

  phase <- view expandCurrentPhase
  store <- use expandBindingStore

  let config = (ParseConfig store phase)
  result <- liftIO (runParseSyntax config stx)
  case result of
    Left  exn  -> throwError (parseToExpandError exn)
    Right sexp -> do
      logExpand (LogExitParse sexp)
      pure sexp
  where
    parseToExpandError :: ParseError -> ExpandError
    parseToExpandError (ParseErrorAmbiguous x)    = ExpandAmbiguous x
    parseToExpandError (ParseErrorEmptyApp  info) = ErrorBadSyntax CoreApp (SyntaxList [] info)
    parseToExpandError (ParseErrorCore      exn)  = case exn of
      ParseErrorLambda x -> ErrorBadSyntax CoreLambda x
      ParseErrorQuote  x -> ErrorBadSyntax CoreQuote x
      ParseErrorSyntax x -> ErrorBadSyntax CoreSyntax x

-- | TODO: docs
--
-- @since 1.0.0
expandAndParseSyntax :: Syntax -> Expand SExp
expandAndParseSyntax stx = do
  stx' <- expand stx
  expanderParse stx'

-- Expand - Expansion ----------------------------------------------------------

dispatch :: Transformer -> Syntax -> Expand Syntax
dispatch (TfmCore core) stx = dispatchCoreForm core stx
dispatch (TfmVal ref)   stx = dispatchTransformer ref stx

dispatchCoreForm :: CoreForm -> Syntax -> Expand Syntax
dispatchCoreForm CoreBegin stx = case stx of
  [syntax| (begin ?exprs ...+) |] -> expandBegin exprs
  _ -> throwError (ErrorBadSyntax CoreBegin stx)
dispatchCoreForm CoreDefine stx = do
  guardDefinitionContext stx
  throwError (ErrorBadSyntax CoreDefine stx)
dispatchCoreForm CoreDefineSyntax stx = do
  guardDefinitionContext stx
  throwError (ErrorBadSyntax CoreDefineSyntax stx)
dispatchCoreForm CoreLambda stx = case stx of
  [syntax| (lambda (?args:id ...) ?expr) |] -> expandLambda args expr
  _ -> throwError (ErrorBadSyntax CoreLambda stx)
dispatchCoreForm CoreLetRec stx = case stx of
  [syntax| (letrec-syntaxes+values (?trans ...) (?vals ...) ?expr) |] -> do
    transIds <- for trans \case
      [syntax| (?transId:id ?transExpr) |] -> pure (transId, transExpr)
      _ -> throwError (ErrorBadSyntax CoreLetRec stx)

    valIds <- for vals \case
      [syntax| (?valId:id ?valExpr) |] -> pure (valId, valExpr)
      _ -> throwError (ErrorBadSyntax CoreLetRec stx)

    expandLetRec transIds valIds expr
  _ -> throwError (ErrorBadSyntax CoreLetRec stx)
dispatchCoreForm CoreModule stx = case stx of
  [syntax| (module ?name:id ?imports ?exports ?expr) |] -> do
    mod <- expandModule name imports exports expr
    pure (moduleToSyntax mod)
  _ -> throwError (ErrorBadSyntax CoreModule stx)
dispatchCoreForm CoreQuote stx = case stx of
  [syntax| (quote ?expr) |] -> expandQuote expr
  _ -> throwError (ErrorBadSyntax CoreQuote stx)
dispatchCoreForm CoreSyntax stx = case stx of
  [syntax| (quote-syntax ?expr) |] -> expandQuoteSyntax expr
  _ -> throwError (ErrorBadSyntax CoreSyntax stx)
dispatchCoreForm core stx = do
  undefined

dispatchVariable :: Identifier -> Syntax -> Expand Syntax
dispatchVariable id stx = do
  logExpand (LogVariable id)
  applyRenameTransformer id stx

dispatchTransformer :: IORef Datum -> Syntax -> Expand Syntax
dispatchTransformer ref stx = do
  val <- liftIO (readIORef ref)

  case val of
    DatumLam fun         -> applyTransformer fun stx
    DatumStx transformer -> case preview syntaxId stx of
      Nothing -> expand [syntax| (?transformer ?stx) |]
      Just id -> dispatchVariable id stx
    _ -> do
      let transformer = datumToSyntax (stx ^. syntaxInfo) val
      expand [syntax| (?transformer ?stx) |]

applyTransformer :: Lambda -> Syntax -> Expand Syntax
applyTransformer t stx = do
  logExpand (LogEnterMacro stx)

  introScope <- newIntroScope
  introStx   <- flipSyntax introScope stx

  -- In a definition context, we need use-site scopes
  usageStx <- maybeCreateUseSiteScope introStx

  transformed <- do
    logExpand (LogEnterMacroExpand usageStx)

    expr   <- expanderParse [syntax| (?t:lam ?stx) |]
    result <- expanderEval (Just introScope) expr

    case result of
      DatumStx stx' -> do
        logExpand (LogExitMacroExpand usageStx)
        pure stx'
      _ ->
        error ("macro expansion did not return a syntax object")
        -- throwError (ErrorBadSyntax _ usageStx)

  -- Flip the introduction scope after the transformer has been applied.
  resultStx <- flipSyntax introScope transformed

  -- In a definition context, we need to add the inside-edge scope to
  -- any expansion result
  postStx <- maybeCreateInsideEdgeScope resultStx

  logExpand (LogExitMacro resultStx)

  pure postStx
  where
    maybeCreateUseSiteScope :: Syntax -> Expand Syntax
    maybeCreateUseSiteScope s = do
      ctx <- view expandContext
      if ctx == ContextDefinition
        then do
          usageScope <- newUsageScope
          scopeSyntax True usageScope s
        else pure s

    maybeCreateInsideEdgeScope :: Syntax -> Expand Syntax
    maybeCreateInsideEdgeScope s = do
      ctx <- view expandContext
      if ctx == ContextDefinition
        then do
          usageScope <- newUsageScope
          scopeSyntax True usageScope s
        else pure s

applyRenameTransformer :: Identifier -> Syntax -> Expand Syntax
applyRenameTransformer id stx = do
  introScope <- newIntroScope
  introId    <- scopeId True introScope id
  pure (syntaxTrackOrigin [syntax| ?introId:id |] stx)

expand :: Syntax -> Expand Syntax
expand stx = do
  logExpand (LogVisitSyntax stx)
  case stx of
    [syntax| ?id:id             |] -> expandId id
    [syntax| (?id:id ?stxs ...) |] -> expandIdApplication id stxs
    [syntax| (?stxs ...)        |] -> expandApplication stxs
    SyntaxVal val info             -> pure (SyntaxVal val info)

expandId :: Identifier -> Expand Syntax
expandId id = do
  t <- lookupEnvironment id
  dispatch t [syntax| ?id:id |]

expandIdApplication :: Identifier -> [Syntax] -> Expand Syntax
expandIdApplication id stxs = do
  t <- lookupEnvironment id
  dispatch t [syntax| (?id:id ?stxs ...) |]

expandApplication :: [Syntax] -> Expand Syntax
expandApplication stxs = do
  results <- traverse expand stxs
  pure [syntax| (?results ...) |]

-- Expand - Expansion - Core Forms ---------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expandLambda :: [Identifier] -> Syntax -> Expand Syntax
expandLambda ids expr = do
  guardExpressionContext [syntax| (lambda (?ids:id ...) ?expr) |]

  sc <- newScope

  bindings <- for ids \id -> do
    id'  <- scopeId True sc id
    bind <- newBinding id'
    pure (bind, id')

  withVarTransformers bindings do
    let args = map snd bindings
    stx'   <- scopeSyntax True sc expr
    result <- expand stx'
    pure [syntax| (lambda (?args:id ...) ?result) |]

-- | Expansion subroutine for the @letrec-syntaxes+values@ core syntactic form.
--
-- @since 1.0.0
expandLetRec ::
  -- | TODO: docs
  [(Identifier, Syntax)] ->
  -- | TODO: docs
  [(Identifier, Syntax)] ->
  -- | TODO: docs
  Syntax ->
  -- | TODO: docs
  Expand Syntax
expandLetRec transExprs valExprs expr = do
  guardExpressionContext
    let stxs = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) transExprs
        vals = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) transExprs
     in [syntax| (letrec-syntaxes+values (?stxs ...) (?vals ...) ?expr) |]

  sc <- newScope

  valBinds <- for valExprs \(valId, valExpr) -> do
    valId'   <- scopeId True sc valId
    valExpr' <- scopeSyntax True sc valExpr
    binder   <- newBinding valId'
    pure (valId, valExpr', binder)

  transBinds <- for transExprs \(transId, transExpr) -> do
    transId'   <- scopeId True sc transId
    transExpr' <- scopeSyntax True sc transExpr

    binder <- newBinding transId'
    result <- nextPhase (expandAndParseSyntax transExpr')

    case result of
      SVal val -> pure (binder, val)
      _        -> throwError (ErrorBadSyntax CoreLetRec transExpr')

  let letBinds = map (\(idt, _, b) -> (b, idt)) valBinds

  withVarTransformers letBinds do
    withValTransformers transBinds do
      vals <- for valBinds \(valId, valExpr, _) -> do
        result <- expand valExpr
        pure [syntax| (?valId:id ?result) |]

      scoped <- scopeSyntax True sc expr
      result <- expand scoped

      pure [syntax| (letrec (?vals ...) ?result) |]

-- | TODO: docs
--
-- @since 1.0.0
expandQuote :: Syntax -> Expand Syntax
expandQuote stx = do
  guardExpressionContext [syntax| (quote ?stx) |]

  pure [syntax| (quote ?stx) |]

-- | TODO: docs
--
-- @since 1.0.0
expandQuoteSyntax :: Syntax -> Expand Syntax
expandQuoteSyntax expr = do
  guardExpressionContext [syntax| (quote-syntax ?expr) |]

  phase  <- view expandCurrentPhase
  intros <- use expandIntroScopes
  let result = syntaxPrune phase intros expr

  pure [syntax| (quote-syntax ?result) |]

-- Expand - Expansion - Definitions --------------------------------------------

expandBegin :: NonEmpty Syntax -> Expand Syntax
expandBegin stxs = do
  guardExpressionContext [syntax| (begin ?stxs ...+) |]

  outsideEdgeScope <- newScope
  outsideEdgeStxs  <- traverse (scopeSyntax True outsideEdgeScope) stxs

  begin <- preExpandBegin outsideEdgeStxs

  insideEdgeScope <- newScope

  let stx = beginToLetRec begin
  result <- expand stx
  scopeSyntax True insideEdgeScope result

preExpandBegin :: NonEmpty Syntax -> Expand Begin
preExpandBegin stxs = do
  mut <- newMutVar []

  withDefinitionContext do
    for_ (NonEmpty.init stxs) \stx -> case stx of
      [syntax| (?id:id ?_ ...) |] -> preExpandIdApplication mut id stx
      _                           -> modifyMutVar' mut (++ [DefnExpr stx])

  defns <- readMutVar mut
  pure (Begin defns (NonEmpty.last stxs))

preExpandCoreDefinition :: MutVar RealWorld [Definition] -> CoreForm -> Syntax -> Expand ()
preExpandCoreDefinition defns CoreBegin stx = case stx of
  [syntax| (begin ?stxs ...+) |] -> do
    begin <- preExpandBegin stxs
    modifyMutVar' defns (++ beginToDefinitions begin)
  _ -> throwError (ErrorBadSyntax CoreBegin stx)
preExpandCoreDefinition defns CoreDefine stx = do
  defn <- preExpandDefine stx
  modifyMutVar' defns (++ [DefnDefine defn])
preExpandCoreDefinition defns CoreDefineSyntax stx = do
  defn <- preExpandDefineSyntax stx
  modifyMutVar' defns (++ [DefnSyntax defn])
preExpandCoreDefinition defns _ stx = do
  modifyMutVar' defns (++ [DefnExpr stx])

preExpandDefine :: Syntax -> Expand Define
preExpandDefine [syntax| (define ?id:id ?expr) |] = pure (Define id expr)
preExpandDefine stx                               = throwError (ErrorBadSyntax CoreDefine stx)

preExpandDefineSyntax :: Syntax -> Expand DefineSyntax
preExpandDefineSyntax [syntax| (define-syntax ?id:id ?expr) |] = pure (DefineSyntax id expr)
preExpandDefineSyntax stx                                      = throwError (ErrorBadSyntax CoreDefineSyntax stx)

preExpandIdApplication :: MutVar RealWorld [Definition] -> Identifier -> Syntax -> Expand ()
preExpandIdApplication defns id stx = do
  transformer <- lookupEnvironment id
  case transformer of
    TfmCore core -> do
      guardDefinitionContext stx
      preExpandCoreDefinition defns core stx
    TfmVal _ -> do
      modifyMutVar' defns (++ [DefnExpr stx])

-- Expand - Expansion - Modules ------------------------------------------------

expandModule :: Identifier -> Syntax -> Syntax -> Syntax -> Expand Module
expandModule name imports exports expr = do
  guardModuleContext [syntax| (module ?imports ?exports ?expr) |]

  ph <- view expandCurrentPhase

  withModuleContext do

    importIds <- fmap (map (ph,)) (expandImports imports)

    exportIds <- fmap (map (ph,)) (expandExports exports)

    moduleNs <- case expr of
      [syntax| (?id:id ?stxs ...) |] -> do
        transformer <- lookupEnvironment id
        case transformer of
          TfmCore CoreModuleBegin -> expandModuleBegin stxs
          _ -> throwError (ErrorBadSyntax CoreModuleBegin expr)
      _ -> throwError (ErrorBadSyntax CoreModuleBegin expr)

    pure (Module (name ^. idtSymbol) importIds exportIds moduleNs)

expandImports :: Syntax -> Expand [Identifier]
expandImports stx = case stx of
  [syntax| (?id:id ?ids:id ...) |] -> do
    transformer <- lookupEnvironment id
    case transformer of
      TfmCore CoreImport -> do

        for_ ids \path -> do
          importId path
          -- mod   <- importId path
          -- defns <- moduleExportDefns mod

          -- for_ defns \case
          --   (ph, DefnDefine (Define defnId _)) -> do
          --     binder <- newBinding defnId
          --     ref    <- liftIO (newIORef (DatumStx (identifierToSyntax defnId)))

          --     expandEnvironment %= Environment.insert binder (TfmVal ref)
          --   (ph, DefnSyntax (DefineSyntax id expr)) -> do
          --     undefined
          --   (_, DefnExpr _) ->
          --     pure ()

        pure ids
      _ ->
        throwError (ErrorBadSyntax CoreImport stx)
  _ -> throwError (ErrorBadSyntax CoreImport stx)
  where
    importId :: Identifier -> Expand Module
    importId id = do
      expr <- expanderReadFile id
      case syntaxScope Nothing def expr of
        [syntax| (module ?name:id ?imports ?exports ?body) |] ->
          withTopLevelContext do
            mod <- expandModule name imports exports body
            liftIO (putDocLn 80 (display mod))
            pure mod
        _ -> throwError (ErrorBadSyntax CoreImport expr)

    lookupDefinition :: Identifier -> [(PhaseShift, Definition)] -> Maybe (PhaseShift, Definition)
    lookupDefinition _  []       = Nothing
    lookupDefinition id (x : xs) = case x of
      (ph, DefnDefine defn)
        | id == defn ^. defineId -> Just (ph, DefnDefine defn)
        | otherwise              -> lookupDefinition id xs
      (ph, DefnSyntax defn)
        | id == defn ^. defineSyntaxId -> Just (ph, DefnSyntax defn)
        | otherwise                    -> lookupDefinition id xs
      (_,  DefnExpr _) -> lookupDefinition id xs

    moduleExportDefn :: Module -> [(Phase, Definition)] -> (Phase, Identifier) -> Expand [(Phase, Definition)]
    moduleExportDefn mod rest (ph, id) = do
      let moduleDefns = mod ^. moduleDefinitions
      case lookupDefinition id moduleDefns of
        Nothing         -> pure rest
        Just (sh, defn) -> pure ((ph `phasePlus` sh, defn) : rest)

    moduleExportDefns :: Module -> Expand [(Phase, Definition)]
    moduleExportDefns mod = do
      let exportIds   = mod ^. moduleExports
      foldM (moduleExportDefn mod) [] exportIds

expandExports :: Syntax -> Expand [Identifier]
expandExports stx@[syntax| (?id:id ?ids:id ...) |] = do
  transformer <- lookupEnvironment id
  case transformer of
    TfmCore CoreExport -> pure ids
    _ -> throwError (ErrorBadSyntax CoreExport stx)
expandExports stx = throwError (ErrorBadSyntax CoreExport stx)

expandModuleBegin :: [Syntax] -> Expand Namespace
expandModuleBegin stxs = do
  ph <- view expandCurrentPhase

  expandNamespace .= newNamespace ph

  withModuleBeginContext do

    defns <- for stxs \stx -> case stx of
      [syntax| (?id:id ?_ ...) |] -> do
        transformer <- lookupEnvironment id

        (sh, defn) <- case transformer of
          TfmCore CoreDefine -> do
            defn   <- preExpandDefine stx
            binder <- newBinding (defn ^. defineId)
            ref    <- liftIO (newIORef (DatumStx (identifierToSyntax (defn ^. defineId))))

            expandEnvironment %= Environment.insert binder (TfmVal ref)

            pure (0, DefnDefine defn)
          TfmCore CoreDefineSyntax -> do
            defn   <- preExpandDefineSyntax stx
            result <- withExpressionContext do
              nextPhase (expandAndParseSyntax (defn ^. defineSyntaxExpr))
            binder <- newBinding (defn ^. defineSyntaxId)

            case result of
              SVal val -> do
                ref <- liftIO (newIORef val)

                expandEnvironment %= Environment.insert binder (TfmVal ref)

                pure (1, DefnSyntax (defn & defineSyntaxExpr .~ datumToSyntax (defn ^. defineSyntaxExpr ^. syntaxInfo) val))
              _ -> do
                throwError (ErrorBadSyntax CoreDefineSyntax stx)
          _ -> do
            pure (0, DefnExpr stx)

        expandNamespace %= over nsDefinitions (++ [(0, defn)])

        pure (sh, defn)
      _ ->
        pure (0, DefnExpr stx)

    result <- for defns \(sh, decl) -> case decl of
      DefnDefine defn -> do
        expr <- withExpressionContext do
          expand (defn ^. defineExpr)
        pure (sh, DefnDefine (defn & defineExpr .~ expr))
      DefnSyntax _ -> do
        pure (sh, decl)
      DefnExpr expr -> do
        expr' <- expand expr
        pure (sh, DefnExpr expr')

    pure (Namespace ph [] result)


