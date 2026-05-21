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
    -- ** Macro-state scoping
  , withIntroScope
  , addUseSiteScope
  , addInsideEdgeScope
  , addOutsideEdgeScope
    -- ** Expand / parse / eval (re-exported for tests)
  , expand
  , expanderEval
  , expanderParse
  , importModule
    -- * ExpandConfig
  , ExpandConfig (..)
    -- * ExpandState
  , ExpandState (..)
  )
where

import Control.Lens (use, view, (^.), (%~), (.=), (%=), over, preview, review, set)

import Control.Monad (unless)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..))

import Data.Default (Default (..))
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Primitive.MutVar (MutVar, newMutVar, modifyMutVar', readMutVar)
import Data.Traversable (for)

import GHC.Exts (RealWorld)

import Opal.Binding (Binding (..))
import Opal.Binding.BindingStore qualified as BindingStore
import Opal.Binding.Environment (Environment)
import Opal.Binding.Environment qualified as Environment
import Opal.Common.Phase (phasePlus, PhaseShift, Phase (..))
import Opal.Common.Scope (MonadScope (..), Scope)
import Opal.Common.ScopeSet qualified as ScopeSet
import Opal.Common.Symbol (MonadGenSym (..), Symbol, symbolToString, eqSymbol)
import Opal.Core (CoreForm (..))
import Opal.Error (ErrorNotInScope (..))
import Opal.Evaluator (EvalConfig (..), EvalError (..), EvalState (..), runEvalSExp)
import Opal.Evaluator.Monad (evalBindingStore)
import Opal.Expander.DefinitionContext
  ( DefinitionContext (..)
  , insertUseSiteScope
  , readUseSiteScopes
  )
import Opal.Expander.Match
import Opal.Expander.Monad
import Opal.Parser (ParseConfig (..), ParseError (..), runParseSyntax)
import Opal.Module
import Opal.Module.Import
import Opal.Reader (runFileReader)
import Opal.Syntax
import Opal.Syntax.Definition
import Opal.Syntax.ScopeInfo qualified as ScopeInfo
import Opal.Syntax.TH (syntax)
import Opal.Syntax.Transformer
import Opal.Writer (Display (..), putDocLn)

import Prelude hiding (id, mod)

import System.Exit (exitFailure)

import Text.Megaparsec (errorBundlePretty)
import Control.Lens.Extras (is)
import Opal.Module.Export
import System.Environment.Blank (getExecutablePath)
import Control.Monad.Writer (tell)
import qualified Data.Map.Strict as Map
import Data.Functor (void)

-- Expand - Basic Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
runExpandFile :: FilePath -> IO Syntax
runExpandFile filepath = do
  runFileReader filepath >>= \case
    Left  exn -> fail (errorBundlePretty exn)
    Right stx -> do
      (m, st) <- runExpandSyntax (Just filepath) stx
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
runExpandSyntax :: Maybe FilePath -> Syntax -> IO (Syntax, ExpandState)
runExpandSyntax filepath stx = do

  let config :: ExpandConfig
      config = def { expand_file_path = filepath }

  (result, logs) <- runExpand config def do
    let stx' = syntaxScope Nothing def stx
    expandNamespace %= declareModule "#%core" (newCoreModule def) False
    void (importModule def "#%core")
    result <- expandModule stx'
    pure (moduleToSyntax result)

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
guardTopLevelContext :: Syntax -> Expand ()
guardTopLevelContext stx = do
  actual <- view expandContext
  let expected :: [ExpansionContext]
      expected = [ContextTopLevel]
   in unless (any (actual ==) expected) do
        throwError (ErrorBadContext stx expected actual)

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
withValTransformers = withTransformers . map (fmap TfmDatum)

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

-- | Run an action with a fresh macro-introduction scope pushed onto
-- 'expandIntroScopes' via 'local'. The scope is visible to nested
-- expansions inside the action and automatically drops out of the
-- intro set on exit — so sibling macro invocations cannot see each
-- other's intro scopes (the @leak-globally@ bug fix).
--
-- @since 1.0.0
withIntroScope :: (Scope -> Expand a) -> Expand a
withIntroScope k = do
  sc <- newScope
  local (over expandIntroScopes (ScopeSet.insert sc)) (k sc)

-- | Attach a fresh use-site scope to @s@ when expanding inside a
-- definition context (module body, internal @begin@, top level).
-- Outside a definition context, this is a no-op — use-site scopes
-- have no meaning there.
--
-- The fresh scope is added to the surrounding 'DefinitionContext'\'s
-- use-site accumulator so that @partialExpandModuleBegin@ can later
-- prune it off the binders this macro produces.
--
-- @since 1.0.0
addUseSiteScope :: Syntax -> Expand Syntax
addUseSiteScope s =
  view expandDefinitionContext >>= \case
    Nothing -> pure s
    Just dc -> do
      usc <- newScope
      insertUseSiteScope usc dc
      pure (syntaxScope Nothing usc s)

-- | Attach the surrounding 'DefinitionContext'\'s /inside-edge/ scope
-- to @s@. Unlike 'addUseSiteScope', this uses the same shared scope
-- for every macro output in the context (it's allocated once per
-- 'newDefinitionContext') — that's what Racket's inside-edge scope
-- means.
--
-- Outside a definition context this is a no-op.
--
-- @since 1.0.0
addInsideEdgeScope :: Syntax -> Expand Syntax
addInsideEdgeScope s =
  view expandDefinitionContext >>= \case
    Nothing -> pure s
    Just dc -> pure (syntaxScope Nothing (defctx_inside_edge_scope dc) s)

-- | Attach the surrounding 'DefinitionContext'\'s /outside-edge/
-- scope to @s@. Like 'addInsideEdgeScope', this is one shared
-- per-context scope (allocated by 'newDefinitionContext') applied to
-- every input form of the context before the pre-pass walks it.
--
-- Outside a definition context this is a no-op.
--
-- @since 1.0.0
addOutsideEdgeScope :: Syntax -> Expand Syntax
addOutsideEdgeScope s =
  view expandDefinitionContext >>= \case
    Nothing -> pure s
    Just dc -> pure (syntaxScope Nothing (defctx_outside_edge_scope dc) s)

-- | Flip a macro-introduction or use-site scope on a 'Syntax' object.
-- These scopes are plain scopes per the scope-sets model and Racket's
-- @flip-scope@: they live in the phase-independent scope set and must
-- be flipped phase-independently so that the symmetry argument
-- (add-before, remove-after) holds even when the expanded syntax is
-- later visited at a different phase.
--
-- @since 1.0.0
flipSyntax :: Scope -> Syntax -> Expand Syntax
flipSyntax sc id = pure (syntaxFlipScope Nothing sc id)

-- Expand - Expand Operations --------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expanderReadFile :: Symbol -> Expand Syntax
expanderReadFile s = do
  filepath <- importFilePath s
  result   <- liftIO (runFileReader filepath)
  case result of
    Left  exn -> throwError (ExpandReaderError exn)
    Right stx -> pure stx

-- | TODO: docs
--
-- @since 1.0.0
expanderEval :: SExp -> Expand Datum
expanderEval expr = do
  writeLog (LogEnterEval expr)

  config <- viewEvalConfig
  st0   <- useEvalState

  result <- liftIO (runEvalSExp config st0 expr)

  case result of
    Left  exn        -> throwError (evalToExpandError exn)
    Right (val, st1) -> do
      expandBindingStore .= st1 ^. evalBindingStore
      val <$ writeLog (LogExitEval val)
  where
    useEvalState :: Expand EvalState
    useEvalState = EvalState <$> use expandBindingStore

    viewEvalConfig :: Expand EvalConfig
    viewEvalConfig = do
      env <- use expandEnvironment
      ph  <- view expandCurrentPhase
      pure (EvalConfig env ph)

    evalToExpandError :: EvalError -> ExpandError
    evalToExpandError (EvalNotBound exn) = ExpandNotBound exn

-- | TODO: docs
--
-- @since 1.0.0
expanderParse :: Syntax -> Expand SExp
expanderParse stx = do
  writeLog (LogEnterParse stx)

  phase <- view expandCurrentPhase
  store <- use expandBindingStore

  let config = (ParseConfig store phase)
  result <- liftIO (runParseSyntax config stx)
  case result of
    Left  exn  -> throwError (parseToExpandError exn)
    Right sexp -> do
      writeLog (LogExitParse sexp)
      pure sexp
  where
    parseToExpandError :: ParseError -> ExpandError
    parseToExpandError (ParseAmbiguous e) = ExpandAmbiguous e
    parseToExpandError (ParseBadSyntax e) = ExpandBadSyntax e

-- | TODO: docs
--
-- @since 1.0.0
expandAndParseSyntax :: Syntax -> Expand SExp
expandAndParseSyntax stx = do
  stx' <- expand stx
  expanderParse stx'

-- Expand - Expansion ----------------------------------------------------------

dispatch :: Transformer -> Syntax -> Expand Syntax
dispatch (TfmCore  core)  stx = dispatchCoreForm core stx
dispatch (TfmDatum datum) stx = dispatchTransformer datum stx

dispatchCoreForm :: CoreForm -> Syntax -> Expand Syntax
dispatchCoreForm CoreApp stx = case stx of
  [syntax| (?_ ?stxs ...) |] -> expandApplication stxs
  _ -> throwBadSyntax CoreApp stx
dispatchCoreForm CoreBegin stx = case stx of
  [syntax| (begin ?exprs ...+) |] -> expandBegin exprs
  _ -> throwBadSyntax CoreBegin stx
dispatchCoreForm CoreBeginForSyntax stx = undefined -- FIXME: unimplemented
dispatchCoreForm CoreDefine stx = do
  writeLog (LogEnterCoreForm CoreDefine stx)
  Define id rhs <- matchDefine stx
  binder <- newBinding id
  expandEnvironment %= Environment.insert binder (TfmDatum (DatumStx (identifierToSyntax id)))
  writeLog (LogExitCoreForm CoreDefine stx)
  pure (defineToSyntax (Define id rhs))
dispatchCoreForm CoreExport stx = do
  guardModuleContext stx
  throwBadSyntax CoreExport stx
dispatchCoreForm CoreImport stx = do
  guardModuleContext stx
  undefined
  -- throwBadSyntax CoreImport stx
dispatchCoreForm CoreDefineSyntax stx = do
  guardDefinitionContext stx
  throwBadSyntax CoreDefineSyntax stx
dispatchCoreForm CoreLambda stx = do
  (args, expr) <- matchLambda stx
  expandLambda args expr
dispatchCoreForm CoreLet stx = do
  (bindings, body) <- matchLet stx
  expandLet bindings body
dispatchCoreForm CoreLetRec stx = do
  (transIds, valIds, expr) <- matchLetRec stx
  expandLetRec transIds valIds expr
dispatchCoreForm CoreQuote stx = do
  expr <- matchQuote stx
  expandQuote expr
dispatchCoreForm CoreSyntax stx = do
  expr <- matchQuoteSyntax stx
  expandQuoteSyntax expr
dispatchCoreForm CoreModule stx = do
  mod <- expandModule stx
  pure (moduleToSyntax mod)
dispatchCoreForm CoreModuleBegin stx = do
  guardTopLevelContext stx
  throwBadSyntax CoreModuleBegin stx

dispatchVariable :: Identifier -> Syntax -> Expand Syntax
dispatchVariable id stx = do
  writeLog (LogVariable id)
  applyRenameTransformer id stx

dispatchTransformer :: Datum -> Syntax -> Expand Syntax
dispatchTransformer (DatumLam fun) stx = applyTransformer fun stx
dispatchTransformer (DatumStx fun) stx = case preview syntaxId stx of
  Nothing -> expand [syntax| (?fun ?stx) |]
  Just id -> dispatchVariable id stx
dispatchTransformer val stx = do
  let transformer = datumToSyntax (stx ^. syntaxInfo) val
  expand [syntax| (?transformer ?stx) |]

applyTransformer :: Lambda -> Syntax -> Expand Syntax
applyTransformer t stx = withIntroScope \introScope -> do
  writeLog (LogEnterMacro stx)

  introStx <- flipSyntax introScope stx

  -- Use-site scope: a no-op outside a definition context. Inside one,
  -- attaches a fresh scope phase-independently and records it in the
  -- enclosing DefinitionContext's accumulator so the matching prune
  -- step at define-binders can find it.
  usageStx <- addUseSiteScope introStx

  transformed <- do
    writeLog (LogEnterMacroExpand usageStx)

    -- Construct the SExp directly instead of going through
    -- expanderParse on `(?t:lam ?stx)`. The parser would otherwise
    -- turn a list-shaped `usageStx` into an `SApp` and the evaluator
    -- would try to *call* the macro's input as a function. We want
    -- to pass the input as a *value* — specifically a `DatumStx`
    -- wrapping the syntax object — so the transformer body can
    -- inspect it via `syntax-e`/`syntax->list`/etc.
    let expr = SApp (SVal (DatumLam t) :| [SVal (DatumStx usageStx)])
    result <- expanderEval expr

    case result of
      DatumStx stx' -> do
        writeLog (LogExitMacroExpand usageStx)
        pure stx'
      _ ->
        error ("macro expansion did not return a syntax object")
        -- throwError (ErrorBadSyntax _ usageStx)

  -- Flip the introduction scope back out of the result.
  resultStx <- flipSyntax introScope transformed

  -- Inside-edge scope: stamps the macro output with the enclosing
  -- DefinitionContext's shared scope (not freshly minted). Subsequent
  -- definitions in this context look up against this scope, which is
  -- the Racket semantics from internal-definition-context-inside-edge.
  postStx <- addInsideEdgeScope resultStx

  writeLog (LogExitMacro resultStx)

  pure postStx

applyRenameTransformer :: Identifier -> Syntax -> Expand Syntax
applyRenameTransformer id stx = withIntroScope \introScope ->
  -- Intro scopes are plain scopes: attach phase-independently so the
  -- corresponding flip removes them at every phase.
  let introId = identifierScope Nothing introScope id
   in pure (syntaxTrackOrigin [syntax| ?introId:id |] stx)

expand :: Syntax -> Expand Syntax
expand stx = do
  writeLog (LogVisitSyntax stx)
  case stx of
    [syntax| ?id:id             |] -> expandId id
    [syntax| (?id:id ?stxs ...) |] -> expandIdApplication id stxs
    [syntax| (?stxs ...)        |] -> expandApplication stxs
    _                              -> pure stx

expandId :: Identifier -> Expand Syntax
expandId id = do
  transformer <- lookupEnvironment id
  case transformer of
    TfmCore  core -> throwBadSyntax core [syntax| ?id:id |]
    TfmDatum val
      | is datumLambda val -> dispatchTransformer val [syntax| ?id:id |]
      | otherwise          -> pure (datumToSyntax (id ^. idtInfo) val)

expandIdApplication :: Identifier -> [Syntax] -> Expand Syntax
expandIdApplication id stxs = do
  t <- lookupEnvironment id
  case t of
    TfmDatum (DatumPrim _) -> do
      -- A built-in primitive: this is a runtime function call, not a
      -- macro or core form. Expand the arguments and reassemble the
      -- application form preserving the head identifier. The parser
      -- will later turn this into `SApp (SVar sym :| args)`, and the
      -- evaluator's `DatumPrim` dispatch in 'evalSExp' takes over at
      -- runtime.
      results <- traverse expand stxs
      pure [syntax| (?id:id ?results ...) |]
    _ -> dispatch t [syntax| (?id:id ?stxs ...) |]

expandApplication :: [Syntax] -> Expand Syntax
expandApplication [] = throwBadSyntax CoreApp [syntax| () |]
expandApplication stxs@(f : args) = case f of
  [syntax| ?id:id |] -> expandIdApplication id args
  _  -> do
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
    let id' = identifierScope Nothing sc id
    bind <- newBinding id'
    pure (bind, id')

  withVarTransformers bindings do
    let args   = map snd bindings
        stx'   = syntaxScope Nothing sc expr
    result <- expand stx'
    pure [syntax| (lambda (?args:id ...) ?result) |]

-- | Expansion subroutine for the @let@ core syntactic form.
--
-- Lowers @(let ((id rhs) ...) body)@ to the equivalent immediately-applied
-- lambda @((lambda (id ...) body) rhs ...)@ and delegates the rest of
-- expansion to the existing 'CoreApp' \/ 'CoreLambda' machinery. This
-- choice — over a 'letrec-values' lowering — keeps the output
-- /evaluable/: lambda application is the one form the runtime
-- evaluator handles natively. RHSs are arguments to the application,
-- so they're evaluated in the outer scope and cannot see each other
-- (correct @let@ semantics; @let*@ \/ @letrec@ would differ).
--
-- @since 1.0.0
expandLet ::
  -- | Value bindings extracted from the @let@ form.
  [(Identifier, Syntax)] ->
  -- | The body expression.
  Syntax ->
  Expand Syntax
expandLet bindings body = do
  guardExpressionContext
    let bindingStxs = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) bindings
     in [syntax| (let (?bindingStxs ...) ?body) |]

  let ids  = map fst bindings
      rhss = map snd bindings
      -- Literal identifiers introduced into the quasiquoter at
      -- TH-compile time arrive scope-less. To resolve to the core
      -- form via the imported `#%core`, attach the default scope
      -- explicitly — same trick `moduleToSyntax` uses for the
      -- `module` head.
      lam  = syntaxScope Nothing def [syntax| lambda |]
  expand [syntax| ((?lam (?ids:id ...) ?body) ?rhss ...) |]

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
        vals = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) valExprs
     in [syntax| (letrec-syntaxes+values (?stxs ...) (?vals ...) ?expr) |]

  sc <- newScope

  valBinds <- for valExprs \(valId, valExpr) -> do
    let valId'   = identifierScope Nothing sc valId
        valExpr' = syntaxScope     Nothing sc valExpr
    binder <- newBinding valId'
    pure (valId', valExpr', binder)

  transBinds <- for transExprs \(transId, transExpr) -> do
    let transId'   = identifierScope Nothing sc transId
        transExpr' = syntaxScope     Nothing sc transExpr
    binder <- newBinding transId'

    nextPhase do
      sexp  <- expandAndParseSyntax transExpr'
      value <- expanderEval sexp
      pure (binder, value)

  let letBinds = map (\(idt, _, b) -> (b, idt)) valBinds

  withVarTransformers letBinds do
    withValTransformers transBinds do
      vals <- for valBinds \(valId, valExpr, _) -> do
        result <- expand valExpr
        pure [syntax| (?valId:id ?result) |]

      let scoped = syntaxScope Nothing sc expr
      result <- expand scoped

      pure [syntax| (letrec-values (?vals ...) ?result) |]

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
  -- Intro scopes now live in the Reader, so only the currently
  -- in-flight macro expansions contribute. Siblings (other macro
  -- invocations whose dynamic extent does not enclose this
  -- quote-syntax) can no longer leak their intro scopes here.
  intros <- view expandIntroScopes
  let result = syntaxPrune phase intros expr

  pure [syntax| (quote-syntax ?result) |]

-- Expand - Expansion - Definitions --------------------------------------------

expandBegin :: NonEmpty Syntax -> Expand Syntax
expandBegin stxs = do
  guardExpressionContext [syntax| (begin ?stxs ...+) |]

  -- Enter a single internal-definition context that owns the outside-
  -- and inside-edge scopes plus the use-site box. Both the pre-pass
  -- (preExpandBegin) and the actual expansion pass share this
  -- context, so macros expanded in either see the same use-site
  -- accumulator and edge scopes.
  withDefinitionContext do
    outsideEdgeStxs <- traverse addOutsideEdgeScope stxs

    begin <- preExpandBegin outsideEdgeStxs

    let stx = beginToLetRec begin
    result <- expand stx
    addInsideEdgeScope result

-- | Categorize the forms of a begin body into definitions/expressions.
-- Runs inside the enclosing definition context allocated by the
-- caller (typically 'expandBegin' or a module-body pass); does not
-- introduce its own.
preExpandBegin :: NonEmpty Syntax -> Expand Begin
preExpandBegin stxs = do
  mut <- newMutVar []

  for_ (NonEmpty.init stxs) \stx -> case stx of
    [syntax| (?id:id ?_ ...) |] -> preExpandIdApplication mut id stx
    _ -> modifyMutVar' mut (++ [DefnExpr stx])

  defns <- readMutVar mut
  pure (Begin defns (NonEmpty.last stxs))

preExpandCoreDefinition :: MutVar RealWorld [Definition] -> CoreForm -> Syntax -> Expand ()
preExpandCoreDefinition defns CoreBegin stx = case stx of
  [syntax| (begin ?stxs ...+) |] -> do
    begin <- preExpandBegin stxs
    modifyMutVar' defns (++ beginToDefinitions begin)
  _ -> do
    throwBadSyntax CoreBegin stx
preExpandCoreDefinition defns CoreDefine stx = case stx of
  [syntax| (define ?id:id ?expr) |] -> do
    let defn = Define id expr
    modifyMutVar' defns (++ [DefnDefine defn])
  _ -> do
    throwBadSyntax CoreDefine stx
preExpandCoreDefinition defns CoreDefineSyntax stx = case stx of
  [syntax| (define-syntax ?id:id ?expr) |] -> do
    let defn = DefineSyntax id expr
    modifyMutVar' defns (++ [DefnSyntax defn])
  _ -> do
    throwBadSyntax CoreDefineSyntax stx
preExpandCoreDefinition defns _ stx = do
  modifyMutVar' defns (++ [DefnExpr stx])

preExpandIdApplication :: MutVar RealWorld [Definition] -> Identifier -> Syntax -> Expand ()
preExpandIdApplication defns id stx = do
  transformer <- lookupEnvironment id
  case transformer of
    TfmCore core -> do
      guardDefinitionContext stx
      preExpandCoreDefinition defns core stx
    TfmDatum _ -> do
      modifyMutVar' defns (++ [DefnExpr stx])

-- (Sub)modules ----------------------------------------------------------------

expandModule :: Syntax -> Expand Module
expandModule stx@[syntax| (?f:id ?args ...) |] = do
  writeLog (LogEnterCoreForm CoreModule stx)

  transformer <- lookupEnvironment f

  case transformer of
    TfmCore CoreModule -> case [syntax| (?args ...) |] of
      body@[syntax| (?id:id ?stxs ...) |] -> withModuleBeginContext do
        writeLog (LogEnterCoreForm CoreModuleBegin body)

        (exports, results) <- expandModuleBegin stxs

        writeLog (LogExitCoreForm CoreModuleBegin body)

        ns <- use expandNamespace

        writeLog (LogExitCoreForm CoreModule stx)

        pure (Module (id ^. idtSymbol) def exports ns)
      _ -> do
        throwBadSyntax CoreModule stx
    _ -> do
      throwBadSyntax CoreModule stx
expandModule stx = do
  throwBadSyntax CoreModule stx

expandModuleBegin :: [Syntax] -> Expand (Export, [Syntax])
expandModuleBegin stxs = do
  phase1 <- partialExpandModuleBegin stxs
  phase2 <- expandModuleBeginExprs phase1
  phase3 <- expandModuleExports phase2
  pure phase3

-- | Phase 1 of the @module-begin@ expansion process.
partialExpandModuleBegin :: [Syntax] -> Expand [Syntax]
partialExpandModuleBegin = loop
  where
    loop :: [Syntax] -> Expand [Syntax]
    loop []              = pure []
    loop (body : bodies) = case body of
      stx@[syntax| (?f:id ?args ...) |] -> do
        transformer <- lookupEnvironment f
        case transformer of
          TfmCore CoreBegin -> do
            results <- loop args
            bodies' <- loop bodies
            pure (results ++ bodies')
          TfmCore CoreBeginForSyntax -> do
            undefined
          TfmCore CoreDefine -> do
            Define id rhs <- matchDefine body

            -- Read use-site scopes from the enclosing definition
            -- context's accumulator, not from a global. Siblings
            -- (other macro invocations) can no longer contribute
            -- here.
            dc    <- useDefinitionContext "partialExpandModuleBegin/CoreDefine"
            uscps <- readUseSiteScopes dc
            phase <- view expandCurrentPhase

            let usageId = identifierPrune phase uscps id
            binder <- newBinding usageId
            expandEnvironment %= Environment.insert binder (TfmDatum (DatumStx (identifierToSyntax usageId)))

            bodies' <- loop bodies
            pure (defineToSyntax (Define usageId rhs) : bodies')
          TfmCore CoreDefineSyntax -> do
            DefineSyntax id rhs <- matchDefineSyntax body

            dc    <- useDefinitionContext "partialExpandModuleBegin/CoreDefineSyntax"
            uscps <- readUseSiteScopes dc
            phase <- view expandCurrentPhase

            let usageId = identifierPrune phase uscps id
            binder <- newBinding usageId
            -- The transformer RHS is an expression, not a module-body
            -- form. Switch to an expression context (mirrors how the
            -- `define` branch in `expandModuleBeginExprs` wraps its
            -- RHS in `withExpressionContext`).
            expr <- nextPhase $ withExpressionContext do
              sexp  <- expandAndParseSyntax rhs
              value <- expanderEval sexp
              pure (binder, value)

            -- Register the transformer under its binder gensym in the
            -- compile-time environment so subsequent macro calls (which
            -- resolve the identifier to the binder, then look it up via
            -- `lookupEnvironment`) actually find the transformer.
            -- Parallel to how the `CoreDefine` branch above inserts
            -- into `expandEnvironment`.
            expandEnvironment %= Environment.insert binder (TfmDatum (snd expr))

            expandNamespace . nsTransformer phase (id ^. idtSymbol) .= Just (TfmDatum (snd expr))

            let stxExpr = datumToSyntax (rhs ^. syntaxInfo) (snd expr)

            bodies' <- loop bodies
            pure (defineSyntaxToSyntax (DefineSyntax usageId stxExpr) : bodies')
          TfmCore CoreImport -> do
            i    <- matchImport [syntax| (import ?args ...) |]
            mods <- expandImport i

            for_ mods \mod -> do
              instantiateModule def mod

            pure bodies
          TfmCore CoreExport -> do
            -- Save for final module body expander pass.
            bodies' <- loop bodies
            pure (body : bodies')
          TfmCore CoreModule -> do
            bodies' <- loop bodies
            pure (body : bodies')
          TfmDatum _ -> do
            -- A macro use at the module-body level. Apply the
            -- transformer (which gives us the macro's output syntax)
            -- and re-feed it through this same loop so that, if it
            -- produced a `(define …)` or `(define-syntax …)` form,
            -- it gets registered as a binding. Mirrors Racket's
            -- iterating partial-expand of module bodies.
            expanded <- dispatch transformer stx
            loop (expanded : bodies)
          _ -> do
            -- Other transformer cases (none reachable today). Pass
            -- through to the next pass.
            bodies' <- loop bodies
            pure (body : bodies')
      _ -> do
        -- Not an identifier-headed application; preserve for the
        -- expression pass.
        bodies' <- loop bodies
        pure (body : bodies')

-- | Phase 2 of the @module-begin@ expansion process.
expandModuleBeginExprs :: [Syntax] -> Expand [Syntax]
expandModuleBeginExprs original = loop original
  where
    loop :: [Syntax] -> Expand [Syntax]
    loop []              = pure []
    loop (body : bodies) = do
      result <- case body of
        [syntax| (define ?id:id ?rhs) |] -> do
          ph   <- view expandCurrentPhase
          expr <- withExpressionContext (expand rhs)

          expandNamespace . nsVariable ph (id ^. idtSymbol) .= Just (TfmDatum (DatumStx expr))

          pure (defineToSyntax (Define id expr))
        [syntax| ?stx |] -> pure stx

      bodies' <- loop bodies
      pure (result : bodies')

-- | Phase 3 of the @module-begin@ expansion process.
expandModuleExports :: [Syntax] -> Expand (Export, [Syntax])
expandModuleExports original = do
  (specs, result) <- loop [] original
  pure (Export 0 specs, result)
  where
    loop :: [ExportSpec] -> [Syntax] -> Expand ([ExportSpec], [Syntax])
    loop specs []              = pure (specs, [])
    loop specs (body : bodies) = case body of
      [syntax| (export ?_ ...) |] -> do
          Export _ exs <- matchExport body

          (specs', bodies') <- loop (specs ++ exs) bodies
          pure (specs', body : bodies')
      -- `define` and `define-syntax` were already fully processed by
      -- partialExpandModuleBegin (define-syntax) or
      -- expandModuleBeginExprs (define). Don't re-expand them — that
      -- would route through dispatchCoreForm where both currently
      -- bottom with `throwBadSyntax`.
      [syntax| (define        ?_dId  ?_dRhs) |] -> do
        (specs', bodies') <- loop specs bodies
        pure (specs', body : bodies')
      [syntax| (define-syntax ?_dsId ?_dsRhs) |] -> do
        (specs', bodies') <- loop specs bodies
        pure (specs', body : bodies')
      [syntax| ?stx |] -> do
        body' <- expand stx
        (specs', bodies') <- loop specs bodies
        pure (specs', body' : bodies')

-- Import ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expandImport :: Import -> Expand [Module]
expandImport (Import sh specs) = do
  ph <- view expandCurrentPhase
  let basePhase = ph `phasePlus` sh
  traverse (expandImportSpec basePhase) specs

expandImportSpec :: Phase -> ImportSpec -> Expand Module
expandImportSpec basePhase (ImportSpecPhaseless s) = importModule basePhase s
expandImportSpec basePhase (ImportSpecForSyntax s) = importModule (basePhase `phasePlus` 1) s

-- | TODO: docs
--
-- @since 1.0.0
importModule :: Phase -> Symbol -> Expand Module
importModule ph s
  | s `eqSymbol` "#%core" = do
    let coreModule = newCoreModule def
    instantiateModule def coreModule
    pure coreModule
  | otherwise = do
    filepath <- importFilePath s

    let config :: ExpandConfig
        config = def { expand_file_path = Just filepath, expand_current_phase = ph }

    let state :: ExpandState
        state = def

    (result, logs) <- liftIO $ runExpand config state do
      stx <- expanderReadFile s
      let coreModule = newCoreModule def
      expandNamespace %= declareModule "#%core" coreModule False
      void (importModule def "#%core")
      expandModule (syntaxScope Nothing def stx)

    case result of
      Left  exn      -> throwError exn
      Right (mod, _) -> do
        tell logs
        expandNamespace . nsModuleDeclarations %= Map.insert s mod
        instantiateModule def mod
        pure mod

-- | TODO: docs
--
-- @since 1.0.0
importFilePath :: Symbol -> Expand FilePath
importFilePath s = do
  rootFilePath <- getExpanderPath
  let relativePath = symbolToString s ++ ".opal"
  let rootDirPath  = reverse (dropWhile (/= '/') (reverse rootFilePath))
  pure (rootDirPath ++ relativePath)
  where
    getExpanderPath :: Expand FilePath
    getExpanderPath = do
      result <- view expandFilePath
      case result of
        Nothing       -> liftIO getExecutablePath
        Just filepath -> pure filepath

-- | TODO: docs
--
-- @since 1.0.0
instantiateModule ::
  Phase ->
  -- ^ The base phase to bind the module's exports in.
  Module ->
  -- ^ The module to instantiate.
  Expand ()
instantiateModule basePhase mod = do
  let exports :: [(PhaseShift, Identifier)]
      exports = moduleExportPhaseLevels mod

  liftIO $ print $ exports
  liftIO $ print $ mod

  for_ exports \(sh, id) -> do
    let ph = basePhase `phasePlus` sh

    local (set expandCurrentPhase ph) do
      binder <- newBinding id
      case view (moduleBinding ph (id ^. idtSymbol)) mod of
        Nothing -> throwError (ExpandNotInScope (ErrorNotInScope id))
        Just t  -> expandEnvironment %= Environment.insert binder t