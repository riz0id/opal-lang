{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
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
    -- ** Expand Operations
  , expandSyntax
  , expandId
  , expandSyntaxList
    -- * ExpandConfig
  , ExpandConfig (..)
  )
where

import Control.Lens (over, use, view, (^.), (%~), (%=))

import Control.Monad (unless, foldM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..))

import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.IORef (readIORef, newIORef)
import Data.Traversable (for)

import Opal.Binding (Binding(..))
import Opal.Binding.BindingStore qualified as BindingStore
import Opal.Common.Phase (Phase, phasePlus)
import Opal.Common.Scope (MonadScope (..), Scope)
import Opal.Common.Symbol (MonadGenSym (..), Symbol)
import Opal.Expander.Monad
import Opal.Module
import Opal.Parser (ParseConfig (..), ParseError (..), runParseSyntax, CoreParseError (..))
import Opal.Reader (runFileReader)
import Opal.Syntax
import Opal.Syntax.Definition
import Opal.Syntax.ScopeInfo qualified as ScopeInfo
import Opal.Syntax.TH (syntax)
import Opal.Syntax.Transformer
import Opal.Writer (Display (..), putDocLn)

import Prelude hiding (id)

import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)
import Opal.Binding.Environment (Environment)
import qualified Opal.Binding.Environment as Environment

-- Expand - Basic Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
runExpandFile :: FilePath -> IO Module
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
runExpandSyntax :: Syntax -> IO (Module, ExpandState)
runExpandSyntax stx = do
  let stx' = syntaxScope Nothing def stx
  runExpand def def (expandTopLevel stx') >>= \case
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
  runExpand def def (expandAndParseSyntax stx') >>= \case
    Left  exn -> do
      putDocLn 80 (display exn)
      exitFailure
    Right rx  -> pure rx

-- Expand - Config Operations --------------------------------------------------

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
withTransformers transExprs = local (over expandEnvironment update)
  where
    update :: Environment -> Environment
    update env = foldr (uncurry Environment.insert) env transExprs

-- | TODO: docs
--
-- @since 1.0.0
withVarTransformers :: [(Symbol, Identifier)] -> Expand a -> Expand a
withVarTransformers = withTransformers . map (fmap TfmVar)

-- | TODO: docs
--
-- @since 1.0.0
withValTransformers :: [(Symbol, Datum)] -> Expand a -> Expand a
withValTransformers binds expand = do
  binds' <- for binds \(id, val) -> do
    ref <- liftIO (newIORef val)
    pure (id, TfmVal ref)

  withTransformers binds' expand

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

-- Expand - Expand Operations --------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expandAndParseSyntax :: Syntax -> Expand SExp
expandAndParseSyntax stx = do
  stx'   <- expandSyntax stx
  config <- useParseConfig
  result <- liftIO (runParseSyntax config stx')
  case result of
    Left  exn  -> throwError (parseToExpandError exn)
    Right sexp -> pure sexp
  where
    useParseConfig :: Expand ParseConfig
    useParseConfig = do
      phase <- view expandCurrentPhase
      store <- use expandBindingStore
      pure (ParseConfig store phase)

    parseToExpandError :: ParseError -> ExpandError
    parseToExpandError (ParseErrorAmbiguous x)    = ExpandErrorAmbiguous x
    parseToExpandError (ParseErrorEmptyApp  info) = ErrorBadSyntax (SyntaxList [] info)
    parseToExpandError (ParseErrorCore      exn)  = case exn of
      ParseErrorLambda x -> ErrorBadSyntax x
      ParseErrorQuote  x -> ErrorBadSyntax x
      ParseErrorSyntax x -> ErrorBadSyntax x

-- | TODO: docs
--
-- @since 1.0.0
expandSyntax :: Syntax -> Expand Syntax
expandSyntax [syntax| (?fun:id ?stxs ...) |] = expandSyntaxListId fun stxs
expandSyntax [syntax| (?stxs ...)         |] = expandSyntaxList stxs
expandSyntax [syntax| ?stx:id             |] = expandId stx
expandSyntax [syntax| ?stx                |] = pure stx

-- | TODO: docs
--
-- @since 1.0.0
expandId :: Identifier -> Expand Syntax
expandId id = do
  tfm <- lookupEnvironment id
  transformerToId tfm
  where
    transformerToId :: Transformer -> Expand Syntax
    transformerToId (TfmVar var) = pure (identifierToSyntax var)
    transformerToId (TfmVal loc) = do
      val <- liftIO (readIORef loc)
      pure (datumToSyntax (id ^. idtInfo) val)
    transformerToId _ = pure (identifierToSyntax id)

-- | TODO: docs
--
-- @since 1.0.0
expandSyntaxList :: [Syntax] -> Expand Syntax
expandSyntaxList stxs = do
  estxs <- traverse expandSyntax stxs
  pure [syntax| (?estxs ...) |]

-- | TODO: docs
--
-- @since 1.0.0
expandSyntaxListId :: Identifier -> [Syntax] -> Expand Syntax
expandSyntaxListId idt stxs = do
  expandId idt >>= \case
    [syntax| begin                  |] -> expandBegin stxs
    [syntax| lambda                 |] -> expandLambda stxs
    [syntax| letrec-syntaxes+values |] -> case [syntax| (?stxs ...) |] of
      stx@[syntax| ((?transBinds ...) (?valBinds ...) ?body) |] -> do
        transBinds' <- for transBinds \case
          [syntax| (?transId:id ?transExpr) |] -> pure (transId, transExpr)
          _ -> throwError (ErrorBadSyntax stx)

        valBinds' <- for valBinds \case
          [syntax| (?valId:id ?valExpr) |] -> pure (valId, valExpr)
          _ -> throwError (ErrorBadSyntax stx)

        expandLetRec transBinds' valBinds' body
      stx -> throwError (ErrorBadSyntax stx)
    [syntax| quote                  |] -> expandQuote stxs
    [syntax| quote-syntax           |] -> expandQuoteSyntax stxs
    [syntax| ?stx                   |] -> do
      estxs <- traverse expandSyntax stxs
      pure [syntax| (?stx ?estxs ...) |]


-- | TODO: docs
--
-- @since 1.0.0
expandLambda :: [Syntax] -> Expand Syntax
expandLambda stxs = do
  guardExpressionContext [syntax| (lambda ?stxs ...) |]

  case [syntax| (?stxs ...) |] of
    [syntax| ((?args:id ...) ?body) |] -> do
      sc <- newScope

      bindings <- for args \arg -> do
        arg' <- scopeId True sc arg
        bind <- newBinding arg'
        pure (bind, arg')

      withVarTransformers bindings do
        let eargs = map snd bindings
        stx'  <- scopeSyntax True sc body
        ebody <- expandSyntax stx'
        pure [syntax| (lambda (?eargs:id ...) ?ebody) |]
    stx -> do
      throwError (ErrorBadSyntax stx)

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
expandLetRec transExprs valExprs body = do
  guardExpressionContext
    let stxs = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) transExprs
        vals = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) transExprs
     in [syntax| (letrec-syntaxes+values (?stxs ...) (?vals ...) ?body) |]

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
    expr   <- nextPhase (expandAndParseSyntax transExpr')

    case expr of
      SVal val -> pure (binder, val)
      _        -> throwError (ErrorBadSyntax transExpr')

  let letBinds = map (\(idt, _, b) -> (b, idt)) valBinds

  withVarTransformers letBinds do
    withValTransformers transBinds do
      vals <- for valBinds \(valId, valExpr, _) -> do
        expr <- expandSyntax valExpr
        pure [syntax| (?valId:id ?expr) |]

      pure [syntax| (letrec (?vals ...) ?body) |]

-- | TODO: docs
--
-- @since 1.0.0
expandQuote :: [Syntax] -> Expand Syntax
expandQuote stxs = do
  guardExpressionContext [syntax| (quote ?stxs ...) |]
  case [syntax| (?stxs ...) |] of
    [syntax| (?stx) |] -> pure [syntax| (quote ?stx) |]
    stx                -> throwError (ErrorBadSyntax stx)

-- | TODO: docs
--
-- @since 1.0.0
expandQuoteSyntax :: [Syntax] -> Expand Syntax
expandQuoteSyntax stxs = do
  guardExpressionContext [syntax| (quote-syntax ?stxs ...) |]

  case [syntax| (?stxs ...) |] of
    [syntax| (?stx) |] -> do
      phase  <- view expandCurrentPhase
      intros <- use expandIntroScopes
      let pstx = syntaxPrune phase intros stx
      pure [syntax| (quote-syntax ?pstx) |]
    stx ->
      throwError (ErrorBadSyntax stx)

-- Expansion - Top-Level -------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expandTopLevel :: Syntax -> Expand Module
expandTopLevel [syntax| (?fun:id ?stxs ...) |] = do
  expandId fun >>= \case
    [syntax| module |] -> expandModule stxs
    other -> do
      throwError (ErrorBadSyntax [syntax| (?other ?stxs ...) |])
expandTopLevel stx =
  throwError (ErrorBadSyntax stx)

-- Expansion - Modules ---------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expandModule :: [Syntax] -> Expand Module
expandModule stxs = case [syntax| (?stxs ...) |] of
  [syntax| (?name:id ?defns ...) |] -> do
    expandModuleBegin defns

    Module (ModuleName (name ^. idtSymbol))
      <$> expandModuleImports defns
      <*> expandModuleExports defns
      <*> use expandNamespace
  stx -> do
    throwError (ErrorBadSyntax stx)

-- | TODO: docs
--
-- @since 1.0.0
expandModuleImports :: [Syntax] -> Expand [(Phase, Symbol)]
expandModuleImports = foldM run []
  where
    run :: [(Phase, Symbol)] -> Syntax -> Expand [(Phase, Symbol)]
    run imports stx = case stx of
      [syntax| (?id:id ?stxs ...) |] -> do
        ide <- expandId id
        case [syntax| (?ide ?stxs ...) |] of
          [syntax| (import ?paths:id ...) |] -> do
              ph <- view expandCurrentPhase
              pure (imports ++ map (\path -> (ph, path ^. idtSymbol)) paths)
          [syntax| (import _ ...) |] ->
            throwError (ErrorBadSyntax stx)
          _ -> pure imports
      _ ->
        pure imports

-- | TODO: docs
--
-- @since 1.0.0
expandModuleExports :: [Syntax] -> Expand [(Phase, Symbol)]
expandModuleExports = foldM run []
  where
    run :: [(Phase, Symbol)] -> Syntax -> Expand [(Phase, Symbol)]
    run exports stx = case stx of
      [syntax| (?id:id ?stxs ...) |] -> do
        ide <- expandId id
        case [syntax| (?ide ?stxs ...) |] of
          [syntax| (export ?paths:id ...) |] -> do
              ph <- view expandCurrentPhase
              pure (exports ++ map (\path -> (ph, path ^. idtSymbol)) paths)
          [syntax| (export _ ...) |] ->
            throwError (ErrorBadSyntax stx)
          _ -> pure exports
      _ ->
        pure exports

-- | TODO: docs
--
-- @since 1.0.0
expandModuleBegin :: [Syntax] -> Expand ()
expandModuleBegin stxs = withModuleBeginContext do
  ph <- view expandCurrentPhase
  for_ stxs \case
    [syntax| (?fun:id ?args ...) |] -> do
      expandId fun >>= \case
        [syntax| import |] -> pure ()
        [syntax| export |] -> pure ()
        [syntax| ?id:id |] -> do
          defn <- partialExpandDefinition [syntax| (?id:id ?args ...) |]
          expandNamespace . namespaceDefinitions %= mappend [(ph, defn)]
        stx -> do
          throwError (ErrorBadSyntax stx)
    stx -> do
      throwError (ErrorBadSyntax stx)

-- Expansion - Definitions -----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
partialExpandDefinition :: Syntax -> Expand Definition
partialExpandDefinition stx = case stx of
  [syntax| (?fun:id ?args ...) |] -> withDefinitionContext do
    expandId fun >>= \case
      [syntax| begin         |] -> do
        partialExpandBegin args (pure . DefnBegin)
      [syntax| define        |] -> do
        defn <- partialExpandDefine stx
        pure (DefnDefine defn)
      [syntax| define-syntax |] -> do
        defn <- partialExpandDefineSyntax stx
        pure (DefnDefineSyntax defn)
      _ -> pure (DefnSyntax stx)
  _ -> pure (DefnSyntax stx)

-- Expansion - Definitions - Begin ---------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
-- expandBegin :: [Syntax] -> Expand Syntax
-- expandBegin stxs = do
--   guardDefinitionContext [syntax| (begin ?stxs ...) |]

--   partialExpandBegin stxs \begin -> do
--     defns <- for (begin ^. beginDefns) \case
--       DefnBegin defn -> liftIO do
--         putStrLn ("Internal error: nested begin did not splice")
--         putDocLn 80 (display defn)
--         exitFailure
--       DefnDefine (Define id stx) -> do
--         stx' <- expandSyntax stx
--         pure (DefnDefine (Define id stx'))
--       DefnDefineSyntax (DefineSyntax id stx) -> do
--         pure (DefnDefineSyntax (DefineSyntax id stx))
--       DefnSyntax stx -> do
--         stx' <- expandSyntax stx
--         pure (DefnSyntax stx')

--     final <- expandSyntax (begin ^. beginFinal)

--     pure (beginToSyntax (Begin (SyntaxBody defns final)))


-- | TODO: docs
--
-- @since 1.0.0
-- partialExpandBegin :: forall a. [Syntax] -> (Begin -> Expand a) -> Expand a
-- partialExpandBegin stxs0 k = do
--   guardDefinitionContext [syntax| (begin ?stxs0 ...) |]
--   case [syntax| (?stxs0 ...) |] of
--     [syntax| (?stxs ...+) |] -> run [] stxs
--     stx                      -> throwError (ErrorBadSyntax stx)
--   where
--     run :: [Definition] -> NonEmpty Syntax -> Expand a
--     run defns (stx :| stxs) = case NonEmpty.nonEmpty stxs of
--       Nothing ->
--         k (Begin (SyntaxBody defns stx))
--       Just stxs' -> partialExpandDefinition stx >>= \case
--         DefnBegin defn ->
--           run (defns ++ beginToDefinitions defn) stxs'
--         DefnDefine defn@(Define id _) -> do
--           binder <- newBinding id
--           withVarTransformers [(binder, id)] do
--             run (defns ++ [DefnDefine defn]) stxs'
--         DefnDefineSyntax defn@(DefineSyntax id stx') -> do
--           expr <- nextPhase (expandAndParseSyntax stx')
--           case expr of
--             SVal val -> do
--               binder <- newBinding id
--               withValTransformers [(binder, val)] do
--                 run (defns ++ [DefnDefineSyntax defn]) stxs'
--             _ -> do
--               let stxDefn = defineSyntaxToSyntax defn
--               throwError (ErrorBadSyntax stxDefn)
--         defn ->
--           run (defns ++ [defn]) stxs'

-- Expansion - Definitions - Define --------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
partialExpandDefine :: Syntax -> Expand Define
partialExpandDefine stx0 = do
  guardDefinitionContext stx0

  case stx0 of
    [syntax| (?id:id ?stx) |] -> pure (Define id stx)
    _                         -> throwError (ErrorBadSyntax stx0)

-- Expansion - Definitions - DefineSyntax --------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
partialExpandDefineSyntax :: Syntax -> Expand DefineSyntax
partialExpandDefineSyntax stx0 = do
  guardDefinitionContext stx0

  case stx0 of
    [syntax| (?id:id ?stx) |] -> pure (DefineSyntax id stx)
    _                         -> throwError (ErrorBadSyntax stx0)

-- Expansion -- Definitions - Begin --------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expandDefinition :: Syntax -> Expand Definition
expandDefinition stx = do
  guardDefinitionContext stx

  case stx of
    [syntax| (?id:id ?stxs ...) |] -> do
      binder  <- resolveId id
      env
      _
    _ -> pure (DefnSyntax stx)

-- | TODO: docs
--
-- @since 1.0.0
expandBegin :: Syntax -> Expand Begin
expandBegin stx = do
  guardExpressionContext stx

  case _ of
    [syntax| (?id:id ?stxs ...) |] -> do
      _
    _ -> throwError (ErrorBadSyntax stx)

-- | TODO: docs
--
-- @since 1.0.0
partialExpandBegin :: Syntax -> Expand Begin
partialExpandBegin stx = _