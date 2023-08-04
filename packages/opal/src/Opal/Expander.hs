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
  , runExpandSyntax
    -- ** Expand Operations
  , expandSyntax
  , expandIdentifier
  , expandSyntaxList
    -- * ExpandConfig
  , ExpandConfig (..)
  )
where

import Control.Lens (set, use, view, (^.), (.~), (%~), (%=))

import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..))

import Data.Default (Default (..))
import Data.Function ((&))
import Data.IORef (readIORef)
import Data.HashMap.Strict qualified as HashMap
import Data.Traversable (for)

import Opal.Common.BindingStore (insertBindingStore, coreBindingStore)
import Opal.Common.Scope (MonadScope (..))
import Opal.Common.Symbol (MonadGenSym (..), Symbol)
import Opal.Expander.Monad
import Opal.Resolve (resolve)
import Opal.Syntax
import Opal.Syntax.ScopeInfo qualified as ScopeInfo
import Opal.Syntax.TH

-- Expand - Basic Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
runExpandSyntax :: Syntax -> IO (Either ExpandError (Syntax, ExpandState))
runExpandSyntax stx =
  runExpand expandConfig expandState do
    let stx' = syntaxScope Nothing def stx
    expandSyntax stx'
  where
    expandConfig :: ExpandConfig
    expandConfig = def

    expandState :: ExpandState
    expandState = def & expandBindingStore .~ coreBindingStore

-- Expand - Binding Operations -------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newBinding :: Identifier -> Expand Symbol
newBinding (Identifier s info) = do
  phase  <- view expandCurrentPhase
  binder <- newGenSym
  let scps = ScopeInfo.lookup (Just phase) (info ^. stxInfoScopes)
  expandBindingStore %= insertBindingStore s scps binder
  pure binder

-- Expand - Config Operations --------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
withExpansionContext :: ExpansionContext -> Expand a -> Expand a
withExpansionContext ctx = local (set expandContext ctx)

-- Expand - Expand Operations --------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expandSyntax :: Syntax -> Expand Syntax
expandSyntax [syntax| (?fun:id ?stxs ...) |] = do
  expandSyntaxListId fun stxs
expandSyntax [syntax| (?stxs ...)         |] = do
  liftIO (print (map (^. syntaxInfo) stxs))
  expandSyntaxList stxs
expandSyntax [syntax| ?stx:id             |] = expandIdentifier stx
expandSyntax [syntax| ?stx                |] = pure stx

-- | TODO: docs
--
-- @since 1.0.0
expandIdentifier :: Identifier -> Expand Syntax
expandIdentifier idt = do
  phase <- view expandCurrentPhase
  store <- use expandBindingStore

  case resolve phase idt store of
    Nothing     -> throwError (ErrorNotInScope idt)
    Just binder -> do
      env <- view expandEnvironment
      case HashMap.lookup binder env of
        Nothing  -> throwError (ErrorNotBound idt binder)
        Just tfm -> transformerToId tfm
  where
    transformerToId :: Transformer -> Expand Syntax
    transformerToId TransformerLambda      = pure (identifierToSyntax idt)
    transformerToId TransformerLetSyntax   = pure (identifierToSyntax idt)
    transformerToId TransformerQuote       = pure (identifierToSyntax idt)
    transformerToId TransformerQuoteSyntax = pure (identifierToSyntax idt)
    transformerToId (TransformerVar var)   = expandIdentifier var
    transformerToId (TransformerVal loc)   = do
      val <- liftIO (readIORef loc)
      pure (datumToSyntax (idt ^. idtInfo) val)

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
  expandIdentifier idt >>= \case
    [syntax| lambda       |] -> expandLambda stxs
    [syntax| quote        |] -> expandQuote stxs
    [syntax| quote-syntax |] -> expandQuoteSyntax stxs
    [syntax| ?stx         |] -> do
      estxs <- traverse expandSyntax stxs
      pure [syntax| (?stx ?estxs ...) |]

-- | TODO: docs
--
-- @since 1.0.0
expandLambda :: [Syntax] -> Expand Syntax
expandLambda stxs = case [syntax| (?stxs ...) |] of
  [syntax| ((?args:id ...) ?body ...+) |] -> do
    ph <- view expandCurrentPhase
    sc <- newScope

    bindings <- for args \arg -> do
      let arg' = identifierScope (Just ph) sc arg
      binder <- newBinding arg'
      pure (binder, arg')

    withLambdaBindings bindings do
      withExpansionContext ContextDefinition do
        let eargs = map snd bindings

        ebody <- for body \stx -> do
          let stx' = syntaxScope (Just ph) sc stx
          expandSyntax stx'

        pure [syntax| (lambda (?eargs:id ...) ?ebody ...+) |]
  stx -> do
    throwError (ErrorBadSyntax stx)
  where
    withLambdaBindings :: [(Symbol, Identifier)] -> Expand a -> Expand a
    withLambdaBindings bindings =
      local \config ->
        config & expandEnvironment %~ \env ->
          foldr (\(s, idt) -> HashMap.insert s (TransformerVar idt)) env bindings

-- | TODO: docs
--
-- @since 1.0.0
expandQuote :: [Syntax] -> Expand Syntax
expandQuote stxs = case [syntax| (?stxs ...) |] of
  [syntax| (?stx) |] -> pure [syntax| (quote ?stx) |]
  stx                -> throwError (ErrorBadSyntax stx)

-- | TODO: docs
--
-- @since 1.0.0
expandQuoteSyntax :: [Syntax] -> Expand Syntax
expandQuoteSyntax stxs = case [syntax| (?stxs ...) |] of
  [syntax| (?stx) |] -> do
    phase  <- view expandCurrentPhase
    intros <- use expandIntroScopes
    let pstx = syntaxPrune phase intros stx
    pure [syntax| (quote-syntax ?pstx) |]
  stx ->
    throwError (ErrorBadSyntax stx)