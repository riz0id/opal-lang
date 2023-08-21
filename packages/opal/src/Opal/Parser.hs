{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Parser
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
module Opal.Parser
  ( -- * Parse
    Parse (..)
    -- ** Basic Operations
  , runParse
  , runParseSyntax
    -- ** Parse Operations
  , parseSyntax
  , parseIdentifier
    -- * ParseConfig
  , ParseConfig (..)
    -- ** Lenses
  , parseBindingStore
  , parseCurrentPhase
    -- * ParseError
  , ParseError (..)
  )
where

import Control.Lens (view, (^.))

import Control.Monad.Except (MonadError(..))

import Data.List.NonEmpty (NonEmpty (..))

import Opal.Common.Symbol
import Opal.Parser.Monad
import Opal.Resolve (ResolveError (..), resolve)
import Opal.Syntax
import Opal.Syntax.TH (syntax)
import Opal.Core (CoreForm(..))
import Opal.Error (ErrorBadSyntax(..))

-- Parse - Basic Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
runParseSyntax :: ParseConfig -> Syntax -> IO (Either ParseError SExp)
runParseSyntax c = runParse c . parseSyntax

-- Parse - Parse Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
parseSyntax :: Syntax -> Parse SExp
parseSyntax stx@[syntax| () |] = do
  throwError (ParseBadSyntax (ErrorBadSyntax CoreApp stx))
parseSyntax stx@[syntax| (?fun:id ?stxs ...) |] = do
  fun' <- parseIdentifier fun
  if| fun' `eqSymbol` "lambda" -> case [syntax| (?stxs ...) |] of
      [syntax| ((?args:id ...) ?body) |] -> parseLambda args body
      _                                  -> throwError (ParseBadSyntax (ErrorBadSyntax CoreLambda stx))
    | fun' `eqSymbol` "quote" -> case [syntax| (?stxs ...) |] of
        [syntax| (?arg) |] -> pure (SVal (syntaxToDatum arg))
        _                  -> throwError (ParseBadSyntax (ErrorBadSyntax CoreQuote stx))
    | fun' `eqSymbol` "quote-syntax" -> case [syntax| (?stxs ...) |] of
      [syntax| (?arg) |] -> pure (SVal (DatumStx arg))
      _                  -> throwError (ParseBadSyntax (ErrorBadSyntax CoreSyntax stx))
    | otherwise -> do
      idt'  <- parseIdentifier fun
      stxs' <- traverse parseSyntax stxs
      pure (SApp (SVar idt' :| stxs'))
parseSyntax [syntax| (?stxs ...+) |] = do
  stxs' <- traverse parseSyntax stxs
  pure (SApp stxs')
parseSyntax [syntax| ?idt:id |] = do
  s <- parseIdentifier idt
  pure (SVar s)
parseSyntax [syntax| ?stx |] = do
  pure (SVal (syntaxToDatum stx))

-- | TODO: docs
--
-- @since 1.0.0
parseLambda :: [Identifier] -> Syntax -> Parse SExp
parseLambda args body = do
  args' <- traverse parseIdentifier args
  body' <- parseSyntax body
  pure (SVal (DatumLam (Lambda args' body')))

-- | TODO: docs
--
-- @since 1.0.0
parseIdentifier :: Identifier -> Parse Symbol
parseIdentifier idt = do
  phase <- view parseCurrentPhase
  store <- view parseBindingStore
  case resolve phase idt store of
    Left  exn     -> case exn of
      ResolveErrorAmbiguous  x -> throwError (ParseAmbiguous x)
      ResolveErrorNotInScope _ -> pure (idt ^. idtSymbol)
    Right gensym  -> pure gensym
