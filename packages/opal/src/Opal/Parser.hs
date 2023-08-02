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
  )
where

import Control.Lens (view, (^.))

import Data.List.NonEmpty (NonEmpty (..))

import Opal.Common.Symbol
import Opal.Parser.Monad
import Opal.Resolve (resolve)
import Opal.Syntax
import Opal.Syntax.TH (syntax)

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
  throwEmptyAppParseError (stx ^. stxInfo)
parseSyntax stx@[syntax| (?fun:id ?stxs ...) |] = do
  fun' <- parseIdentifier fun
  if| fun' `eqSymbol` "lambda" -> case [syntax| (?stxs ...) |] of
      [syntax| ((?args:id ...) ?body ...+) |] -> do
        args' <- traverse parseIdentifier args
        body' <- traverse parseSyntax body
        pure (SVal (DatumLam (Lambda args' body')))
      _ -> do
        throwLambdaParseError stx
    | fun' `eqSymbol` "quote" -> case [syntax| (?stxs ...) |] of
        [syntax| (?arg) |] -> pure (SVal (syntaxToDatum arg))
        _                  -> throwQuoteSyntaxParseError stx
    | fun' `eqSymbol` "quote-syntax" -> case [syntax| (?stxs ...) |] of
      [syntax| (?arg) |] -> pure (SVal (DatumStx arg))
      _                  -> throwQuoteSyntaxParseError stx
    | otherwise -> do
      stxs' <- traverse parseSyntax stxs
      pure (SApp (SVar (fun ^. idtSymbol) :| stxs'))
parseSyntax [syntax| ?idt:id |] = do
  s <- parseIdentifier idt
  pure (SVar s)
parseSyntax [syntax| ?stx |] = do
  pure (SVal (syntaxToDatum stx))

-- | TODO: docs
--
-- @since 1.0.0
parseIdentifier :: Identifier -> Parse Symbol
parseIdentifier idt = do
  phase <- view parseCurrentPhase
  store <- view parseBindingStore
  case resolve phase idt store of
    Nothing -> pure (idt ^. idtSymbol)
    Just s  -> pure s