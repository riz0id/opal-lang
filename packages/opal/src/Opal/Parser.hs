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
  , parseLambda
  , parseQuote
  , parseQuoteSyntax
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

import Control.Lens (view)

import Control.Monad.Except (MonadError(..))

import Data.List.NonEmpty (NonEmpty (..))

import Opal.Common.Symbol
import Opal.Parser.Monad
import Opal.Resolve (MonadResolve (..))
import Opal.Syntax
import Opal.Syntax.TH (syntax)
import Opal.Core (CoreForm(..))
import Opal.Error (ErrorBadSyntax(..))

import Prelude hiding (id)

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
parseSyntax [syntax| (?stxs ...) |] = parseApplication stxs
parseSyntax [syntax| ?idt:id     |] = fmap SVar (parseIdentifier idt)
parseSyntax [syntax| ?stx        |] = pure (SVal (syntaxToDatum stx))

-- | TODO: docs
--
-- @since 1.0.0
parseApplication :: [Syntax] -> Parse SExp
parseApplication []           = throwBadSyntax CoreApp [syntax| () |]
parseApplication (stx : stxs) = case stx of
  [syntax| ?id:id |] -> parseIdApplication id stxs
  _                  -> fmap SApp (traverse parseSyntax (stx :| stxs))

-- | TODO: docs
--
-- @since 1.0.0
parseIdApplication :: Identifier -> [Syntax] -> Parse SExp
parseIdApplication id stxs =
  parseIdentifier id >>= \case
    "lambda"       -> parseLambda [syntax| (lambda ?stxs ...) |]
    "quote"        -> parseQuote [syntax| (quote ?stxs ...) |]
    "quote-syntax" -> parseQuoteSyntax [syntax| (quote-syntax ?stxs ...) |]

-- | TODO: docs
--
-- @since 1.0.0
parseLambda :: Syntax -> Parse SExp
parseLambda [syntax| (lambda (?args:id ...) ?body) |] = do
  args' <- traverse parseIdentifier args
  body' <- parseSyntax body
  pure (SVal (DatumLam (Lambda args' body')))
parseLambda stx =
  throwBadSyntax CoreLambda stx

-- | TODO: docs
--
-- @since 1.0.0
parseQuote :: Syntax -> Parse SExp
parseQuote [syntax| (quote ?stx) |] = pure (SVal (syntaxToDatum stx))
parseQuote stx                      = throwBadSyntax CoreQuote stx

-- | TODO: docs
--
-- @since 1.0.0
parseQuoteSyntax :: Syntax -> Parse SExp
parseQuoteSyntax [syntax| (quote-syntax ?stx) |] = pure (SVal (DatumStx stx))
parseQuoteSyntax stx                             = throwBadSyntax CoreSyntax stx

-- | TODO: docs
--
-- @since 1.0.0
parseIdentifier :: Identifier -> Parse Symbol
parseIdentifier id = do
  ph <- view parseCurrentPhase
  resolve ph id
