{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Expander.Match
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
module Opal.Expander.Match
  ( -- * Syntax Match
    matchDefine
  , matchDefineSyntax
  , matchImport
  , matchExport
  , matchLambda
  , matchLetRec
  , matchQuote
  , matchQuoteSyntax
  )
where

import Control.Lens (view, (^.))

import Opal.Core (CoreForm (..))
import Opal.Expander.Monad (Expand, throwBadSyntax)
import Opal.Module.Import (Import (..), ImportSpec (..))
import Opal.Syntax (Identifier, Syntax, idtSymbol)
import Opal.Syntax.Definition (Define (..), DefineSyntax (..))
import Opal.Syntax.TH (syntax)

import Prelude hiding (id)
import Opal.Module.Export

-- Syntax Match ----------------------------------------------------------------

-- | Syntax match on a @define@ form.
--
-- @since 1.0.0
matchDefine :: Syntax -> Expand Define
matchDefine original
  | [syntax| (define ?id:id ?rhs) |] <- original = pure (Define id rhs)
  | otherwise = throwBadSyntax CoreDefine original

-- | Syntax match on a @define-syntax@ form.
--
-- @since 1.0.0
matchDefineSyntax :: Syntax -> Expand DefineSyntax
matchDefineSyntax original
  | [syntax| (define-syntax ?id:id ?rhs) |] <- original = pure (DefineSyntax id rhs)
  | otherwise = throwBadSyntax CoreDefine original

-- | Syntax match on a @import@ form.
--
-- @since 1.0.0
matchImport :: Syntax -> Expand Import
matchImport original = case original of
  [syntax| (?_ ?args ...) |] -> do
    specs <- traverse matchImportSpec args
    pure (Import 0 (mconcat specs))
  _ -> do
    throwBadSyntax CoreImport original
  where
    matchImportSpec :: Syntax -> Expand [ImportSpec]
    matchImportSpec [syntax| (for-syntax ?ids:id ...) |] = pure (map (ImportSpecForSyntax . view idtSymbol) ids)
    matchImportSpec [syntax| ?id:id                   |] = pure [ImportSpecPhaseless (id ^. idtSymbol)]
    matchImportSpec stx = throwBadSyntax CoreImport stx

-- | Syntax match on a @export@ form.
--
-- @since 1.0.0
matchExport :: Syntax -> Expand Export
matchExport original
  | [syntax| (export ?args ...) |] <- original = do
    specs <- traverse matchImportSpec args
    pure (Export 0 (mconcat specs))
  | otherwise = do
    throwBadSyntax CoreExport original
  where
    matchImportSpec :: Syntax -> Expand [ExportSpec]
    matchImportSpec [syntax| (for-syntax ?ids:id ...) |] = pure (map ExportSpecForSyntax ids)
    matchImportSpec [syntax| ?id:id                   |] = pure [ExportSpecPhaseless id]
    matchImportSpec _ = throwBadSyntax CoreExport original

-- | Syntax match on a @lambda@ form.
--
-- @since 1.0.0
matchLambda :: Syntax -> Expand ([Identifier], Syntax)
matchLambda original
  | [syntax| (lambda (?args:id ...) ?rhs) |] <- original = pure (args, rhs)
  | otherwise = throwBadSyntax CoreDefine original

-- | Syntax match on a @letrec-syntax+values@ form.
--
-- @since 1.0.0
matchLetRec :: Syntax -> Expand ([(Identifier, Syntax)], [(Identifier, Syntax)], Syntax)
matchLetRec original
  | [syntax| (letrec-syntaxes+values (?trans ...) (?vals ...) ?rhs) |] <- original = do
    transIds <- traverse matchBinding trans
    valIds   <- traverse matchBinding vals
    pure (transIds, valIds, rhs)
  | otherwise =
    throwBadSyntax CoreLetRec original
  where
    matchBinding :: Syntax -> Expand (Identifier, Syntax)
    matchBinding stx
      | [syntax| (?id:id ?rhs) |] <- stx = pure (id, rhs)
      | otherwise = throwBadSyntax CoreLetRec original

-- | Syntax match on a @quote@ form.
--
-- @since 1.0.0
matchQuote :: Syntax -> Expand Syntax
matchQuote original
  | [syntax| (quote ?expr) |] <- original = pure expr
  | otherwise = throwBadSyntax CoreDefine original

-- | Syntax match on a @quote-syntax@ form.
--
-- @since 1.0.0
matchQuoteSyntax :: Syntax -> Expand Syntax
matchQuoteSyntax original
  | [syntax| (quote-syntax ?expr) |] <- original = pure expr
  | otherwise = throwBadSyntax CoreDefine original