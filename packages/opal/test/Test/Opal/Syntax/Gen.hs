{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Syntax.Gen
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Syntax.Gen
  ( -- * Generators
    sexp
  , datum
  , lambda
  , identifier
  , syntax
  , syntaxInfo
  ) where

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Opal.Syntax
  ( Datum (..)
  , Identifier (..)
  , Lambda (..)
  , SExp (..)
  , Syntax (..)
  , SyntaxInfo (..)
  )

import Test.Opal.Common.Symbol.Gen qualified as Gen
import Test.Opal.Syntax.Value.Gen qualified as Gen

-- Generators ------------------------------------------------------------------

-- | 'SExp' generator.
sexp :: MonadGen m => m SExp
sexp =
  Gen.choice
    [ fmap SVal datum
    , fmap SVar Gen.symbol
    , fmap SApp (Gen.nonEmpty (Range.linear 1 8) sexp)
    ]

-- | 'Datum' generator.
datum :: MonadGen m => m Datum
datum =
  Gen.choice
    [ fmap DatumVal Gen.value
    , fmap DatumLam lambda
    , fmap DatumStx syntax
    ]

-- | 'Lambda' generator.
lambda :: MonadGen m => m Lambda
lambda =
  Lambda
    <$> Gen.list (Range.linear 0 8) Gen.symbol
    <*> sexp

-- | 'Identifier' generator.
identifier :: MonadGen m => m Identifier
identifier =
  Identifier
    <$> Gen.symbol
    <*> syntaxInfo

-- | 'Syntax' generator.
syntax :: MonadGen m => m Syntax
syntax =
  Gen.choice
    [ SyntaxVal
        <$> Gen.value
        <*> syntaxInfo
    , SyntaxLam
        <$> lambda
        <*> syntaxInfo
    , SyntaxList
        <$> Gen.list (Range.linear 0 8) syntax
        <*> syntaxInfo
    ]

-- | 'SyntaxInfo' generator.
syntaxInfo :: MonadGen m => m SyntaxInfo
syntaxInfo =
  SyntaxInfo
    <$> Gen.maybe undefined
    <*> undefined
    <*> undefined
