{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Laws.Optics
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Laws.Optics
  ( -- * Lens Laws
    lensLaws
  , lensLawInjective
  , lensLawSurjective
  ) where

import Control.Lens (Lens', view, set)

import Hedgehog (Gen, annotate, evalIO, forAll, (===))

import Test.Core (testCase)
import Test.Tasty (TestTree, testGroup)
import Control.Exception (evaluate)

-- Lens Laws -------------------------------------------------------------------

-- | TODO: docs
lensLaws ::
  (Eq s, Eq x, Show s, Show x) =>
  -- | The name of the lens.
  String ->
  -- | Generator for the "outer" structure.
  Gen s ->
  -- | Generator for the "inner" structure.
  Gen x ->
  -- | The lens to test.
  Lens' s x ->
  -- | The unit test.
  TestTree
lensLaws lensName genS genX l =
  testGroup lensName
    [ lensLawInjective lensName genS genX l
    , lensLawSurjective lensName genS l
    , lensLawIdempotent lensName genS genX l
    ]

-- | TODO: docs
lensLawInjective ::
  (Eq s, Eq x, Show s, Show x) =>
  -- | The name of the lens.
  String ->
  -- | Generator for the "outer" structure.
  Gen s ->
  -- | Generator for the "inner" structure.
  Gen x ->
  -- | The lens to test.
  Lens' s x ->
  -- | The unit test.
  TestTree
lensLawInjective lensName genS genX l = testCase testName do
  s  <- forAll genS
  x1 <- forAll genX
  x2 <- evalIO (evaluate (view l (set l x1 s)))
  annotate ("retrieved value: " ++ show x2)
  x1 === x2
  where
    testName :: String
    testName = "∀ s x. view " ++ lensName ++ " (set " ++ lensName ++ " x s) ≡ x"

-- | TODO: docs
lensLawSurjective ::
  (Eq s, Eq x, Show s, Show x) =>
  -- | The name of the lens.
  String ->
  -- | Generator for the "outer" structure.
  Gen s ->
  -- | The lens to test.
  Lens' s x ->
  -- | The unit test.
  TestTree
lensLawSurjective lensName genS l = testCase testName do
  s1 <- forAll genS
  s2 <- evalIO (evaluate (set l (view l s1) s1))
  annotate ("retrieved structure: " ++ show s2)
  s1 === s2
  where
    testName :: String
    testName = "∀ s x. set " ++ lensName ++ " (view " ++ lensName ++ " s) s ≡ s"

-- | TODO: docs
lensLawIdempotent ::
  (Eq s, Eq x, Show s, Show x) =>
  -- | The name of the lens.
  String ->
  -- | Generator for the "outer" structure.
  Gen s ->
  -- | Generator for the "inner" structure.
  Gen x ->
  -- | The lens to test.
  Lens' s x ->
  -- | The unit test.
  TestTree
lensLawIdempotent lensName genS genX l = testCase testName do
  s  <- forAll genS
  x  <- forAll genX
  r1 <- evalIO (evaluate (set l  x s))
  r2 <- evalIO (evaluate (set l x (set l x s)))
  annotate ("structure #1 (lhs): " ++ show r1)
  annotate ("structure #2 (rhs): " ++ show r2)
  r1 === r2
  where
    testName :: String
    testName = "∀ s x. set " ++ lensName ++ " x (set " ++ lensName ++ " x s) ≡ set " ++ lensName ++ " x s"