{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Core
  ( Property,
    TestTree,
    TestName,

    -- * TODO
    property,
    testGroup,
    testCase,
    testProp,

    -- * TODO
    footnote,
    annotate,
  )
where

import Hedgehog (Property, annotate, footnote, withTests, property)

import Test.Compat (testProp)
import Test.Tasty (TestName, TestTree, testGroup)

--------------------------------------------------------------------------------

-- | Like 'testProp', but only performs a single test run on the 'Property'.
testCase :: TestName -> Property -> TestTree
testCase name prop = testProp name (withTests 1 prop)

