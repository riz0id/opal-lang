{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Expand.Lexer.Core
  ( -- * TODO
    testLexerCase,

    -- * Re-exports
    module Test.Core,
  )
where

import Hedgehog ((===))

import Opal.Lexer (Error, Lexer, runStringLexer)
import Test.Core

--------------------------------------------------------------------------------

-- | Like 'testProp', but only performs a single test run on the 'Property'.
testLexerCase ::
  (Eq a, Show a) =>
  TestName ->
  String ->
  Lexer a ->
  Either Error a -> 
  TestTree
testLexerCase name input lexer expect =
  testCase name $ property do
    footnote ("lexer input: " ++ show input)
    expect === runStringLexer input lexer