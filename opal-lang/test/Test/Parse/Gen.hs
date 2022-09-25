{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Parse.Gen
  ( -- * TODO
    except,
    string,
    string'unicode,
    Gen.unicode,

    -- * Re-exports
    module Test.Core,
  )
where

import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Hedgehog (Gen, Range)

-- import Opal.Lexer (Error, Lexer, runStringLexer)
import Test.Core

--------------------------------------------------------------------------------

except :: Eq a => a -> Gen a -> Gen a
except x gen = do 
  x' <- gen
  if x == x'
    then Gen.discard
    else pure x'

string :: Gen Char -> Gen String 
string gen = 
  Gen.sized \size -> 
    let range :: Range Int
        range = Range.constant 0 (fromIntegral size)
     in Gen.string range gen

string'unicode :: Gen String 
string'unicode = string Gen.unicode

-- | Like 'testProp', but only performs a single test run on the 'Property'.
-- testLexerCase ::
--   (Eq a, Show a) =>
--   TestName ->
--   String ->
--   Lexer a ->
--   Either Error a -> 
--   TestTree
-- testLexerCase name input lexer expect =
--   testCase name $ property do
--     footnote ("lexer input: " ++ show input)
--     expect === runStringLexer input lexer