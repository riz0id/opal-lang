module Test.Common.Gen
  ( -- * Symbols
    symbol,
    genSym,

    -- * Re-exports
    Gen,
    Range,
  )
where

import Control.Applicative (liftA2)

import Hedgehog (Gen, Range)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Opal.Common.GenSym (GenSym (GenSym))
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

-- Scopes ----------------------------------------------------------------------

symbolChar :: Gen Char
symbolChar = Gen.element "abcdefghijklmnopqrstuvwxyz123456789/-!$"

symbol :: Gen Symbol
symbol =
  Gen.sized \size ->
    let range :: Range Int
        range = Range.constant 1 (fromIntegral size)
     in fmap Symbol.pack (Gen.list range symbolChar)

genSym :: Gen GenSym
genSym = liftA2 GenSym symbol (Gen.int (Range.constant 0 maxBound))