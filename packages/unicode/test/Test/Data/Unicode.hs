{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Data.Unicode
-- Copyright   :  (c) Jacob Leach, 2026
-- License     :  ISC, see LICENSE
--
-- Round-trip and validation tests for @Data.Unicode@. Covers:
--
-- * Round-trips: encode then decode any 'Char' via the appropriate
--   width path; result equals the original.
-- * Boundary code points at each width transition.
-- * Rejection of malformed continuation bytes.
-- * Rejection of overlong encodings.
-- * Rejection of surrogate-range code points.
-- * Rejection of out-of-Unicode-range code points.
-- * @readUtf8OffPtr@ honours its bounds argument.
module Test.Data.Unicode
  ( testTree
  )
where

import Control.Monad.IO.Class (liftIO)

import Data.Primitive.ByteArray (newPinnedByteArray, mutableByteArrayContents)
import Data.Unicode

import Foreign.Ptr (Ptr)

import GHC.Word (Word8)

import Hedgehog (PropertyT, forAll, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (PropertyName (..), property, withTests)

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

--------------------------------------------------------------------------------

testCase :: TestName -> PropertyT IO () -> TestTree
testCase name = testPropertyNamed name (PropertyName name) . property

testUnit :: TestName -> PropertyT IO () -> TestTree
testUnit name = testPropertyNamed name (PropertyName name) . withTests 1 . property

testTree :: TestTree
testTree =
  testGroup "Data.Unicode"
    [ roundTrip
    , boundaries
    , malformed
    , bounds
    ]

-- | Allocate a pinned byte array and return a pointer to its contents.
-- The array is reachable for the duration of the IO action via the
-- @Ptr@; outside of `k`, the array may be garbage-collected.
withBuffer :: Int -> (Ptr Word8 -> IO a) -> IO a
withBuffer n k = do
  mba <- newPinnedByteArray n
  k (mutableByteArrayContents mba)

--------------------------------------------------------------------------------

roundTrip :: TestTree
roundTrip =
  testGroup "round-trip"
    [ testCase "any Char -> encode -> decode -> same Char" do
        c <- forAll Gen.unicode
        let width = sizeofCharUtf8 c
        result <- liftIO $ withBuffer 4 \ptr -> do
          mp <- writeUtf8OffPtr ptr 4 c
          case mp of
            Nothing -> pure (Left "writeUtf8OffPtr returned Nothing in a 4-byte buffer")
            Just _  -> readUtf8OffPtr ptr 4 >>= \case
              Nothing     -> pure (Left "readUtf8OffPtr returned Nothing on freshly-written bytes")
              Just (c', n) -> pure (Right (c', n))
        case result of
          Left err          -> Hedgehog.failure >> Hedgehog.annotate err
          Right (c', n)     -> do
            c' === c
            n  === width
    ]

--------------------------------------------------------------------------------

boundaries :: TestTree
boundaries =
  testGroup "boundary code points"
    [ testUnit "U+007F = 1-byte" $ checkEncode '\x007F' [0x7F]
    , testUnit "U+0080 = 2-byte" $ checkEncode '\x0080' [0xC2, 0x80]
    , testUnit "U+07FF = 2-byte" $ checkEncode '\x07FF' [0xDF, 0xBF]
    , testUnit "U+0800 = 3-byte" $ checkEncode '\x0800' [0xE0, 0xA0, 0x80]
    , testUnit "U+FFFF = 3-byte" $ checkEncode '\xFFFF' [0xEF, 0xBF, 0xBF]
    , testUnit "U+10000 = 4-byte" $ checkEncode '\x10000' [0xF0, 0x90, 0x80, 0x80]
    , testUnit "U+10FFFF = 4-byte" $ checkEncode '\x10FFFF' [0xF4, 0x8F, 0xBF, 0xBF]
    ]
  where
    checkEncode :: Char -> [Word8] -> PropertyT IO ()
    checkEncode c expected = do
      sizeofCharUtf8 c === length expected
      case length expected of
        1 -> ord1 c === head expected
        2 -> let (a, b) = ord2 c in [a, b] === expected
        3 -> let (a, b, d) = ord3 c in [a, b, d] === expected
        4 -> let (a, b, d, e) = ord4 c in [a, b, d, e] === expected
        _ -> Hedgehog.failure

--------------------------------------------------------------------------------

malformed :: TestTree
malformed =
  testGroup "rejects malformed input"
    [ testUnit "chr1 rejects bytes >= 0x80" do
        chr1 0x80 === Nothing
        chr1 0xC2 === Nothing
        chr1 0xFF === Nothing

    , testUnit "chr2 rejects bad continuation" do
        -- 0xC3 is a 2-byte leader; 0x29 is not a valid continuation
        -- (high bits are 00, not 10).
        chr2 0xC3 0x29 === Nothing

    , testUnit "chr2 rejects overlong" do
        -- 0xC0 0x80 would decode to U+0000 via the 2-byte path —
        -- overlong (U+0000 must be encoded as one byte).
        chr2 0xC0 0x80 === Nothing
        -- 0xC1 0xBF would decode to U+007F overlong.
        chr2 0xC1 0xBF === Nothing

    , testUnit "chr3 rejects bad continuation" do
        -- 0xE0 0x29 0x80: middle byte's high bits are 00.
        chr3 0xE0 0x29 0x80 === Nothing
        chr3 0xE0 0xA0 0x29 === Nothing

    , testUnit "chr3 rejects overlong" do
        -- 0xE0 0x80 0x80 decodes to U+0000 via 3 bytes — overlong.
        chr3 0xE0 0x80 0x80 === Nothing
        chr3 0xE0 0x9F 0xBF === Nothing

    , testUnit "chr3 rejects surrogates" do
        -- 0xED 0xA0 0x80 decodes to U+D800 (high surrogate).
        chr3 0xED 0xA0 0x80 === Nothing
        -- 0xED 0xBF 0xBF decodes to U+DFFF (low surrogate).
        chr3 0xED 0xBF 0xBF === Nothing

    , testUnit "chr4 rejects bad continuation" do
        chr4 0xF0 0x29 0x80 0x80 === Nothing
        chr4 0xF0 0x90 0x29 0x80 === Nothing
        chr4 0xF0 0x90 0x80 0x29 === Nothing

    , testUnit "chr4 rejects overlong" do
        -- 0xF0 0x80 0x80 0x80 would decode to U+0000 overlong.
        chr4 0xF0 0x80 0x80 0x80 === Nothing
        chr4 0xF0 0x8F 0xBF 0xBF === Nothing

    , testUnit "chr4 rejects out-of-Unicode-range" do
        -- 0xF4 0x90 0x80 0x80 decodes to U+110000 — past U+10FFFF.
        chr4 0xF4 0x90 0x80 0x80 === Nothing
    ]

--------------------------------------------------------------------------------

bounds :: TestTree
bounds =
  testGroup "readUtf8OffPtr bounds check"
    [ testCase "Nothing when remaining < needed bytes" do
        c <- forAll (Gen.filter (\c -> sizeofCharUtf8 c > 1) Gen.unicode)
        let needed = sizeofCharUtf8 c
        result <- liftIO $ withBuffer 4 \ptr -> do
          _ <- writeUtf8OffPtr ptr 4 c
          -- Read with one fewer byte than needed.
          readUtf8OffPtr ptr (needed - 1)
        result === Nothing

    , testUnit "Nothing when remaining is 0" do
        result <- liftIO $ withBuffer 4 \ptr -> readUtf8OffPtr ptr 0
        result === Nothing
    ]
