{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Unicode
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
module Data.Unicode
  ( -- * Encoding
    ord1,
    ord2,
    ord3,
    ord4,
    -- * Decoding (safe / validating)
    chr1,
    chr2,
    chr3,
    chr4,
    -- * Pointer-arena I\/O
    readUtf8OffPtr,
    writeUtf8OffPtr,
    -- * Width queries
    sizeofLeaderUtf8,
    sizeofCharUtf8,
    -- * Predicates
    isContinuation,
  )
where

import Data.Bits qualified as Bits
import Data.Char qualified as Char
import Data.Primitive.Ptr (readOffPtr)
import Data.Unicode.TH (staticListE)

import GHC.Exts (Char (..), Char#, Int (..), Word#, Word8#, Int#)
import GHC.Exts qualified as GHC
import GHC.IO (unsafeDupablePerformIO)
import GHC.Ptr (Ptr (..), plusPtr)
import GHC.Storable (readWord8OffPtr, writeWord8OffPtr)
import GHC.Word (Word8 (..))

--------------------------------------------------------------------------------

wordToChar# :: Word# -> Char#
wordToChar# x# = GHC.chr# (GHC.word2Int# x#)

charToWord# :: Char# -> Word#
charToWord# x# = GHC.int2Word# (GHC.ord# x#)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
ord1 :: Char -> Word8
ord1 (C# x#) = W8# (ord1# x#)
{-# INLINE ord1 #-}

-- TODO: docs
ord1# :: Char# -> Word8#
ord1# x# = GHC.wordToWord8# (charToWord# x#)

-- | TODO: docs
--
-- @since 1.0.0
ord2 :: Char -> (Word8, Word8)
ord2 (C# x#) = case ord2# x# of
  (# b0#, b1# #) -> (W8# b0#, W8# b1#)
{-# INLINE ord2 #-}

-- TODO: docs
ord2# :: Char# -> (# Word8#, Word8# #)
ord2# (charToWord# -> x#) =
  let !b0# = GHC.or# 0xc0## (GHC.and# 0xff## (GHC.uncheckedShiftRL# x# 6#))
      !b1# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 0#))
   in (# GHC.wordToWord8# b0#, GHC.wordToWord8# b1# #)

-- | TODO: docs
--
-- @since 1.0.0
ord3 :: Char -> (Word8, Word8, Word8)
ord3 (C# x#) = case ord3# x# of
  (# b0#, b1#, b2# #) -> (W8# b0#, W8# b1#, W8# b2#)
{-# INLINE ord3 #-}

-- TODO: docs
ord3# :: Char# -> (# Word8#, Word8#, Word8# #)
ord3# (charToWord# -> x#) =
  let !b0# = GHC.or# 0xe0## (GHC.and# 0xff## (GHC.uncheckedShiftRL# x# 12#))
      !b1# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 6#))
      !b2# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 0#))
   in (# GHC.wordToWord8# b0#, GHC.wordToWord8# b1#, GHC.wordToWord8# b2# #)

-- | TODO: docs
--
-- @since 1.0.0
ord4 :: Char -> (Word8, Word8, Word8, Word8)
ord4 (C# x#) = case ord4# x# of
  (# b0#, b1#, b2#, b3# #) -> (W8# b0#, W8# b1#, W8# b2#, W8# b3#)
{-# INLINE ord4 #-}

-- TODO: docs
ord4# :: Char# -> (# Word8#, Word8#, Word8#, Word8# #)
ord4# (charToWord# -> x#) =
  let !b0# = GHC.or# 0xf0## (GHC.and# 0xff## (GHC.uncheckedShiftRL# x# 18#))
      !b1# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 12#))
      !b2# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 6#))
      !b3# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 0#))
   in (# GHC.wordToWord8# b0#, GHC.wordToWord8# b1#, GHC.wordToWord8# b2#, GHC.wordToWord8# b3# #)

-- | Decode a 1-byte UTF-8 sequence. The byte must be in the ASCII
-- range (@\< 0x80@); any other input is invalid UTF-8 (a leader or
-- continuation byte cannot stand alone) and produces 'Nothing'.
-- Delegates the arithmetic to 'chr1#'.
--
-- @since 1.0.0
chr1 :: Word8 -> Maybe Char
chr1 b@(W8# x#)
  | b < 0x80  = Just (C# (chr1# x#))
  | otherwise = Nothing
{-# INLINE chr1 #-}

-- | Unchecked 1-byte UTF-8 decode primop. The caller is responsible
-- for ensuring the input is in @0x00..0x7F@; out-of-range bytes
-- produce a 'Char' that is /not/ a valid 1-byte UTF-8 decode but
-- whose Latin-1 code point matches the byte value.
chr1# :: Word8# -> Char#
chr1# x# = wordToChar# (GHC.word8ToWord# x#)

-- | Decode a 2-byte UTF-8 sequence. Rejects malformed continuation
-- bytes (high bits must be @10@) and overlong encodings of code
-- points @\< 0x80@. Returns 'Nothing' on either failure. The actual
-- byte arithmetic runs through the hand-unboxed 'chr2#' primop; this
-- wrapper only adds the validation guards.
--
-- @since 1.0.0
chr2 :: Word8 -> Word8 -> Maybe Char
chr2 (W8# x#) b2@(W8# y#)
  | not (isContinuation b2) = Nothing
  | Char.ord c < 0x80       = Nothing  -- overlong
  | otherwise               = Just c
  where
    !c = C# (chr2# x# y#)
{-# INLINE chr2 #-}

-- | Unchecked 2-byte UTF-8 decode primop. The caller is responsible
-- for ensuring the input is a well-formed 2-byte sequence; malformed
-- input produces a 'Char#' whose value mixes in the spurious high
-- bits of the leader\/continuation. See 'chr2' for the validating
-- variant.
chr2# :: Word8# -> Word8# -> Char#
chr2# x# y# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x3f##) 6#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1#)

-- | Decode a 3-byte UTF-8 sequence. Rejects malformed continuation
-- bytes, overlong encodings of code points @\< 0x800@, and code
-- points in the surrogate range @U+D800..U+DFFF@ (which are not
-- valid in UTF-8). Delegates the arithmetic to 'chr3#'.
--
-- @since 1.0.0
chr3 :: Word8 -> Word8 -> Word8 -> Maybe Char
chr3 (W8# x#) b2@(W8# y#) b3@(W8# z#)
  | not (isContinuation b2 && isContinuation b3) = Nothing
  | cp < 0x800                                   = Nothing  -- overlong
  | cp >= 0xD800 && cp <= 0xDFFF                 = Nothing  -- surrogate
  | otherwise                                    = Just c
  where
    !c  = C# (chr3# x# y# z#)
    !cp = Char.ord c
{-# INLINE chr3 #-}

-- | Unchecked 3-byte UTF-8 decode primop. See 'chr3' for the safe
-- variant.
chr3# :: Word8# -> Word8# -> Word8# -> Char#
chr3# x# y# z# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x1f##) 12#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 6#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1# `GHC.or#` b2#)

-- | Decode a 4-byte UTF-8 sequence. Rejects malformed continuation
-- bytes, overlong encodings of code points @\< 0x10000@, and out-of-
-- Unicode code points above @U+10FFFF@. Delegates the arithmetic to
-- 'chr4#'.
--
-- @since 1.0.0
chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Maybe Char
chr4 (W8# x#) b2@(W8# y#) b3@(W8# z#) b4@(W8# w#)
  | not (isContinuation b2 && isContinuation b3 && isContinuation b4)
                = Nothing
  | cp < 0x10000  = Nothing  -- overlong
  | cp > 0x10FFFF = Nothing  -- out of Unicode range
  | otherwise     = Just c
  where
    !c  = C# (chr4# x# y# z# w#)
    !cp = Char.ord c
{-# INLINE chr4 #-}

-- | Unchecked 4-byte UTF-8 decode primop. See 'chr4' for the safe
-- variant.
chr4# :: Word8# -> Word8# -> Word8# -> Word8# -> Char#
chr4# x# y# z# w# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x0f##) 18#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 12#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 6#
      !b3# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# w#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1# `GHC.or#` b2# `GHC.or#` b3#)

-- | A UTF-8 continuation byte has the high two bits set to @10@.
--
-- @since 1.0.0
isContinuation :: Word8 -> Bool
isContinuation b = b Bits..&. 0xC0 == 0x80
{-# INLINE isContinuation #-}

-- | Read one UTF-8 character from memory at @ptr@, bounded by
-- @remaining@ bytes. Returns the decoded character and its byte
-- width on success, or 'Nothing' if:
--
-- * @remaining@ is less than 1 (no leader byte to read).
-- * The leader byte indicates a sequence longer than @remaining@.
-- * The continuation bytes are malformed (see 'chr2'\/'chr3'\/'chr4').
-- * The decoded code point is overlong, in the surrogate range, or
--   above @U+10FFFF@.
--
-- The bounds check ensures @ptr@ is read at offsets in
-- @0..(needed-1)@ where @needed@ is the UTF-8 width derived from the
-- leader. Callers must guarantee that @remaining@ bytes are addressable
-- from @ptr@.
--
-- @since 1.0.0
readUtf8OffPtr :: Ptr Word8 -> Int -> IO (Maybe (Char, Int))
readUtf8OffPtr ptr remaining
  | remaining < 1 = pure Nothing
  | otherwise = do
      cu1 <- readWord8OffPtr ptr 0
      let needed = sizeofLeaderUtf8 cu1
      if remaining < needed
        then pure Nothing
        else case needed of
          4 -> do
            cu2 <- readWord8OffPtr ptr 1
            cu3 <- readWord8OffPtr ptr 2
            cu4 <- readWord8OffPtr ptr 3
            pure (fmap (\c -> (c, 4)) (chr4 cu1 cu2 cu3 cu4))
          3 -> do
            cu2 <- readWord8OffPtr ptr 1
            cu3 <- readWord8OffPtr ptr 2
            pure (fmap (\c -> (c, 3)) (chr3 cu1 cu2 cu3))
          2 -> do
            cu2 <- readWord8OffPtr ptr 1
            pure (fmap (\c -> (c, 2)) (chr2 cu1 cu2))
          _ ->
            pure (fmap (\c -> (c, 1)) (chr1 cu1))
{-# INLINEABLE readUtf8OffPtr #-}

-- | Write one UTF-8 character to memory at @ptr@, bounded by
-- @remaining@ bytes. Returns the advanced pointer on success or
-- 'Nothing' if the character's UTF-8 width exceeds @remaining@.
--
-- @since 1.0.0
writeUtf8OffPtr :: Ptr Word8 -> Int -> Char -> IO (Maybe (Ptr Word8))
writeUtf8OffPtr ptr remaining c
  | needed > remaining = pure Nothing
  | otherwise = case needed of
      4 -> do
        let !(cu1, cu2, cu3, cu4) = ord4 c
        writeWord8OffPtr ptr 0 cu1
        writeWord8OffPtr ptr 1 cu2
        writeWord8OffPtr ptr 2 cu3
        writeWord8OffPtr ptr 3 cu4
        pure (Just (plusPtr ptr 4))
      3 -> do
        let !(cu1, cu2, cu3) = ord3 c
        writeWord8OffPtr ptr 0 cu1
        writeWord8OffPtr ptr 1 cu2
        writeWord8OffPtr ptr 2 cu3
        pure (Just (plusPtr ptr 3))
      2 -> do
        let !(cu1, cu2) = ord2 c
        writeWord8OffPtr ptr 0 cu1
        writeWord8OffPtr ptr 1 cu2
        pure (Just (plusPtr ptr 2))
      _ -> do
        let !cu1 = ord1 c
        writeWord8OffPtr ptr 0 cu1
        pure (Just (plusPtr ptr 1))
  where
    !needed = sizeofCharUtf8 c

utf8LeaderLengthTable :: Ptr Int
utf8LeaderLengthTable = Ptr $(staticListE @Int [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0])

-- | Given a valid UTF-8 leader byte, obtain the number of UTF-8 code units
-- used to encode the UTF-8 character.
--
-- >>> sizeofLeaderUtf8 0x61 -- The UTF-8 representation of 'a'
-- 1
--
-- @since 1.0.0
sizeofLeaderUtf8 :: Word8 -> Int
sizeofLeaderUtf8 (W8# x#) =
  let !offset# = GHC.word2Int# (GHC.word8ToWord# (GHC.uncheckedShiftRLWord8# x# 3#))
      result   = readOffPtr utf8LeaderLengthTable (I# offset#)
   in unsafeDupablePerformIO result

-- | Obtain the number of UTF-8 code units that are required to encode the
-- given 'Char' in UTF-8
--
-- @since 1.0.0
sizeofCharUtf8 :: Char -> Int
sizeofCharUtf8 (C# x#) = I# (sizeofCharUtf8# x#)
{-# INLINE sizeofCharUtf8 #-}

-- TODO: docs
sizeofCharUtf8# :: Char# -> Int#
sizeofCharUtf8# x# =
  let !cmp0# = GHC.geChar# x# '\x80'#
      !cmp1# = GHC.geChar# x# '\x800'#
      !cmp2# = GHC.geChar# x# '\x10000'#
   in cmp0# GHC.+# cmp1# GHC.+# cmp2# GHC.+# 1#