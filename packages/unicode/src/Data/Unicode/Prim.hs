{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Unicode.Prim
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Unchecked unboxed primops for UTF-8 codec work. These are the
-- fast inner paths used by the safe boxed APIs in "Data.Unicode" —
-- exposed here so callers who already know their input is well-formed
-- (and want to skip the validation overhead) can reach them directly.
--
-- All @#@-suffixed encoders assume the input @Char@ is in the
-- expected range for the chosen width — calling 'ord3#' on a 4-byte
-- character produces nonsense. Use 'Data.Unicode.sizeofCharUtf8' to
-- dispatch.
--
-- All @#@-suffixed decoders assume the input bytes are well-formed
-- UTF-8 — malformed bytes produce a @Char#@ whose spurious bits leak
-- into the result. Use the validating boxed wrappers in
-- "Data.Unicode" if your input may be untrusted.
--
-- @since 1.0.0
module Data.Unicode.Prim
  ( -- * Char\#\/Word\# conversion helpers
    wordToChar#,
    charToWord#,
    -- * Encoding primops
    ord1#,
    ord2#,
    ord3#,
    ord4#,
    -- * Decoding primops
    chr1#,
    chr2#,
    chr3#,
    chr4#,
    -- * Width-query primop
    sizeofCharUtf8#,
  )
where

import GHC.Exts (Char#, Word#, Word8#, Int#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

-- | Reinterpret a 'Word#' as a 'Char#'. No range check.
--
-- @since 1.0.0
wordToChar# :: Word# -> Char#
wordToChar# x# = GHC.chr# (GHC.word2Int# x#)

-- | Reinterpret a 'Char#' as a 'Word#'.
--
-- @since 1.0.0
charToWord# :: Char# -> Word#
charToWord# x# = GHC.int2Word# (GHC.ord# x#)

--------------------------------------------------------------------------------

-- | Encode a code point in @U+0000..U+007F@ (ASCII) as one UTF-8
-- byte. The caller is responsible for ensuring the input is in
-- range; out-of-range input produces a low byte that is not a valid
-- 1-byte UTF-8 sequence.
ord1# :: Char# -> Word8#
ord1# x# = GHC.wordToWord8# (charToWord# x#)

-- | Encode a code point in @U+0080..U+07FF@ as a 2-byte UTF-8
-- sequence @(leader, continuation)@.
ord2# :: Char# -> (# Word8#, Word8# #)
ord2# (charToWord# -> x#) =
  let !b0# = GHC.or# 0xc0## (GHC.and# 0xff## (GHC.uncheckedShiftRL# x# 6#))
      !b1# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 0#))
   in (# GHC.wordToWord8# b0#, GHC.wordToWord8# b1# #)

-- | Encode a code point in @U+0800..U+FFFF@ as a 3-byte UTF-8
-- sequence @(leader, cont, cont)@.
ord3# :: Char# -> (# Word8#, Word8#, Word8# #)
ord3# (charToWord# -> x#) =
  let !b0# = GHC.or# 0xe0## (GHC.and# 0xff## (GHC.uncheckedShiftRL# x# 12#))
      !b1# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 6#))
      !b2# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 0#))
   in (# GHC.wordToWord8# b0#, GHC.wordToWord8# b1#, GHC.wordToWord8# b2# #)

-- | Encode a code point in @U+10000..U+10FFFF@ as a 4-byte UTF-8
-- sequence @(leader, cont, cont, cont)@.
ord4# :: Char# -> (# Word8#, Word8#, Word8#, Word8# #)
ord4# (charToWord# -> x#) =
  let !b0# = GHC.or# 0xf0## (GHC.and# 0xff## (GHC.uncheckedShiftRL# x# 18#))
      !b1# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 12#))
      !b2# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 6#))
      !b3# = GHC.or# 0x80## (GHC.and# 0x3f## (GHC.uncheckedShiftRL# x# 0#))
   in (# GHC.wordToWord8# b0#, GHC.wordToWord8# b1#, GHC.wordToWord8# b2#, GHC.wordToWord8# b3# #)

--------------------------------------------------------------------------------

-- | Unchecked 1-byte UTF-8 decode. The caller is responsible for
-- ensuring the input is in @0x00..0x7F@; out-of-range bytes produce
-- a 'Char#' that is /not/ a valid 1-byte UTF-8 decode but whose
-- Latin-1 code point matches the byte value.
chr1# :: Word8# -> Char#
chr1# x# = wordToChar# (GHC.word8ToWord# x#)

-- | Unchecked 2-byte UTF-8 decode. Assumes the inputs are a valid
-- 2-byte sequence; malformed input produces a 'Char#' whose value
-- mixes in spurious bits from the leader\/continuation. See
-- 'Data.Unicode.chr2' for the validating variant.
chr2# :: Word8# -> Word8# -> Char#
chr2# x# y# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x3f##) 6#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1#)

-- | Unchecked 3-byte UTF-8 decode. See 'Data.Unicode.chr3' for the
-- validating variant.
chr3# :: Word8# -> Word8# -> Word8# -> Char#
chr3# x# y# z# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x1f##) 12#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 6#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1# `GHC.or#` b2#)

-- | Unchecked 4-byte UTF-8 decode. See 'Data.Unicode.chr4' for the
-- validating variant.
chr4# :: Word8# -> Word8# -> Word8# -> Word8# -> Char#
chr4# x# y# z# w# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x0f##) 18#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 12#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 6#
      !b3# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# w#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1# `GHC.or#` b2# `GHC.or#` b3#)

--------------------------------------------------------------------------------

-- | Unboxed primop behind 'Data.Unicode.sizeofCharUtf8'. Sums three
-- @\>=@ comparisons to pick a width in @1..4@.
sizeofCharUtf8# :: Char# -> Int#
sizeofCharUtf8# x# =
  let !cmp0# = GHC.geChar# x# '\x80'#
      !cmp1# = GHC.geChar# x# '\x800'#
      !cmp2# = GHC.geChar# x# '\x10000'#
   in cmp0# GHC.+# cmp1# GHC.+# cmp2# GHC.+# 1#
