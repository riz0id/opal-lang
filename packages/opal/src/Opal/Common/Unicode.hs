{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UnboxedTuples              #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.Unicode
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
module Opal.Common.Unicode
  ( ord1
  , ord2
  , ord3
  , ord4
  , writeUtf8OffPtr
  , chr1
  , chr2
  , chr3
  , chr4
  , readUtf8OffPtr
  , copyStringUtf8ToPtr
    -- * Query
  , sizeofLeaderUtf8
  , sizeofCharUtf8
  , sizeofStringUtf8
  , sizeofUtf8OffForeignPtr
  , sizeofUtf8OffPtr
  )
where 

import Data.Primitive.Ptr (readOffPtr)
import Data.Word (Word8)

import GHC.Exts (Char (..), Char#, Word#, Word8#, Int#, Int (..))
import GHC.Exts qualified as GHC
import GHC.Ptr (Ptr (..), plusPtr)
import GHC.Storable (writeWord8OffPtr, readWord8OffPtr)
import GHC.Word (Word8 (..))

import Opal.Common.TH (staticListE)

import System.IO.Unsafe (unsafeDupablePerformIO)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)

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

-- | TODO: docs
--
-- @since 1.0.0
writeUtf8OffPtr :: Ptr Word8 -> Char -> IO (Ptr Word8)
writeUtf8OffPtr ptr c 
  | c >= '\x10000' = do
    let !(cu1, cu2, cu3, cu4) = ord4 c
    writeWord8OffPtr ptr 0 cu1
    writeWord8OffPtr ptr 1 cu2
    writeWord8OffPtr ptr 2 cu3
    writeWord8OffPtr ptr 3 cu4
    pure (plusPtr ptr 4)
  | c >= '\x800' = do
    let !(cu1, cu2, cu3) = ord3 c
    writeWord8OffPtr ptr 0 cu1
    writeWord8OffPtr ptr 1 cu2
    writeWord8OffPtr ptr 2 cu3
    pure (plusPtr ptr 3)
  | c >= '\x80' = do
    let !(cu1, cu2) = ord2 c
    writeWord8OffPtr ptr 0 cu1
    writeWord8OffPtr ptr 1 cu2
    pure (plusPtr ptr 2)
  | otherwise = do 
    let !cu1 = ord1 c
    writeWord8OffPtr ptr 0 cu1
    pure (plusPtr ptr 1)

-- | TODO: docs
--
-- @since 1.0.0
chr1 :: Word8 -> Char
chr1 (W8# x#) = C# (chr1# x#)
{-# INLINE chr1 #-}

-- TODO: docs
chr1# :: Word8# -> Char#
chr1# x# = wordToChar# (GHC.word8ToWord# x#)

-- | TODO: docs
--
-- @since 1.0.0
chr2 :: Word8 -> Word8 -> Char
chr2 (W8# x#) (W8# y#) = C# (chr2# x# y#)
{-# INLINE chr2 #-}

-- TODO: docs
chr2# :: Word8# -> Word8# -> Char#
chr2# x# y# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x3f##) 6#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1#)

-- | TODO: docs
--
-- @since 1.0.0
chr3 :: Word8 -> Word8 -> Word8 -> Char
chr3 (W8# x#) (W8# y#) (W8# z#) = C# (chr3# x# y# z#)
{-# INLINE chr3 #-}

-- TODO: docs
chr3# :: Word8# -> Word8# -> Word8# -> Char#
chr3# x# y# z# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x1f##) 12#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 6#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1# `GHC.or#` b2#)

-- | TODO: docs
--
-- @since 1.0.0
chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr4 (W8# x#) (W8# y#) (W8# z#) (W8# w#) = C# (chr4# x# y# z# w#)
{-# INLINE chr4 #-}

-- TODO: docs
chr4# :: Word8# -> Word8# -> Word8# -> Word8# -> Char#
chr4# x# y# z# w# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x0f##) 18#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 12#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 6#
      !b3# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# w#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1# `GHC.or#` b2# `GHC.or#` b3#)

readUtf8OffPtr :: Ptr Word8 -> IO (Char, Int)
readUtf8OffPtr ptr = do 
  cu1 <- readWord8OffPtr ptr 0
  case sizeofLeaderUtf8 cu1 of 
    4 -> do
      cu2 <- readWord8OffPtr ptr 1
      cu3 <- readWord8OffPtr ptr 2
      cu4 <- readWord8OffPtr ptr 3
      pure (chr4 cu1 cu2 cu3 cu4, 4)
    3 -> do 
      cu2 <- readWord8OffPtr ptr 1
      cu3 <- readWord8OffPtr ptr 2
      pure (chr3 cu1 cu2 cu3, 3)
    2 -> do 
      cu2 <- readWord8OffPtr ptr 1
      pure (chr2 cu1 cu2, 2)
    _ -> 
      pure (chr1 cu1, 1)
{-# INLINEABLE readUtf8OffPtr #-}

-- | \(O(n)\). TODO: docs 
--
-- @since 1.0.0
copyStringUtf8ToPtr :: Ptr Word8 -> String -> IO ()
copyStringUtf8ToPtr _   []       = pure ()
copyStringUtf8ToPtr ptr (x : xs) = do 
  ptr' <- writeUtf8OffPtr ptr x
  copyStringUtf8ToPtr ptr' xs

-- Query -----------------------------------------------------------------------

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

-- | \(O(n)\). Obtain the number of bytes required to store the given 'String' 
-- in UTF-8.
--
-- @since 1.0.0
sizeofStringUtf8 :: String -> Int 
sizeofStringUtf8 = foldl (\n c -> n + sizeofCharUtf8 c) 0 

-- | \(O(n)\). Similar to 'sizeofUtf8OffPtr', but instead accepts a 'ForeignPtr' 
-- as the pointer to the UTF-8 string.
--
-- @since 1.0.0
sizeofUtf8OffForeignPtr :: ForeignPtr Word8 -> Int -> IO Int
sizeofUtf8OffForeignPtr src len = withForeignPtr src (`sizeofUtf8OffPtr` len)

-- | \(O(n)\). Count the number of UTF-8 code units that are encoded off a 
-- pointer to a UTF-8 string. 
--
-- @since 1.0.0
sizeofUtf8OffPtr :: 
  -- | Pointer to a UTF-8 encoded string.
  Ptr Word8 -> 
  -- | The size of the UTF-8 string in bytes.
  Int -> 
  -- | Returns size of the UTF-8 string in characters.
  IO Int
sizeofUtf8OffPtr src len = do
  let end :: Ptr Word8 
      end = plusPtr src len
      
  let run :: Int -> Ptr Word8 -> IO Int
      run n ptr 
        | ptr < end = do
          leader <- readWord8OffPtr ptr 0 
          let i = sizeofLeaderUtf8 leader
          run (i + n) (ptr `plusPtr` i)
        | otherwise = pure n

  run 0 src 