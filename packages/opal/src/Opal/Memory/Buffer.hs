{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Memory.Buffer
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Continuous regions of pinned memory.
--
-- @since 1.0.0
module Opal.Memory.Buffer
  ( -- * Buffer
    Buffer (..)

    -- ** Basic Operations
  , emptyBuffer
  , newBuffer
  , dropBuffer
  , takeBuffer
  , copyBuffer
  , copyBufferToPtr
  , fillWord8OffBuffer
  , resizeBuffer
  , withBufferContents

    -- ** Comparisons
  , bufferEq

    -- ** Conversions
  , packBuffer
  , unpackBuffer

    -- ** Folds
  , foldlBuffer
  , foldrBuffer'
  , foldlBufferUtf8
  , foldrBufferUtf8'

    -- ** Query
  , nullBuffer
  , sizeofBuffer

    -- ** IO Operations
  , openFileBuffer
  , hGetBuffer
  , hPutBuffer

    -- ** Read Operations
  , readWord8OffBuffer
  , readUtf8OffBuffer

    -- ** Write Operations
  , writeWord8OffBuffer
  , writeUtf8OffBuffer
  )
where

import Control.DeepSeq (NFData (..))

import Control.Exception (Exception (..), throwIO)

import Control.Monad (when)

import Data.Default (Default (..))
import Data.Word (Word8)

import Foreign (copyBytes, fillBytes)

import GHC.Exts qualified as GHC
import GHC.ForeignPtr
  ( ForeignPtr (..)
  , mallocPlainForeignPtrBytes
  , withForeignPtr
  )
import GHC.Ptr (Ptr, minusPtr, plusPtr)
import GHC.Storable (readWord8OffPtr, writeWord8OffPtr)

import Opal.Memory.ForeignPtr
  ( eqOffForeignPtr
  , isNullForeignPtr
  , nullForeignPtr
  , plusForeignPtr
  )
import Opal.Memory.Ptr (copyListOffPtr, foldlWord8OffPtr, foldrWord8OffPtr)
import Opal.Common.Unicode (readUtf8OffPtr, writeUtf8OffPtr)

import System.IO
  ( Handle
  , IOMode (..)
  , hFileSize
  , hGetBuf
  , hPutBuf
  , withFile
  )

-- Buffer ----------------------------------------------------------------------

-- | 'Buffer' is a contiguous region of fixed size.
--
-- @since 1.0.0
data Buffer = Buffer
  {-# UNPACK #-} !(ForeignPtr Word8)
  {-# UNPACK #-} !Int

-- | @since 1.0.0
instance Default Buffer where
  def = emptyBuffer

-- | @since 1.0.0
instance Eq Buffer where
  Buffer fp1 len1 == Buffer fp2 len2 = fp1 == fp2 && len1 == len2
  {-# INLINE (==) #-}

  Buffer fp1 len1 /= Buffer fp2 len2 = fp1 /= fp2 || len1 /= len2
  {-# INLINE (/=) #-}

-- | @since 1.0.0
instance NFData Buffer where
  rnf Buffer {} = ()

-- | @since 1.0.0
instance Show Buffer where
  showsPrec p (Buffer fp _) = showString "Buffer<" . showsPrec p fp . showChar '>'
  {-# INLINE showsPrec #-}

-- Buffer - Basic Operations ---------------------------------------------------

-- | The empty 'Buffer'.
--
-- @since 1.0.0
emptyBuffer :: Buffer
emptyBuffer = Buffer nullForeignPtr 0

-- | Construct a new 'Buffer' of the specified size in bytes.
--
-- __Note:__ If the specified size is less than or equal to zero, then the
-- resulting 'Buffer' will be an 'emptyBuffer'.
--
-- @since 1.0.0
newBuffer :: Int -> IO Buffer
newBuffer size
  | 0 < size  = fmap (`Buffer` size) (mallocPlainForeignPtrBytes size)
  | otherwise = pure emptyBuffer

-- | TODO: docs
--
-- @since 1.0.0
dropBuffer :: Int -> Buffer -> Buffer
dropBuffer n (Buffer fp len)
  | n < len   = Buffer (fp `plusForeignPtr` n) (len - n)
  | otherwise = emptyBuffer

-- | TODO: docs
--
-- @since 1.0.0
takeBuffer :: Int -> Buffer -> Buffer
takeBuffer n (Buffer fp len)
  | n < len   = Buffer fp (len - n)
  | otherwise = emptyBuffer

-- | Copy a slice of a 'Buffer' to a @('Ptr' 'Word8')@. The slice to fill is
-- specified by an offset into the buffers and the length of the slice, both
-- given in bytes.
--
-- @since 1.0.0
copyBuffer :: Buffer -> Buffer -> Int ->Int -> IO ()
copyBuffer dst src i n =
  withBufferContents dst \ptr ->
    copyBufferToPtr (ptr `plusPtr` i) src i n

-- | Copy a slice of a 'Buffer' to a @('Ptr' 'Word8')@. The slice to fill is
-- specified by an offset into the source buffer and the length of the slice,
-- both given in bytes.
--
-- @since 1.0.0
copyBufferToPtr :: Ptr Word8 -> Buffer -> Int -> Int ->IO ()
copyBufferToPtr dst src i n =
  withBufferContents src \srcPtr ->
    copyBytes dst (srcPtr `plusPtr` i) n

-- | Fills a slice of a 'Buffer' with the given 'Word8'. The slice to fill is
-- specified by an offset into the buffer and the length of the slice, both
-- given in bytes.
--
-- __Example:__
--
-- Allocate a new buffer containing 128 bytes. Set all bytes in the first half
-- of the buffer to @(0 :: 'Word8')@ and all bytes in the second half of the
-- buffer to @(1 :: 'Word8')@.
--
-- @
-- let buffer = newBuffer 128
-- fillWord8OffBuffer buffer 0  64  0
-- fillWord8OffBuffer buffer 64 128 1
-- @
--
-- @since 1.0.0
fillWord8OffBuffer :: Buffer -> Int -> Int -> Word8 -> IO ()
fillWord8OffBuffer buf i n x =
  withBufferContents buf \ptr ->
    fillBytes (ptr `plusPtr` i) x n

-- | TODO: docs
--
-- @since 1.0.0
resizeBuffer :: Buffer -> Int -> IO Buffer
resizeBuffer src dstLen = do
  dst <- newBuffer dstLen
  copyBuffer dst src 0 (sizeofBuffer src `min` dstLen)
  pure dst

-- | Similar to 'withForeignPtr'. Obtains the 'Word8' pointer to the given
-- 'Buffer' and brackets it with the given 'IO' action. The buffer's pointer is
-- guaranteed to be kept alive for __at least__ the extent of the 'IO' action.
--
-- __Note:__ All caveats that apply to 'withForeignPtr' also apply to
-- 'withBufferContents'.
--
-- @since 1.0.0
withBufferContents :: Buffer -> (Ptr Word8 -> IO a) -> IO a
withBufferContents (Buffer fp _) = withForeignPtr fp
{-# INLINE withBufferContents #-}

-- Buffer - Comparisons --------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
bufferEq :: Buffer -> Buffer -> IO Bool
bufferEq (Buffer fp1 len1) (Buffer fp2 len2)
  | len1 == len2 = eqOffForeignPtr fp1 fp2 len1
  | otherwise    = pure False

-- Buffer - Conversions --------------------------------------------------------

-- | Pack a list of 'Word8' into a 'Buffer'.
--
-- __Note:__ The given @['Word8']@ must be finite, otherwise 'packBuffer' will
-- diverge.
--
-- @since 1.0.0
packBuffer :: [Word8] -> IO Buffer
packBuffer src
  | null src  = pure emptyBuffer
  | otherwise = do
    dst <- newBuffer (length src)
    withBufferContents dst (`copyListOffPtr` src)
    pure dst

-- | Unpack the contents of a 'Buffer' into a list of 'Word8'.
--
-- @since 1.0.0
unpackBuffer :: Buffer -> IO [Word8]
unpackBuffer = foldrBuffer' (:) []

-- Buffer - Folds --------------------------------------------------------------

-- | Left-associative fold of a 'Buffer', lazy in the accumulator.
--
-- @since 1.0.0
foldlBuffer :: (a -> Word8 -> a) -> a -> Buffer -> IO a
foldlBuffer cons nil (Buffer fp len) =
  withForeignPtr fp \ptr ->
    foldlWord8OffPtr cons nil ptr len
{-# INLINE foldlBuffer #-}

-- | Right-associative fold of a 'Buffer', strict in the accumulator.
--
-- @since 1.0.0
foldrBuffer' :: (Word8 -> a -> a) -> a -> Buffer -> IO a
foldrBuffer' cons nil (Buffer fp len) =
  withForeignPtr fp \ptr ->
    foldrWord8OffPtr cons nil ptr len

-- | Left-associative fold of a 'Buffer', lazy in the accumulator.
--
-- @since 1.0.0
foldlBufferUtf8 :: forall a. (a -> Char -> a) -> a -> Buffer -> IO a
foldlBufferUtf8 c e (Buffer fp len) = do
  withForeignPtr fp \begin -> do
    let end :: Ptr Word8
        end = begin `plusPtr` len

    let run :: Ptr Word8 -> a -> IO a
        run ptr acc
          | ptr < end = do
              (x, n) <- readUtf8OffPtr ptr
              run (ptr `plusPtr` n) (acc `c` x)
          | otherwise = pure acc

    run begin e
{-# INLINE foldlBufferUtf8 #-}

-- | Right-associative fold of a 'Buffer', strict in the accumulator.
--
-- @since 1.0.0
foldrBufferUtf8' :: (Char -> a -> a) -> a -> Buffer -> IO a
foldrBufferUtf8' c e buf = foldlBufferUtf8 (\k x -> GHC.oneShot \xs -> xs `seq` k (c x xs)) id buf <*> pure e

-- Buffer - Query --------------------------------------------------------------

-- | Is the given 'Buffer' empty?
--
-- @since 1.0.0
nullBuffer :: Buffer -> Bool
nullBuffer (Buffer fp len) = isNullForeignPtr fp || len == 0
{-# INLINE nullBuffer #-}

-- | Obtain the size of a 'Buffer' in bytes.
--
-- __Example:__
--
-- >>> buffer <- newBuffer 32
-- >>> sizeofBuffer buffer
-- 32
--
-- @since 1.0.0
sizeofBuffer :: Buffer -> Int
sizeofBuffer (Buffer fp len)
  | isNullForeignPtr fp = 0
  | otherwise           = len
{-# INLINE sizeofBuffer #-}

-- Buffer - IO Operations ------------------------------------------------------

-- | 'FileSizeError' is thrown when the size of a file exceeds the upper bound
-- for 'Int'. This is a limitation of the representation for the 'Buffer' type,
-- which stores the size of the buffer as an 'Int'.
--
-- @since 1.0.0
data FileSizeError = FileSizeError FilePath Integer

-- | @since 1.0.0
instance Exception FileSizeError

-- | @since 1.0.0
instance Show FileSizeError where
  show (FileSizeError filepath size) =
    unlines
      [ "FileSizeError: '" ++ filepath ++ "' is too large"
      , "  * this file is: " ++ show size ++ " bytes"
      , "  * maximum file size: " ++ show (maxBound :: Int) ++ " bytes"
      ]

-- | TODO: docs
--
-- @since 1.0.0
openFileBuffer :: FilePath -> IO Buffer
openFileBuffer filepath = do
  withFile filepath ReadMode \handle -> do
    integerFileSize <- hFileSize handle

    when (maxFileSize <= integerFileSize) do
      throwIO (FileSizeError filepath integerFileSize)

    let fileSize :: Int
        fileSize = fromInteger integerFileSize

    buffer <- newBuffer fileSize
    count  <- hGetBuffer handle buffer 0 fileSize

    if count == fileSize
      then pure buffer
      else resizeBuffer buffer count
  where
    maxFileSize :: Integer
    maxFileSize = toInteger (maxBound :: Int)

-- | TODO: docs
--
-- @since 1.0.0
hGetBuffer :: Handle -> Buffer -> Int -> Int -> IO Int
hGetBuffer handle buffer i n =
  withBufferContents buffer \ptr ->
    hGetBuf handle (ptr `plusPtr` i) n

-- | Copy a slice of a 'Buffer' to file 'Handle'. The slice to fill is
-- specified by an offset into the buffer and the length of the slice, both
-- given in bytes.
--
-- @since 1.0.0
hPutBuffer :: Handle -> Buffer -> Int -> Int -> IO ()
hPutBuffer handle buffer i n =
  withBufferContents buffer \ptr ->
    hPutBuf handle (ptr `plusPtr` i) n

-- Buffer - Write Operations ---------------------------------------------------

-- | Read a 'Word8' from the given 'Buffer' at the specified offset. Offset
-- given in bytes.
--
-- @since 1.0.0
readWord8OffBuffer :: Buffer -> Int -> IO Word8
readWord8OffBuffer buffer i = withBufferContents buffer (`readWord8OffPtr` i)

-- | TODO: docs
--
-- @since 1.0.0
readUtf8OffBuffer :: Buffer -> Int -> IO (Char, Int)
readUtf8OffBuffer buffer i =
  withBufferContents buffer \ptr ->
    readUtf8OffPtr (ptr `plusPtr` i)

-- Buffer - Write Operations ---------------------------------------------------

-- | Write a 'Word8' to the given 'Buffer' at the specified offset. Offset
-- given in bytes.
--
-- @since 1.0.0
writeWord8OffBuffer :: Buffer -> Int -> Word8 -> IO ()
writeWord8OffBuffer buffer i x = withBufferContents buffer \ptr -> writeWord8OffPtr ptr i x

-- | Encode a 'Char' in UTF-8 and write the character to the given 'Buffer' at
-- the specified offset. Offset given in bytes.
--
-- @since 1.0.0
writeUtf8OffBuffer :: Buffer -> Int -> Char -> IO Int
writeUtf8OffBuffer buffer i x =
  withBufferContents buffer \ptr -> do
    ptr' <- writeUtf8OffPtr (ptr `plusPtr` i) x
    pure (ptr' `minusPtr` ptr - i)

