{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Memory.Slice
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
module Opal.Memory.Slice 
  ( -- * Slice 
    Slice (..)
    -- ** Basic Operations
  , emptySlice 
  , sliceFromPtr
  , sliceBuffer
    -- ** Folds 
  , foldlSliceUtf8
  , foldrSliceUtf8'
    -- ** Lenses
  , sliceBegin
  , sliceEnd
    -- ** Query
  , sizeofSlice
  )
where 

import Control.Lens (Lens', lens)

import Data.Word (Word8)

import GHC.Exts qualified as GHC

import Opal.Common.Unicode (readUtf8OffPtr)
import Opal.Memory.Buffer (Buffer, sizeofBuffer, withBufferContents)
import Opal.Memory.Ptr (Ptr, isNullPtr, minusPtr, nullPtr, plusPtr)

-- Slice -----------------------------------------------------------------------

-- | TODO: docs 
--
-- @since 1.0.0
data Slice = Slice
  { slice_begin :: {-# UNPACK #-} !(Ptr Word8)
    -- ^ The pointer to the beginning of the 'Slice'.
  , slice_end   :: {-# UNPACK #-} !(Ptr Word8)
    -- ^ The pointer to the end of the 'Slice'.
  }
  deriving (Eq, Ord, Show)

-- Slice - Basic Operations ----------------------------------------------------

-- | TODO: docs 
--
-- @since 1.0.0
emptySlice :: Slice
emptySlice = Slice nullPtr nullPtr

-- | TODO: docs 
--
-- @since 1.0.0
sliceFromPtr :: Ptr Word8 -> Int -> Slice 
sliceFromPtr ptr len 
  | isNullPtr ptr = emptySlice
  | otherwise     = Slice ptr (ptr `plusPtr` len)

-- | TODO: docs 
--
-- @since 1.0.0
sliceBuffer :: Buffer -> (Slice -> IO a) -> IO a 
sliceBuffer buffer k = withBufferContents buffer (k . (`sliceFromPtr` sizeofBuffer buffer))

-- Slice - Folds ---------------------------------------------------------------

-- | \(O(n)\). Left-associative fold of a 'Slice', lazy in the accumulator.
--
-- @since 1.0.0
foldlSliceUtf8 :: forall a. (a -> Char -> a) -> a -> Slice -> IO a
foldlSliceUtf8 cons nil (Slice begin end) = do
  let run :: Ptr Word8 -> a -> IO a 
      run ptr acc 
        | ptr < end = do 
            (c, n) <- readUtf8OffPtr ptr 
            run (ptr `plusPtr` n) (acc `cons` c)
        | otherwise = pure acc

  run begin nil

-- | \(O(n)\). Right-associative fold of a 'Slice', strict in the accumulator.
--
-- @since 1.0.0
foldrSliceUtf8' :: (Char -> a -> a) -> a -> Slice -> IO a
foldrSliceUtf8' c e slice = foldlSliceUtf8 (\k x -> GHC.oneShot \xs -> xs `seq` k (c x xs)) id slice <*> pure e 

-- Slice - Lenses --------------------------------------------------------------

-- | Lens focusing on the 'slice_begin' field of a 'Slice'.
--
-- @since 1.0.0
sliceBegin :: Lens' Slice (Ptr Word8)
sliceBegin = lens slice_begin \s x -> s { slice_begin = x }
{-# INLINE sliceBegin #-}

-- | Lens focusing on the 'slice_end' field of a 'Slice'.
--
-- @since 1.0.0
sliceEnd :: Lens' Slice (Ptr Word8)
sliceEnd = lens slice_end \s x -> s { slice_end = x }
{-# INLINE sliceEnd #-}

-- Slice - Query ---------------------------------------------------------------

-- | Obtain the size of a 'Slice' in bytes.
--
-- @since 1.0.0
sizeofSlice :: Slice -> Int
sizeofSlice (Slice begin end) 
  | isNullPtr begin = 0
  | isNullPtr end   = 0
  | otherwise       = end `minusPtr` begin
