{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.MemoryPtr
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
module Opal.Memory.Ptr
  (  -- * Ptr
    Ptr (..)
    -- ** Basic Operations
  , nullPtr
  , plusPtr
  , minusPtr
  , copyListOffPtr
    -- ** Comparison
  , compareOffPtr
  , eqOffPtr
    -- ** Folds
  , foldlWord8OffPtr
  , foldrWord8OffPtr
    -- ** Query
  , isNullPtr
  )
where

import Control.Monad.Primitive (PrimMonad)

import Data.Primitive (Prim, sizeOf)
import Data.Primitive.Ptr (writeOffPtr)
import Data.Word (Word8)

import Foreign.C (CInt (..), CSize (..))
import Foreign.Ptr (minusPtr, nullPtr, plusPtr)

import GHC.Exts qualified as GHC
import GHC.Ptr (Ptr(..))
import GHC.Storable (readWord8OffPtr)

-- Ptr - Basic Operations ------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
copyListOffPtr :: (PrimMonad m, Prim a) => Ptr a -> [a] -> m ()
copyListOffPtr _   []            = pure ()
copyListOffPtr src items@(i0 :_) = step src items
  where
    size :: Int
    size = sizeOf i0

    step _   []       = pure ()
    step ptr (x : xs) = do
      writeOffPtr ptr 0 x
      step (plusPtr ptr size) xs

-- Ptr - Comparison ------------------------------------------------------------

foreign import ccall unsafe "string.h memcmp" c_memcmp
  :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

-- | \(O(n)\). Compare the contents of the two given pointers up to the
-- specified length.
--
-- @since 1.0.0
compareOffPtr :: Ptr Word8 -> Ptr Word8 -> Int -> IO Ordering
compareOffPtr ptr1 ptr2 len = do
  result <- c_memcmp ptr1 ptr2 (fromIntegral len)
  pure (compare result 0)

-- | \(O(n)\). Compare the contents of the two given pointers for equality up to
-- the specified length.
--
-- @since 1.0.0
eqOffPtr :: Ptr Word8 -> Ptr Word8 -> Int -> IO Bool
eqOffPtr ptr1 ptr2 len = do
  result <- c_memcmp ptr1 ptr2 (fromIntegral len)
  pure (result == 0)

-- Ptr - Folds -----------------------------------------------------------------

-- | \(O(n)\). TODO: docs
--
-- @since 1.0.0
foldlWord8OffPtr :: forall a. (a -> Word8 -> a) -> a -> Ptr Word8 -> Int -> IO a
foldlWord8OffPtr cons nil begin len = do
  let end :: Ptr Word8
      end = begin `plusPtr` len

  let run :: Ptr Word8 -> a -> IO a
      run ptr acc
        | ptr < end = do
            x <- readWord8OffPtr ptr 0
            run (ptr `plusPtr` 1) (acc `cons` x)
        | otherwise = pure acc

  run begin nil

-- | \(O(n)\). TODO: docs
--
-- @since 1.0.0
foldrWord8OffPtr :: (Word8 -> a -> a) -> a -> Ptr Word8 -> Int -> IO a
foldrWord8OffPtr cons nil ptr len =
  foldlWord8OffPtr (\k x -> GHC.oneShot \xs -> xs `seq` k (cons x xs)) id ptr len <*> pure nil

-- Ptr - Query -----------------------------------------------------------------

-- | Is the pointer the null pointer?
--
-- @since 1.0.0
isNullPtr :: Ptr a -> Bool
isNullPtr ptr = nullPtr == ptr

