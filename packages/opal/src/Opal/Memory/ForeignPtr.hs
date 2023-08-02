{-# LANGUAGE ForeignFunctionInterface #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Memory.ForeignPtr
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Foreign pointer operations.
--
-- @since 1.0.0
module Opal.Memory.ForeignPtr
  ( -- * ForeignPtr
    ForeignPtr (..)
  , ForeignPtrContents (..)
    -- ** Basic Operations
  , nullForeignPtr
  , hashForeignPtr
  , plusForeignPtr
    -- ** Comparison
  , compareOffForeignPtr
  , eqOffForeignPtr
    -- ** Query
  , isNullForeignPtr
  )
where

import Data.Hashable (hashPtr, hashPtrWithSalt)
import Data.Word (Word8)

import GHC.Exts qualified as GHC
import GHC.ForeignPtr
  ( ForeignPtr (..)
  , ForeignPtrContents (..)
  , plusForeignPtr
  , withForeignPtr
  )

import Opal.Memory.Ptr (compareOffPtr, eqOffPtr)

-- ForeignPtr - Basic Operations -----------------------------------------------

-- | The constant null 'ForeignPtr'. This 'ForeignPtr' is not associated with
-- any valid memory location.
--
-- @since 1.0.0
nullForeignPtr :: ForeignPtr a
nullForeignPtr = ForeignPtr GHC.nullAddr# FinalPtr

-- | \(O(n)\). Computes the hash of the given foreign pointer's contents up to
-- the specified length in bytes.
--
-- @since 1.0.0
hashForeignPtr ::
  -- | The salt compute the 'ForeignPtr' hash with. The default hash salt will
  -- be used if not given.
  Maybe Int ->
  -- | The 'ForeignPtr'
  ForeignPtr a ->
  -- | The length of the foreign pointer's contents in bytes.
  Int ->
  -- | Returns an 'Int' hash computed from the foreign pointer's contents.
  IO Int
hashForeignPtr Nothing  fp len = withForeignPtr fp \ptr -> hashPtr ptr len
hashForeignPtr (Just s) fp len = withForeignPtr fp \ptr -> hashPtrWithSalt ptr len s

-- ForeignPtr - Comparison -----------------------------------------------------

-- | \(O(n)\). Compare the contents of the two given foreign pointers up to the
-- specified length in bytes.
--
-- @since 1.0.0
compareOffForeignPtr :: ForeignPtr Word8 -> ForeignPtr Word8 -> Int -> IO Ordering
compareOffForeignPtr fp1 fp2 len =
  withForeignPtr fp1 \ptr1 ->
    withForeignPtr fp2 \ptr2 ->
      compareOffPtr ptr1 ptr2 len

-- | \(O(n)\). Compare the contents of the two given foreign pointers for
-- equality up to the specified length in bytes.
--
-- @since 1.0.0
eqOffForeignPtr :: ForeignPtr Word8 -> ForeignPtr Word8 -> Int -> IO Bool
eqOffForeignPtr fp1 fp2 len = do
  withForeignPtr fp1 \ptr1 ->
    withForeignPtr fp2 \ptr2 ->
      eqOffPtr ptr1 ptr2 len

-- ForeignPtr - Query ----------------------------------------------------------

-- | Is the given 'ForeignPtr' the 'nullForeignPtr'?
--
-- @since 1.0.0
isNullForeignPtr :: ForeignPtr a -> Bool
isNullForeignPtr fp = fp == nullForeignPtr

