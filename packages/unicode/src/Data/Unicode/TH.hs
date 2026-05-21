{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Unicode.TH
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Template Haskell helpers for embedding static data at compile
-- time. Currently exports 'staticListE' — a splice that compiles a
-- @['Prim' a]@ list into a 'BytesPrimL' literal so the resulting
-- pointer reaches into a static memory region in the executable
-- image rather than re-allocating on each call. Used by
-- "Data.Unicode" to back the UTF-8 leader-length lookup table.
--
-- @since 1.0.0
module Data.Unicode.TH
  ( staticListE
  )
where

import Control.Monad.Primitive (PrimMonad)

import Data.Primitive (Prim (..), alignment, sizeOf)
import Data.Primitive.Ptr (writeOffPtr)

import GHC.Exts qualified as GHC
import GHC.ForeignPtr
  ( ForeignPtr (..)
  , ForeignPtrContents (..)
  , castForeignPtr
  , mallocForeignPtrAlignedBytes
  , withForeignPtr
  )
import GHC.Ptr (Ptr, plusPtr)

import Language.Haskell.TH (Exp (..), Lit (..), Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Bytes(..))

--------------------------------------------------------------------------------

nullForeignPtr :: ForeignPtr a
nullForeignPtr = ForeignPtr GHC.nullAddr# FinalPtr

--------------------------------------------------------------------------------

-- | Splice a list of 'Prim' values into a 'BytesPrimL' literal. The
-- list is allocated and serialised at compile time; the runtime
-- value is a pointer to a static memory region embedded in the
-- executable.
--
-- @since 1.0.0
staticListE :: Prim a => [a] -> Q Exp
staticListE xs = do
  bytes <- TH.runIO (listToBytes xs)
  pure (LitE (BytesPrimL bytes))

-- | Serialise a list of 'Prim' values into a 'Bytes' record by
-- allocating a 'ForeignPtr' with proper alignment and writing each
-- element in turn. Used by 'staticListE' at compile time.
--
-- @since 1.0.0
listToBytes :: Prim a => [a] -> IO Bytes
listToBytes []          = pure (Bytes nullForeignPtr 0 0)
listToBytes xs@(x0 : _) = do
  let align = alignment x0
  let size  = length xs * sizeOf x0
  fp <- mallocForeignPtrAlignedBytes size align
  withForeignPtr fp (`copyListOffPtr` xs)
  pure (Bytes (castForeignPtr fp) 0 (fromIntegral size))

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