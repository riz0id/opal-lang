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
-- TODO: docs
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

-- | TODO: docs
--
-- @since 1.0.0
staticListE :: Prim a => [a] -> Q Exp
staticListE xs = do
  bytes <- TH.runIO (listToBytes xs)
  pure (LitE (BytesPrimL bytes))

-- | TODO: docs
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