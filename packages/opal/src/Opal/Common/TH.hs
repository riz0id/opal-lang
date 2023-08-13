{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE RoleAnnotations #-}

-- |
-- Module      :  Opal.Common.TH
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
module Opal.Common.TH
  ( -- * Pattern
    Pattern (..)
    -- * Literal
  , Literal (..)
  , staticListE
  )
where

import Data.Int (Int8, Int16, Int32)
import Data.Primitive (Prim (..), alignment, sizeOf)
import Data.Ratio (Ratio)
import Data.Word (Word8, Word16, Word32)

import GHC.Exts (Char (..), Float (..), Int (..), Int#, TYPE, Word (..), Word#)
import GHC.ForeignPtr
  ( castForeignPtr
  , mallocForeignPtrAlignedBytes
  , withForeignPtr
  )

import Language.Haskell.TH (Exp (..), Lit (..), Pat (..), Q, Quote, Type (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Bytes(..))

import Opal.Memory.ForeignPtr (nullForeignPtr)
import Opal.Memory.Ptr (copyListOffPtr)

-- Pattern ---------------------------------------------------------------------

-- | 'Pattern' is a class of types whose values can be lifted into Haskell
-- patterns.
--
-- @since 1.0.0
class Pattern (a :: TYPE r) where
  -- | Turn a value of @a@ into a Haskell pattern.
  --
  -- @since 1.0.0
  liftPat :: Quote m => a -> m Pat

-- | @since 1.0.0
instance Pattern Bool where
  liftPat False = pure (ConP 'False [] [])
  liftPat True  = pure (ConP 'True [] [])

-- | @since 1.0.0
instance Pattern Char where
  liftPat x = pure (ConP 'C# [] [LitP (CharPrimL x)])

-- | @since 1.0.0
instance Pattern Float where
  liftPat x = pure (ConP 'F# [] [LitP (FloatPrimL (toRational x))])

-- | @since 1.0.0
instance Pattern Integer where
  liftPat x = pure (LitP (IntegerL x) `SigP` ConT ''Integer)

-- | @since 1.0.0
instance Pattern Int where
  liftPat x = pure (ConP 'I# [] [LitP (IntPrimL (fromIntegral x))])

-- | @since 1.0.0
instance Pattern Int# where
  liftPat x# = pure (LitP (IntPrimL (fromIntegral (I# x#))))

-- | @since 1.0.0
instance Pattern Int8 where
  liftPat x = pure (LitP (IntegerL (fromIntegral x)) `SigP` ConT ''Int8)

-- | @since 1.0.0
instance Pattern Int16 where
  liftPat x = pure (LitP (IntegerL (fromIntegral x)) `SigP` ConT ''Int16)

-- | @since 1.0.0
instance Pattern Int32 where
  liftPat x = pure (LitP (IntegerL (fromIntegral x)) `SigP` ConT ''Int32)

-- | @since 1.0.0
instance Pattern a => Pattern (Maybe a) where
  liftPat Nothing  = pure (ConP 'Nothing [] [])
  liftPat (Just x) = fmap (\x' -> ConP 'Just [] [x']) (liftPat x)

-- | @since 1.0.0
instance Pattern String where
  liftPat x = pure (LitP (StringL x))

-- | @since 1.0.0
instance Pattern (Ratio Integer) where
  liftPat x = pure (LitP (RationalL x))

-- | @since 1.0.0
instance Pattern Word where
  liftPat x = pure (ConP 'W# [] [LitP (WordPrimL (fromIntegral x))])

-- | @since 1.0.0
instance Pattern Word# where
  liftPat x# = pure (LitP (WordPrimL (fromIntegral (W# x#))))

-- | @since 1.0.0
instance Pattern Word8 where
  liftPat x = pure (LitP (IntegerL (fromIntegral x)) `SigP` ConT ''Word8)

-- | @since 1.0.0
instance Pattern Word16 where
  liftPat x = pure (LitP (IntegerL (fromIntegral x)) `SigP` ConT ''Word16)

-- | @since 1.0.0
instance Pattern Word32 where
  liftPat x = pure (LitP (IntegerL (fromIntegral x)) `SigP` ConT ''Word32)

-- Literal ---------------------------------------------------------------------

-- | 'Pattern' is a class of types whose values can be lifted into Haskell
-- literals.
--
-- @since 1.0.0
class Literal (a :: TYPE r) where
  -- | Turn a value of @a@ into a Haskell literal.
  --
  -- @since 1.0.0
  liftLit :: Quote m => a -> m Lit

-- | @since 1.0.0
instance Literal Char where
  liftLit x = pure (CharL x)

-- | @since 1.0.0
instance Literal Integer where
  liftLit x = pure (IntegerL x)

-- | @since 1.0.0
instance Literal Int where
  liftLit x = pure (IntegerL (fromIntegral x))

-- | @since 1.0.0
instance Literal Int# where
  liftLit x# = pure (IntPrimL (fromIntegral (I# x#)))

-- | @since 1.0.0
instance Literal Int8 where
  liftLit x = pure (IntegerL (fromIntegral x))

-- | @since 1.0.0
instance Literal Int16 where
  liftLit x = pure (IntegerL (fromIntegral x))

-- | @since 1.0.0
instance Literal Int32 where
  liftLit x = pure (IntegerL (fromIntegral x))

-- | @since 1.0.0
instance Literal (Ratio Integer) where
  liftLit x = pure (RationalL x)

-- | @since 1.0.0
instance Literal Word where
  liftLit x = pure (IntegerL (fromIntegral x))

-- | @since 1.0.0
instance Literal Word# where
  liftLit x# = pure (WordPrimL (fromIntegral (W# x#)))

-- | @since 1.0.0
instance Literal Word8 where
  liftLit x = pure (IntegerL (fromIntegral x))

-- | @since 1.0.0
instance Literal Word16 where
  liftLit x = pure (IntegerL (fromIntegral x))

-- | @since 1.0.0
instance Literal Word32 where
  liftLit x = pure (IntegerL (fromIntegral x))

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
