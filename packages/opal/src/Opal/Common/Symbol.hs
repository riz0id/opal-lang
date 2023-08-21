{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.Symbol
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
module Opal.Common.Symbol
  ( -- * MonadGenSym
    MonadGenSym (..)
    -- * Symbol
  , Symbol (..)
    -- ** Basic Operations
  , stringToSymbol
  , symbolToString
  , symbolFromPtr
  , symbolFromCString
  , symbolFromCStringLen
  , hashSymbol
  , symbolHead
  , symbolTail
    -- ** Comparisons
  , eqSymbol
  , compareSymbol
    -- ** Folds
  , foldl
  , foldr
  , foldr'
    -- ** Query
  , isSymbolChar
  , sizeofSymbol
  , lengthSymbol
    -- ** Substrings
  , splitSymbol
    -- ** Template Haskell
  , symbolToExp
  , symbolToPat
  )
where

import Control.DeepSeq (NFData (..))

import Data.Char (isSpace)
import Data.Hashable (Hashable (..))
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.String (IsString (..))

import Foreign (copyBytes, plusForeignPtr)

import GHC.Exts (Addr#, Int (..))
import GHC.Exts qualified as GHC
import GHC.ForeignPtr (ForeignPtr, mallocPlainForeignPtrBytes, withForeignPtr)
import GHC.Ptr (Ptr(..), plusPtr)
import GHC.Word (Word8)

import Language.Haskell.TH (Exp (..), Lit (..), Pat (..))
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (..))

import Opal.Common.Unicode
  ( readUtf8OffPtr
  , sizeofStringUtf8
  , copyStringUtf8ToPtr
  , sizeofUtf8OffForeignPtr
  )
import Opal.Common.TH (Pattern (..))
import Opal.Memory.ForeignPtr
  ( compareOffForeignPtr
  , eqOffForeignPtr
  , hashForeignPtr
  )
import Opal.Memory.Ptr (minusPtr)
import Opal.Writer.Class (Display(..))
import Opal.Writer.Doc qualified as Doc

import Prelude hiding (foldl, foldr)

import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

--------------------------------------------------------------------------------

genSymSource :: IORef Word
genSymSource = unsafePerformIO (newIORef 0)
{-# NOINLINE genSymSource #-}

-- MonadGenSym -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
class Monad m => MonadGenSym m where
  newGenSym :: m Symbol

-- | @since 1.0.0
instance MonadGenSym IO where
  newGenSym =
    atomicModifyIORef' genSymSource \x ->
      (1 + x, stringToSymbol ('g' : show x))

-- Symbol ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Symbol = Symbol
  {-# UNPACK #-} !(ForeignPtr Word8)
  {-# UNPACK #-} !Int

-- | @since 1.0.0
instance Display Symbol where
  display x = Doc.char '\'' <> Doc.string (symbolToString x)

-- | 'Symbol' uses pointer equality.
--
-- @since 1.0.0
instance Eq Symbol where
  (==) = eqSymbol
  {-# INLINE (==) #-}

-- | @since 1.0.0
instance Ord Symbol where
  compare a b = unsafeDupablePerformIO (compareSymbol a b)
  {-# INLINE compare #-}

-- | @since 1.0.0
instance Hashable Symbol where
  hash = hashSymbol Nothing

  hashWithSalt = hashSymbol . Just

-- | @since 1.0.0
instance IsString Symbol where
  fromString = stringToSymbol

-- | @since 1.0.0
instance Lift Symbol where
  lift = pure . symbolToExp

  liftTyped = TH.unsafeCodeCoerce . lift

-- | @since 1.0.0
instance Pattern Symbol where
  liftPat = pure . symbolToPat

-- | @since 1.0.0
instance NFData Symbol where
  rnf Symbol {} = ()

-- | @since 1.0.0
instance Show Symbol where
  show x = '\'' : symbolToString x

-- Symbol - Basic Operations ---------------------------------------------------

-- | Packs the contents of a 'String' into a 'Symbol'.
--
-- @since 1.0.0
stringToSymbol :: String -> Symbol
stringToSymbol str = unsafeDupablePerformIO do
  let len = sizeofStringUtf8 str
  fp <- mallocPlainForeignPtrBytes len
  withForeignPtr fp (`copyStringUtf8ToPtr` str)
  pure (Symbol fp len)
{-# INLINE [0] stringToSymbol #-}

-- | Unpacks the contents of a 'Symbol' as a 'String'.
--
-- @since 1.0.0
symbolToString :: Symbol -> String
symbolToString = foldr (:) ""
{-# INLINE [0] symbolToString #-}

-- | TODO: docs
--
-- @since 1.0.0
symbolFromPtr :: Ptr Word8 -> Int -> Symbol
symbolFromPtr (Ptr ptr#) = symbolFromCStringLen ptr#

-- | TODO: docs
--
-- @since 1.0.0
symbolFromCString :: Addr# -> Symbol
symbolFromCString ptr# = symbolFromCStringLen ptr# (I# (GHC.cstringLength# ptr#))

-- | TODO: docs
--
-- @since 1.0.0
symbolFromCStringLen :: Addr# -> Int -> Symbol
symbolFromCStringLen src# len = unsafeDupablePerformIO do
  dst <- mallocPlainForeignPtrBytes len
  withForeignPtr dst \ptr ->
    copyBytes ptr (Ptr src#) len
  pure (Symbol dst len)

{-# RULES

"OPAL symbolFromString/unpackCString# -> symbolFromCString"
  forall (ptr# :: Addr#).
  stringToSymbol (GHC.unpackCString# ptr#) = symbolFromCString ptr#

  #-}

-- | Computes the hash of the given symbol's contents.
--
-- @since 1.0.0
hashSymbol :: Maybe Int -> Symbol -> Int
hashSymbol salt (Symbol fp len) = unsafeDupablePerformIO (hashForeignPtr salt fp len)

-- | @'(takeSymbol' n s)@ will take the first @n@ characters of the
-- given 'Symbol'.
--
-- @since 1.0.0
takeSymbol :: Int -> Symbol -> Maybe Symbol
takeSymbol n s =
  let substr :: String
      substr = take n (symbolToString s)
   in if null substr
        then Nothing
        else Just (stringToSymbol substr)

-- | @('dropSymbol' n s)@ will take the first
--
-- @since 1.0.0
dropSymbol :: Int -> Symbol -> Maybe Symbol
dropSymbol n s =
  let substr :: String
      substr = drop n (symbolToString s)
   in if null substr
        then Nothing
        else Just (stringToSymbol substr)

-- | Obtain the first character of a 'Symbol'.
--
-- @since 1.0.0
symbolHead :: Symbol -> Char
symbolHead (Symbol fp _) = fst (unsafeDupablePerformIO (withForeignPtr fp readUtf8OffPtr))

-- | Extract the tail of the 'Symbol'.
--
-- @since 1.0.0
symbolTail :: Symbol -> Maybe Symbol
symbolTail (Symbol fp len)
  | 1 < len   = unsafeDupablePerformIO do
    (_, n) <- withForeignPtr fp readUtf8OffPtr
    pure (Just (Symbol (fp `plusForeignPtr` n) (len - n)))
  | otherwise = Nothing

-- Symbol - Comparisons --------------------------------------------------------

-- | Compare the contents of the two given symbols for equality
--
-- @since 1.0.0
eqSymbol :: Symbol -> Symbol -> Bool
eqSymbol (Symbol fp1 len1) (Symbol fp2 len2)
  | len1 == len2 = unsafeDupablePerformIO (eqOffForeignPtr fp1 fp2 len1)
  | otherwise    = False

-- | Compare the contents of the two given symbols.
--
-- @since 1.0.0
compareSymbol :: Symbol -> Symbol -> IO Ordering
compareSymbol (Symbol fp1 len1) (Symbol fp2 len2) =
  let len :: Int
      len = min len1 len2
   in case unsafeDupablePerformIO (compareOffForeignPtr fp1 fp2 len) of
        EQ    -> pure (compare len1 len2)
        order -> pure order

-- Symbol - Folds --------------------------------------------------------------

-- | \(O(n)\). TODO: docs
--
-- @since 1.0.0
foldl :: forall a. (a -> Char -> a) -> a -> Symbol -> a
foldl cons nil (Symbol fp len) =
  unsafeDupablePerformIO $ withForeignPtr fp \src -> do
    let end :: Ptr Word8
        end = plusPtr src len

    let run :: a -> Ptr Word8 -> IO a
        run x ptr
          | ptr < end = do
            (c, n) <- readUtf8OffPtr ptr
            run (cons x c) (plusPtr ptr n)
          | otherwise = pure x

    run nil src

-- | \(O(n)\). TODO: docs
--
-- @since 1.0.0
foldr :: (Char -> a -> a) -> a -> Symbol -> a
foldr cons nil symbol = foldl (\k x -> GHC.oneShot \xs -> k (cons x xs)) id symbol nil
{-# INLINE foldr #-}

-- | \(O(n)\). TODO: docs
--
-- @since 1.0.0
foldr' :: (Char -> a -> a) -> a -> Symbol -> a
foldr' cons nil symbol = foldl (\k x -> GHC.oneShot \xs -> xs `seq` k (cons x xs)) id symbol nil

-- Symbol - Query --------------------------------------------------------------

-- | \(O(1)\). Obtain the size of the 'Symbol' in bytes.
--
-- @since 1.0.0
isSymbolChar :: Char -> Bool
isSymbolChar c
  | isSpace c = False
  | otherwise = not (any @[] (c ==) "(){}[]\",'`;#|\\")

-- | \(O(1)\). Obtain the size of the 'Symbol' in bytes.
--
-- @since 1.0.0
sizeofSymbol :: Symbol -> Int
sizeofSymbol (Symbol _ len) = len

-- | \(O(n)\). Obtain the size of the 'Symbol' in UTF-8 characters.
--
-- @since 1.0.0
lengthSymbol :: Symbol -> Int
lengthSymbol (Symbol fp len) = unsafeDupablePerformIO (sizeofUtf8OffForeignPtr fp len)

-- Symbol - Searching ----------------------------------------------------------


-- Symbol - Substrings ---------------------------------------------------------

-- | \(O(n)\). TODO: docs
--
-- @since 1.0.0
splitSymbol :: (Char -> Bool) -> Symbol -> Maybe (Symbol, Symbol)
splitSymbol match s@(Symbol fp len) =
  unsafeDupablePerformIO $ withForeignPtr fp \begin -> do
    let end :: Ptr Word8
        end = begin `plusPtr` len

    let run :: Ptr Word8 -> IO (Maybe (Ptr Word8))
        run ptr
          | ptr <= end = do
            (c, n) <- readUtf8OffPtr ptr
            if match c
              then pure (Just ptr)
              else run (ptr `plusPtr` n)
          | otherwise  =
            pure Nothing

    run begin >>= \case
      Nothing  -> pure Nothing
      Just ptr ->
        let n :: Int
            n = ptr `minusPtr` begin
         in pure (liftA2 (,) (takeSymbol n s) (dropSymbol n s))

-- Symbol - Template Haskell ---------------------------------------------------

-- | \(O(n)\). TODO: docs
--
-- @since 1.0.0
symbolToExp :: Symbol -> Exp
symbolToExp (Symbol fp len) = do
  let ptrE = LitE (BytesPrimL (TH.mkBytes fp 0 (fromIntegral len)))
      lenE = ConE 'I# `AppE` LitE (IntPrimL (fromIntegral len))
   in VarE 'symbolFromCStringLen `AppE` ptrE `AppE` lenE

-- | \(O(n)\). TODO: docs
--
-- @since 1.0.0
symbolToPat :: Symbol -> Pat
symbolToPat s = ViewP (VarE 'eqSymbol `AppE` symbolToExp s) (ConP 'True [] [])