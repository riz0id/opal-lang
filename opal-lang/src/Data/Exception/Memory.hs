{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Data.Exception.Memory
  ( -- * Memory Errors
    MemoryError (MemoryError),

    -- ** Construction
    exnInvalidArgument,
    exnMemoryExhausted,

    -- ** Query
    errorcode,
    count,
    align,

    -- ** Memory Error Codes
    MemoryErrorCode (InvalidArgument, MemoryExhausted),
  )
where

import Control.Exception (Exception, displayException)

import Data.Data (Data)
import Data.List qualified as List

import GHC.Records (HasField, getField)

import Text.Printf (PrintfArg)
import Text.Printf qualified as Text

default (Int)

-- Memory Errors ---------------------------------------------------------------

formatMemSize :: Integer -> String
formatMemSize x
  | abs x >= 2 ^ 40 = formatIntegerComma (reduce x 40) ++ "tB"
  | abs x >= 2 ^ 30 = formatIntegerComma (reduce x 30) ++ "gB"
  | abs x >= 2 ^ 20 = formatIntegerComma (reduce x 20) ++ "mB"
  | abs x >= 2 ^ 10 = formatIntegerComma (reduce x 10) ++ "kB"
  | otherwise = formatIntegerComma x ++ " bytes"
  where 
    reduce :: Integer -> Int -> Integer 
    reduce int e 
      | 0 <= int = int - (2 ^ e - 1)
      | otherwise = int + (2 ^ e - 1)

formatIntegerComma :: Integer -> String
formatIntegerComma x
  | abs x < 1000 = if 0 <= x then '+' : show (abs x) else '-' : show (abs x)
  | otherwise =
      let int = show (abs x)
          len = length int
       in case List.splitAt (rem len 3) int of
            (prefix, suffix) ->
              (if null prefix then "" else if 0 <= x then '+' : prefix ++ "," else '-' : prefix ++ ",")
                ++ List.intercalate "," (split (quot len 3) suffix)
  where
    split :: Int -> String -> [String]
    split 0 _ = []
    split i s = List.take 3 s : split (i - 1) (List.drop 3 s)

-- | TODO
--
-- @since 1.0.0
data MemoryError = MemoryError MemoryErrorCode Integer Integer
  deriving (Eq, Data)

-- | @since 1.0.0
instance Exception MemoryError where
  displayException (MemoryError InvalidArgument i j) =
    Text.printf
      "exn:io:memory<size=%s; align=%s>: invalid argument"
      (formatMemSize i)
      (formatMemSize j)
  displayException (MemoryError MemoryExhausted i j) =
    Text.printf
      "exn:io:memory<size=%s; align=%s>: memory exhausted"
      (formatMemSize i)
      (formatMemSize j)
  {-# INLINE CONLIKE displayException #-}

-- | @since 1.0.0
instance HasField "errorcode" MemoryError MemoryErrorCode where 
  getField = errorcode
  {-# INLINE CONLIKE getField #-}

-- | @since 1.0.0
instance HasField "count" MemoryError Integer where 
  getField = count
  {-# INLINE CONLIKE getField #-}

-- | @since 1.0.0
instance HasField "align" MemoryError Integer where 
  getField = align
  {-# INLINE CONLIKE getField #-}

-- | @since 1.0.0
instance PrintfArg MemoryError where
  formatArg exn fmt
    | 'v' == Text.fmtChar fmt = shows exn
    | 's' == Text.fmtChar fmt = showString (displayException exn)
    | otherwise = Text.errorBadFormat (Text.fmtChar fmt)
  {-# INLINE formatArg #-}

-- | @since 1.0.0
instance Show MemoryError where
  showsPrec i (MemoryError code x y) =
    showParen (0 < i) case code of
      InvalidArgument -> showString "MemoryError InvalidArgument " . shows x . (' ' :) . shows y
      MemoryExhausted -> showString "MemoryError MemoryExhausted " . shows x . (' ' :) . shows y
  {-# INLINE showsPrec #-}

-- Memory Errors - Construction ------------------------------------------------

-- | TODO
--
-- @since 1.0.0
exnInvalidArgument :: Integral a => a -> a -> MemoryError
exnInvalidArgument x y = MemoryError InvalidArgument (toInteger x) (toInteger y)
{-# INLINE CONLIKE exnInvalidArgument #-}

-- | TODO
--
-- @since 1.0.0
exnMemoryExhausted :: Integral a => a -> a -> MemoryError
exnMemoryExhausted x y = MemoryError MemoryExhausted (toInteger x) (toInteger y)
{-# INLINE CONLIKE exnMemoryExhausted #-}

-- Memory Errors - Query -------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
errorcode :: MemoryError -> MemoryErrorCode
errorcode (MemoryError x _ _) = x
{-# INLINE CONLIKE errorcode #-}

-- | TODO
--
-- @since 1.0.0
count :: MemoryError -> Integer
count (MemoryError _ x _) = x
{-# INLINE CONLIKE count #-}

-- | TODO
--
-- @since 1.0.0
align :: MemoryError -> Integer
align (MemoryError _ _ x) = x
{-# INLINE CONLIKE align #-}

-- Memory Error Codes ----------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data MemoryErrorCode
  = -- | TODO
    --
    -- @since 1.0.0
    InvalidArgument
  | -- | TODO
    --
    -- @since 1.0.0
    MemoryExhausted
  deriving (Bounded, Data, Enum, Eq, Ord, Show)

-- | @since 1.0.0
instance PrintfArg MemoryErrorCode where
  formatArg code fmt
    | 'v' == Text.fmtChar fmt = shows code
    | 'd' == Text.fmtChar fmt = shows (fromEnum code)
    | 's' == Text.fmtChar fmt = case code of
        InvalidArgument -> showString "invalid argument"
        MemoryExhausted -> showString "memory exhausted"
    | otherwise = Text.errorBadFormat (Text.fmtChar fmt)
  {-# INLINE formatArg #-}
