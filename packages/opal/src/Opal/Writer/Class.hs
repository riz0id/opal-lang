{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Writer.Class
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
module Opal.Writer.Class
  ( -- * Display
    Display (..)
  )
where

import Opal.Writer.Doc (Doc)
import Opal.Writer.Doc qualified as Doc

import Data.Word (Word8, Word16, Word32)

-- Display ---------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
class Display a where
  -- | TODO: docs
  --
  -- @since 1.0.0
  display :: a -> Doc

  -- | TODO: docs
  --
  -- @since 1.0.0
  displayList :: [a] -> Doc
  displayList xs = Doc.char '[' <> Doc.sepMap display (Doc.string ",") xs <> Doc.char ']'

  {-# MINIMAL display #-}

-- | @since 1.0.0
instance Display Bool where
  display True  = Doc.string "True"
  display False = Doc.string "False"

-- | @since 1.0.0
instance Display Char where
  display = Doc.char

  displayList = Doc.string

-- | @since 1.0.0
instance Display Double where
  display = Doc.double

-- | @since 1.0.0
instance Display Float where
  display = Doc.float

-- | @since 1.0.0
instance Display Int where
  display = Doc.int

-- | @since 1.0.0
instance Display Word where
  display = Doc.word

-- | @since 1.0.0
instance Display Word8 where
  display = Doc.word . fromIntegral

-- | @since 1.0.0
instance Display Word16 where
  display = Doc.word . fromIntegral

-- | @since 1.0.0
instance Display Word32 where
  display = Doc.word . fromIntegral

-- | @since 1.0.0
instance Display a => Display [a] where
  display = displayList

-- | @since 1.0.0
instance Display () where
  display () = Doc.string "()"