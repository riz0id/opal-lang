{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Writer
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
module Opal.Writer
  ( -- * Basic Operations
    pretty
  , pretty'
    -- * IO Operations
  , putDoc
  , putDocLn
  , hPutDoc
  , hPutDocLn
  , module Opal.Writer.Class
  , module Opal.Writer.Doc
  )
where

import Opal.Writer.Class
import Opal.Writer.Doc

import Prelude hiding (concat)

import System.IO (Handle, hPutStr, hPutStrLn, stdout)

-- Basic Operations -------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
pretty :: Doc -> String
pretty = pretty' 80

-- | TODO: docs
--
-- @since 1.0.0
pretty' :: Int -> Doc -> String
pretty' w = layoutDocStream . bestFitDoc w 0

-- IO Operations ---------------------------------------------------------------

-- | Writes given 'Doc' to 'stdout'. The document is fit to the specified
-- maximum document width.
--
-- @since 1.0.0
putDoc :: Int -> Doc -> IO ()
putDoc = hPutDoc stdout

-- | Writes given 'Doc' to 'stdout' followed by a newline character. The
-- document is fit to the specified maximum document width.
--
-- @since 1.0.0
putDocLn :: Int -> Doc -> IO ()
putDocLn = hPutDocLn stdout

-- | Writes given 'Doc' to the file 'Handle'. The document is fit to the
-- specified maximum document width.
--
-- @since 1.0.0
hPutDoc :: Handle -> Int -> Doc -> IO ()
hPutDoc handle w = hPutStr handle . pretty' w

-- | Writes given 'Doc' to the file 'Handle' followed by a newline character.
-- The document is fit to the specified maximum document width.
--
-- @since 1.0.0
hPutDocLn :: Handle -> Int -> Doc -> IO ()
hPutDocLn handle w = hPutStrLn handle . pretty' w
