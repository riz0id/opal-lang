{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Reader
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
module Opal.Reader
  ( -- * Reader
    Reader (..)
    -- ** Basic Operations
  , runFileReader
  , runStringReader
  , runTextReader
    -- ** Query Operations
  , readerFilePath
  , readerSyntaxInfo
  , readerSrcLoc
    -- * ReaderError
  , ReaderError (..)
    -- * Readers
  , readSExpOpal
  , readSyntaxOpal
    -- ** Combinators
  , readSymbol
  , tokenSyntax
  , readEnclosed
    -- ** S-Expression
  , readSExp
  , readSExpVal
  , readSExpVar
  , readSExpApp
    -- ** Datum
  , readDatum
  , readDatumBool
  , readDatumChar
  , readDatumSymbol
  , readDatumI32
  , readDatumF32
  , readDatumList
    -- ** Syntax
  , readSyntax
  , readSyntaxBool
  , readSyntaxChar
  , readSyntaxSymbol
  , readSyntaxI32
  , readSyntaxF32
  , readSyntaxList
  )
where

import Control.Applicative (Alternative (..))

import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO

import Opal.Common.Symbol (Symbol, isSymbolChar, stringToSymbol)
import Opal.Reader.Monad
import Opal.Syntax
  ( Datum (..)
  , SExp (..)
  , Syntax(..)
  , datumToSyntax
  )

import Text.Megaparsec
  ( MonadParsec (..)
  , ParseErrorBundle
  , anySingle
  , between
  , choice
  , eof
  , parse
  , single
  , takeWhile1P
  , try
  )
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer (decimal, float)

-- Reader - Basic Operations ---------------------------------------------------

-- | Read a source file as 'Syntax'.
--
-- @since 1.0.0
runFileReader ::
  -- | The source 'FilePath' the 'Text' was read from.
  FilePath ->
  -- | Returns either the read 'Syntax' or a 'ReaderError' bundle.
  IO (Either (ParseErrorBundle Text ReaderError) Syntax)
runFileReader filepath = do
  input <- Text.IO.readFile filepath
  pure (runTextReader filepath input)

-- | Read a source 'String' obtained from the given 'FilePath' as 'Syntax'.
--
-- @since 1.0.0
runStringReader ::
  -- | The source 'FilePath' the 'Text' was read from.
  FilePath ->
  -- | The source 'String' to read.
  String ->
  -- | Returns either the read 'Syntax' or a 'ReaderError' bundle.
  Either (ParseErrorBundle Text ReaderError) Syntax
runStringReader filepath = parse (unReader readSyntaxOpal) filepath . Text.pack

-- | Read a source 'Text' obtained from the given 'FilePath' as 'Syntax'.
--
-- @since 1.0.0
runTextReader ::
  -- | The source 'FilePath' the 'Text' was read from.
  FilePath ->
  -- | The source 'Text' to read.
  Text ->
  -- | Returns either the read 'Syntax' or a 'ReaderError' bundle.
  Either (ParseErrorBundle Text ReaderError) Syntax
runTextReader = parse (unReader readSyntaxOpal)

-- Readers ---------------------------------------------------------------------

-- | The complete Opal syntax object reader. This is the reader used to read a
-- source files into a syntax object.
--
-- @since 1.0.0
readSExpOpal :: Reader SExp
readSExpOpal = space *> readSExp <* eof

-- | The complete Opal syntax object reader. This is the reader used to read a
-- source files into a syntax object.
--
-- @since 1.0.0
readSyntaxOpal :: Reader Syntax
readSyntaxOpal = space *> readSyntax <* eof

-- Readers - Combinators -------------------------------------------------------

-- | Read a single 32-bit symbol 'Datum'.
--
-- @since 1.0.0
readSymbol :: MonadParsec e Text m => m Symbol
readSymbol = do
  text <- takeWhile1P Nothing isSymbolChar
  pure (stringToSymbol (Text.unpack text))

-- | Create a syntax object reader from a 'Datum' reader. The resulting syntax
-- object is created by wrapping the 'Datum' that was read by the given reader
-- and wrapping it in lexical information obtained from the reader via
-- 'readerSyntaxInfo'.
--
-- @since 1.0.0
tokenSyntax :: Reader Datum -> Reader Syntax
tokenSyntax = liftA2 datumToSyntax readerSyntaxInfo

-- | @'readEnclosed' reader@ will enclose the given @reader@ in parenthesis or
-- square brackets.
--
-- @since 1.0.0
readEnclosed :: Reader a -> Reader a
readEnclosed reader =
  choice
    [ between (single '[' *> space) (single ']' *> space) reader
    , between (single '(' *> space) (single ')' *> space) reader
    ]

-- Readers - S-Expressions -----------------------------------------------------

-- | Read an s-expression.
readSExp :: Reader SExp
readSExp = do
  sexp <- choice
    [ readSExpVal
    , readSExpVar
    , readSExpApp
    ]
  sexp <$ space

-- | Read a 'Datum' as a s-expression.
readSExpVal :: Reader SExp
readSExpVal = fmap SVal readDatumQuote

-- | Read a s-expression variable.
readSExpVar :: Reader SExp
readSExpVar = fmap SVar readSymbol

-- | Read a s-expression application.
readSExpApp :: Reader SExp
readSExpApp = fmap SApp (readEnclosed (liftA2 (:|) readSExp (many readSExp)))

-- Readers - Datum -------------------------------------------------------------

-- | Read an 'Datum' atom.
readDatum :: Reader Datum
readDatum = do
  val <- choice
    [ readDatumQuote
    , try readDatumBool
    , readDatumChar
    , try readDatumF32
    , readDatumI32
    , readDatumList readDatum
    , readDatumSymbol
    ]
  val <$ space

-- | Read a single 32-bit boolean 'Datum'.
--
-- @since 1.0.0
readDatumBool :: Reader Datum
readDatumBool = do
  void (single '#')
  readDatumTrue <|> readDatumFalse
  where
    readDatumTrue :: Reader Datum
    readDatumTrue = DatumB True <$ choice [single 't', single 'T']

    readDatumFalse :: Reader Datum
    readDatumFalse = DatumB False <$ choice [single 'f', single 'F']

-- | Read a single character 'Datum'.
--
-- @since 1.0.0
readDatumChar :: Reader Datum
readDatumChar = fmap DatumC (string "#\\" *> anySingle)

-- | Read a symbol 'Datum'.
--
-- @since 1.0.0
readDatumSymbol :: Reader Datum
readDatumSymbol = fmap DatumS readSymbol

-- | Read a single 32-bit floating point number 'Datum'.
--
-- @since 1.0.0
readDatumF32 :: Reader Datum
readDatumF32 = fmap DatumF32 float

-- | Read a single 32-bit integer 'Datum'.
--
-- @since 1.0.0
readDatumI32 :: Reader Datum
readDatumI32 = fmap DatumI32 decimal

-- | Read a list of 'Datum'.
--
-- @since 1.0.0
readDatumList :: Reader Datum -> Reader Datum
readDatumList = fmap DatumList . readEnclosed . many

-- | Read the @quote@ syntax sugar followed by any atomic 'Datum'. The resulting
-- datum @val@ is desugared to @(quote stx)@.
--
-- @since 1.0.0
readDatumQuote :: Reader Datum
readDatumQuote = do
  quote <- DatumS (stringToSymbol "quote") <$ single '\''
  datum <- readDatum
  pure (DatumList [quote, datum])

-- Readers - Syntax ------------------------------------------------------------

-- | Read a syntax object.
--
-- @since 1.0.0
readSyntax :: Reader Syntax
readSyntax = do
  stx <- choice
    [ readQuote
    , try readQuoteSyntax
    , try readSyntaxBool
    , readSyntaxChar
    , try readSyntaxF32
    , readSyntaxI32
    , readSyntaxSymbol
    , readSyntaxList readSyntax
    ]
  stx <$ space

-- | Read a single 32-bit boolean 'Syntax'.
--
-- @since 1.0.0
readSyntaxBool :: Reader Syntax
readSyntaxBool = tokenSyntax readDatumBool

-- | Read a single character 'Syntax'.
--
-- @since 1.0.0
readSyntaxChar :: Reader Syntax
readSyntaxChar = tokenSyntax readDatumChar

-- | Read a symbol 'Syntax'.
--
-- @since 1.0.0
readSyntaxSymbol :: Reader Syntax
readSyntaxSymbol = tokenSyntax readDatumSymbol

-- | Read a single 32-bit floating point number 'Syntax'.
--
-- @since 1.0.0
readSyntaxF32 :: Reader Syntax
readSyntaxF32 = tokenSyntax readDatumF32

-- | Read a single 32-bit integer 'Syntax'.
--
-- @since 1.0.0
readSyntaxI32 :: Reader Syntax
readSyntaxI32 = tokenSyntax readDatumI32

-- | Read a list of 'Syntax'.
--
-- @since 1.0.0
readSyntaxList :: Reader Syntax -> Reader Syntax
readSyntaxList reader = do
  info <- readerSyntaxInfo
  stxs <- readEnclosed (many reader)
  pure (SyntaxList stxs info)

-- | Read the @quote@ syntax sugar followed by any syntax object. The resulting
-- syntax object @stx@ is desugared to @#'(quote stx)@.
--
-- @since 1.0.0
readQuote :: Reader Syntax
readQuote = do
  info  <- readerSyntaxInfo
  quote <- tokenSyntax (DatumS (stringToSymbol "quote") <$ single '\'')
  stx   <- readSyntax
  pure (SyntaxList [quote, stx] info)

-- | Read the @quote-syntax@ syntax sugar followed by any syntax object. The
-- resulting syntax object @stx@ is desugared to @#'(quote-syntax stx)@.
--
-- @since 1.0.0
readQuoteSyntax :: Reader Syntax
readQuoteSyntax = do
  info  <- readerSyntaxInfo
  quote <- tokenSyntax (DatumS (stringToSymbol "quote-syntax") <$ string "#'")
  stx   <- readSyntax
  pure (SyntaxList [quote, stx] info)

