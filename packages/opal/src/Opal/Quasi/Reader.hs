{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  Opal.Quasi.Reader
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
module Opal.Quasi.Reader
  ( -- * QuasiReader
    QuasiReader (..)
    -- ** Basic Operations
  , runQuasiReader
    -- ** Error Operations
  , throwUnexpected
  , errorLabel
    -- ** Combinators
  , anyIdentifier
  , satisfyIdentifier
  , identifier
    -- * QuasiReader
  , QuasiReaderError (..)
    -- * Readers
  , readQExp
  , readQuasiVar
  , readQuasiList
  , readEllipsisClass
  )
where

import Control.Applicative (Alternative(..))

import Control.Lens ((^.))

import Control.Monad (MonadPlus(..))

import Data.Primitive.Array qualified as Array
import Data.List.NonEmpty (NonEmpty)
import Data.Void (Void)

import Opal.Common.NonEmpty.TH (nonEmptyString)
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol
import Opal.Quasi
import Opal.Syntax (Datum (..), Identifier (..), Syntax)
import Opal.Syntax qualified as Syntax
import Opal.Reader (runStringReader)

import Text.Megaparsec (ErrorItem (..), MonadParsec (..), Parsec)
import Text.Megaparsec qualified as Parsec

-- QuasiReader -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype QuasiReader a = QuasiReader
  { unQuasiReader :: Parsec Void [Syntax] a }
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadParsec Void [Syntax]
    )

-- QuasiReader - Basic Operations ----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
runQuasiReader :: FilePath -> String -> QuasiReader a -> IO a
runQuasiReader filepath input reader = do
  stxs <- case runStringReader filepath input of
    Left  exn -> error (Parsec.errorBundlePretty exn)
    Right stx -> pure [stx]

  case Parsec.parse (unQuasiReader reader) filepath stxs of
    Left  e -> error (show e)
    Right x -> pure x

-- QuasiReader - Error Operations ----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
throwUnexpected :: Syntax -> QuasiReader a
throwUnexpected = Parsec.unexpected . Label . errorLabel

-- | TODO: docs
--
-- @since 1.0.0
errorLabel :: Syntax -> NonEmpty Char
errorLabel stx = case Syntax.syntaxToDatum stx of
  DatumB    {}   -> [nonEmptyString| "bool" |]
  DatumC    {}   -> [nonEmptyString| "char" |]
  DatumS    {}   -> [nonEmptyString| "identifier" |]
  DatumF32  {}   -> [nonEmptyString| "f32" |]
  DatumI32  {}   -> [nonEmptyString| "i32" |]
  DatumLam  {}   -> [nonEmptyString| "lambda" |]
  DatumList {}   -> [nonEmptyString| "list" |]
  DatumStx  stx' -> errorLabel stx'

-- QuasiReader - Combinators ---------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
withInput :: [Syntax] -> QuasiReader a -> QuasiReader a
withInput newInput parser = do
  prevInput <- Parsec.getInput
  Parsec.setInput newInput
  result <- parser
  result <$ Parsec.setInput prevInput

-- | TODO: docs
--
-- @since 1.0.0
anyIdentifier :: QuasiReader Identifier
anyIdentifier = do
  stx <- Parsec.anySingle
  case Syntax.syntaxToIdentifier stx of
    Nothing  -> throwUnexpected stx
    Just idt -> pure idt

-- | TODO: docs
--
-- @since 1.0.0
syntaxList :: QuasiReader a -> QuasiReader [a]
syntaxList reader = do
  stx <- Parsec.anySingle
  case Syntax.syntaxToDatum stx of
    DatumList vals -> do
      let stxs = map (Syntax.datumToSyntax (stx ^. Syntax.stxInfo)) vals
      withInput stxs (many reader)
    _              ->
      throwUnexpected stx

-- | TODO: docs
--
-- @since 1.0.0
satisfyIdentifier :: (Identifier -> Bool) -> QuasiReader Identifier
satisfyIdentifier match = do
  idt <- anyIdentifier
  if match idt
    then pure idt
    else throwUnexpected (Syntax.identifierToSyntax idt)

-- | TODO: docs
--
-- @since 1.0.0
identifier :: Symbol -> QuasiReader Identifier
identifier s =
  satisfyIdentifier \idt ->
    Symbol.eqSymbol (idt ^. Syntax.idtSymbol) s

-- QuasiReaderError ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data QuasiReaderError = QuasiReaderError
  deriving (Eq, Ord, Show)

-- Readers ---------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
readQExp :: QuasiReader QExp
readQExp =
  Parsec.choice
    [ try (fmap QVal readQuasiVal)
    , try (fmap QVar readQuasiVar)
    , fmap QExp readQuasiList
    ]

-- | TODO: docs
--
-- @since 1.0.0
readQuasiVal :: QuasiReader QuasiVal
readQuasiVal = do
  Parsec.choice
    [ fmap QuasiValS readQuasiSymbol
    ]

-- | TODO: docs
--
-- @since 1.0.0
readQuasiSymbol :: QuasiReader Symbol
readQuasiSymbol = do
  idt <- satisfyIdentifier \idt ->
    Symbol.symbolHead (idt ^. Syntax.idtSymbol) /= '?'
  pure (idt ^. Syntax.idtSymbol)

-- | TODO: docs
--
-- @since 1.0.0
readQuasiVar :: QuasiReader QuasiVar
readQuasiVar = do
  idt <- satisfyIdentifier \idt ->
    Symbol.symbolHead (idt ^. Syntax.idtSymbol) == '?'

  (var, k) <- case Symbol.symbolTail (idt ^. Syntax.idtSymbol) of
    Nothing -> throwUnexpected (Syntax.identifierToSyntax idt)
    Just s  -> case Symbol.splitSymbol (== ':') s of
      Nothing -> pure (s, QuasiClassStx)
      Just (s', k)
        | k `Symbol.eqSymbol` ":id" -> pure (s', QuasiClassId)
        | otherwise                 -> throwUnexpected (Syntax.identifierToSyntax idt)

  ellipsis <- readEllipsisClass

  pure (QuasiVar var k ellipsis)

-- | TODO: docs
--
-- @since 1.0.0
readQuasiList :: QuasiReader QuasiList
readQuasiList = do
  exps <- syntaxList readQExp
  pure (QuasiList (Array.fromList exps))

-- | TODO: docs
--
-- @since 1.0.0
readEllipsisClass :: QuasiReader EllipsisClass
readEllipsisClass =
  Parsec.choice
    [ try (EllipsisSome <$ identifier "...+")
    , try (EllipsisMany <$ identifier "...")
    , pure EllipsisNone
    ]