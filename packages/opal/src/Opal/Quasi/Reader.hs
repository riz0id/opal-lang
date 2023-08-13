{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

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
  ( -- * Readers
    runQuasiReader
    -- * Readers
  , readQExp
  , readQuasiVar
  , readQuasiList
  , readEllipsisClass
  )
where

import Data.Functor (void)
import Data.Primitive.Array qualified as Array
import Data.Text (Text)
import Data.Text qualified as Text

import Language.Haskell.TH (Loc (..), Q)
import Language.Haskell.TH qualified as TH

import Opal.Common.Symbol
  ( eqSymbol
  , splitSymbol
  , symbolTail
  , symbolToString, symbolHead
  )
import Opal.Quasi
import Opal.Reader (Reader (..), ReaderError (..), readEnclosed, readSymbol,)

import Text.Megaparsec
import Text.Megaparsec.Char (space, string)

-- Basic Operations ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
runQuasiReader :: String -> Q QExp
runQuasiReader input = do
  filepath <- fmap loc_filename TH.location
  let text :: Text
      text = Text.pack input
   in case parse (unReader readQExp) filepath text of
        Left err -> fail (errorBundlePretty err)
        Right a  -> pure a

-- Readers ---------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
readQExp :: Reader QExp
readQExp = do
  space
  qexp <- choice
    [ try readQuasiBool
    , readQuasiVar
    , readQuasiList
    ]
  qexp <$ space

-- | TODO: docs
--
-- @since 1.0.0
readQuasiBool :: Reader QExp
readQuasiBool = do
  void (single '#')
  readDatumTrue <|> readDatumFalse
  where
    readDatumTrue :: Reader QExp
    readDatumTrue = QVal (QuasiValB True) <$ choice [single 't', single 'T']

    readDatumFalse :: Reader QExp
    readDatumFalse = QVal (QuasiValB False) <$ choice [single 'f', single 'F']

-- | TODO: docs
--
-- @since 1.0.0
readQuasiVar :: Reader QExp
readQuasiVar = do
  s <- readSymbol <* space

  ellipsis <- readEllipsisClass

  if symbolHead s == '?'
    then case symbolTail s of
      Nothing -> pure (QVal (QuasiValS s))
      Just s' -> case splitSymbol (== ':') s' of
        Nothing -> pure (QVar (QuasiVar s' QuasiClassStx ellipsis))
        Just (var, k)
          | k `eqSymbol` ":id" ->
            pure (QVar (QuasiVar var QuasiClassId ellipsis))
          | k `eqSymbol` ":bool" ->
            pure (QVar (QuasiVar var QuasiClassBool ellipsis))
          | otherwise          ->
            customFailure (ReaderError ("invalid quasi-variable class: " ++ symbolToString k))
    else pure (QVal (QuasiValS s))

-- | TODO: docs
--
-- @since 1.0.0
readQuasiList :: Reader QExp
readQuasiList = do
  list <- readEnclosed (many readQExp)
  pure (QExp (QuasiList (Array.fromList list)))

-- | TODO: docs
--
-- @since 1.0.0
readEllipsisClass :: Reader EllipsisClass
readEllipsisClass =
  choice
    [ try (EllipsisSome <$ string "...+")
    , try (EllipsisMany <$ string "...")
    , pure EllipsisNone
    ]