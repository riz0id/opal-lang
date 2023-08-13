{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Text.Parse.Monad
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
module Text.Parse.Monad
  ( -- * Parse
    Parse (..),
    -- ** Basic Operations
    parse,
    runParse,
  )
where

import Control.Applicative (Alternative (..))

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.State (MonadState (..), StateT (..))

import Data.Function ((&))

import Text.Parse.Buffer (stringToBuffer)
import Text.Parse.Error
import Text.Parse.State

-- Parse -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Parse a = Parse
  { unParse :: ExceptT ParseError (StateT ParseState IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError ParseError
    , MonadState ParseState
    )

-- | @since 1.0.0
instance Alternative Parse where
  empty = throwError mempty

  p1 <|> p2 = do
    s0 <- get
    (result1, s1) <- liftIO (runParse s0 p1)
    put s1
    case result1 of
      Left e1 -> do
        (result2, s2) <- liftIO (runParse s1 p2)
        put s2
        case result2 of
          Left e2 -> throwError (e1 <> e2)
          Right x -> pure x
      Right x -> pure x

-- Parse - Basic Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
parse :: String -> Parse a -> IO (Either ParseError a, ParseState)
parse input parser = do
  buffer <- stringToBuffer input
  runParse (newParseState buffer) parser

-- | TODO: docs
--
-- @since 1.0.0
runParse :: ParseState -> Parse a -> IO (Either ParseError a, ParseState)
runParse s0 parser = do
  unParse parser
    & runExceptT
    & flip runStateT s0
