{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Text.Parse
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
module Text.Parse
  ( -- * Parse
    Parse (..),
    -- ** Basic Operations
    parse,
    runParse,
    -- * Combinators
    consume,
    single,
    string,
    try,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState (..))

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)

import Text.Parse.Error
import Text.Parse.Monad
import Text.Parse.State
import Text.Parse.Token

-- Combinators -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
consume :: Parse (Maybe Char)
consume = do
  s0 <- get
  liftIO (consumeParseState s0) >>= \case
    Nothing      -> pure Nothing
    Just (c, s1) -> Just c <$ put s1

-- | TODO: docs
--
-- @since 1.0.0
consume1 :: Parse Char
consume1 = fmap (fromMaybe '\NUL') consume

-- | TODO: docs
--
-- @since 1.0.0
single :: Char -> Parse ()
single c = do
  c' <- consume1
  when (c /= c') do
    throwError (newParseError (token c) (token c'))

-- | TODO: docs
--
-- @since 1.0.0
string :: String -> Parse ()
string = traverse_ single

-- | TODO: docs
--
-- @since 1.0.0
try :: Parse a -> Parse a
try parser = do
  s0 <- get
  catchError parser \exn -> do
    put s0
    throwError exn