{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Error.ErrorCode.TH
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
module Opal.Error.ErrorCode.TH
  ( -- * Template Haskell
    errorcode
  , makeErrorCode
  )
where

import Control.Monad ((>=>))

import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Void (Void)

import Language.Haskell.TH (Exp, Loc (..), Q, Name)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import Opal.Error.ErrorCode (ErrorCode (..), readErrorCode)

import Prelude hiding (id)

import Text.Megaparsec (MonadParsec (..), Parsec, errorBundlePretty, parse)
import Text.Megaparsec.Char (space)

import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word16)

-- ErrorRegistry ---------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
errorRegistry :: IORef (Map Word16 Name)
errorRegistry = unsafePerformIO (newIORef Map.empty)

-- | TODO: docs
--
-- @since 1.0.0
registerErrorCode :: ErrorCode -> Name -> Q ()
registerErrorCode (ErrorCode _ id) cons = do
  result <- TH.runIO (atomicModifyIORef' errorRegistry insertErrorCode)
  case result of
    Nothing    -> pure ()
    Just cons' -> fail ("error code " ++ show id ++ " is already registerd to " ++ show cons')
  where
    insertErrorCode :: Map Word16 Name -> (Map Word16 Name, Maybe Name)
    insertErrorCode ids = case Map.lookup id ids of
      Nothing -> (Map.insert id cons ids, Nothing)
      Just cons'
        | cons == cons' -> (ids, Nothing)
        | otherwise     -> (ids, Just cons')

-- Template Haskell ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
errorcode :: QuasiQuoter
errorcode = QuasiQuoter
  { quoteExp  = readErrorCodeQ >=> TH.lift
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

-- | TODO: docs
--
-- @since 1.0.0
readErrorCodeQ :: String -> Q ErrorCode
readErrorCodeQ input = do
  filepath <- fmap loc_filename TH.location
  case parse parser filepath input of
    Left  exn -> fail (errorBundlePretty exn)
    Right ec  -> pure ec
  where
    parser :: Parsec Void String ErrorCode
    parser = space *> readErrorCode <* space <* eof

-- | TODO: docs
--
-- @since 1.0.0
makeErrorCode :: String -> Name -> Q Exp
makeErrorCode input name = do
  ec  <- readErrorCodeQ input
  registerErrorCode ec name
  TH.lift ec