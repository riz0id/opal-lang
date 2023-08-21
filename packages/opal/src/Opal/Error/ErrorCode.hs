{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilyDependencies   #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Error.ErrorCode
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
module Opal.Error.ErrorCode
  ( -- * Diagnostic
    Diagnostic
    -- * ErrorCode
  , ErrorCode (..)
    -- ** Optics
  , errorCodeNamespace
  , errorCodeNumber
    -- ** Readers
  , readErrorCode
  , readErrorCodeUUID
  )
where

import Data.Char (isAlpha, isUpper)
import Data.Functor (void)
import Data.Word (Word16)

import GHC.TypeLits (Nat, Symbol)

import Language.Haskell.TH (Pat (..))
import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.Lens (defineLenses)
import Opal.Common.TH (Pattern (..))
import Opal.Writer.Class (Display(..))
import Opal.Writer qualified as Doc

import Prelude hiding (id)

import Text.Megaparsec (MonadParsec (..), single)
import Text.Megaparsec.Char.Lexer (decimal)

-- Diagnostic ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
type Diagnostic :: Symbol -> Nat
type family Diagnostic e = n | n -> e where
  Diagnostic "ErrorAmbiguous"  = 10001
  Diagnostic "ErrorNotBound"   = 10002
  Diagnostic "ErrorNotInScope" = 10003
  Diagnostic "ErrorNoModule"   = 10004
  Diagnostic "ErrorBadSyntax"  = 10005

-- ErrorCode -------------------------------------------------------------------

-- | The 'ErrorCode' record is a error namespace paired with a UUID used to
-- identify the error.
--
-- @since 1.0.0
data ErrorCode = ErrorCode
  { error_code_namespace :: String
    -- ^ The namespace of the error code. The namespace is a prefix to the error
    -- code's UUID.
  , error_code_number    :: {-# UNPACK #-} !Word16
    -- ^ A unique identifier for the error code.
  }
  deriving (Eq, Lift, Ord)

$(defineLenses ''ErrorCode)

-- | @since 1.0.0
instance Display ErrorCode where
  display (ErrorCode ns id) = display ns <> Doc.char '-' <> display id

-- | @since 1.0.0
instance Pattern ErrorCode where
  liftPat (ErrorCode ns id) = do
    nsP <- liftPat ns
    idP <- liftPat id
    pure (ConP 'ErrorCode [] [nsP, idP])

-- | @since 1.0.0
instance Show ErrorCode where
  show = Doc.pretty . display

-- ErrorCode - Readers ---------------------------------------------------------

-- | Read an 'ErrorCode' from the input stream.
--
-- @since 1.0.0
readErrorCode :: (MonadFail m, MonadParsec e String m) => m ErrorCode
readErrorCode = do
  ns <- readErrorCodeNamespace
  void (single '-')
  id <- readErrorCodeUUID
  pure (ErrorCode ns id)

-- | The 'readErrorCodeNamespace' combinator reads the error code namespace.
--
-- @since 1.0.0
readErrorCodeNamespace :: (MonadFail m, MonadParsec e String m) => m String
readErrorCodeNamespace = takeWhile1P Nothing \c -> isAlpha c && isUpper c

-- | The 'readErrorCodeUUID' combinator reads an 'ErrorCode' UUID from the input
-- stream.
--
-- @since 1.0.0
readErrorCodeUUID :: (MonadFail m, MonadParsec e String m) => m Word16
readErrorCodeUUID = do
  id <- decimal
  if id < maxBoundWord16
    then pure (fromInteger id)
    else fail ("UUID out of bounds (must be less than " ++ show maxBoundWord16 ++ "): " ++ show id)
  where
    maxBoundWord16 :: Integer
    maxBoundWord16 = toInteger (maxBound :: Word16)
