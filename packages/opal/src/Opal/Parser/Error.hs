{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Parser.Error
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
module Opal.Parser.Error
  ( -- * ParseError
    ParseError (..)
    -- ** Basic Operations
  , throwBadSyntax
  )
where

import Control.Lens (view, (^.))

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))

import Opal.Core (CoreForm (..))
import Opal.Error (ErrorAmbiguous (..), ErrorBadSyntax (..))
import Opal.Syntax (Syntax)

import Prelude hiding (id)

-- ParseError ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ParseError
  = ParseAmbiguous {-# UNPACK #-} !ErrorAmbiguous
    -- ^ TODO: docs
  | ParseBadSyntax {-# UNPACK #-} !ErrorBadSyntax
    -- ^ TODO: docs
  deriving (Show)

-- ParseError - Basic Operations -----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
throwBadSyntax :: MonadError ParseError m => CoreForm -> Syntax -> m a
throwBadSyntax form stx = throwError (ParseBadSyntax (ErrorBadSyntax form stx))