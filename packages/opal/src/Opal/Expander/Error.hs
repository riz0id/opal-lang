{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Opal.Expander.Error
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'Expand' monad along with its associated read-only
-- state 'ExpandConfig'.
--
-- @since 1.0.0
module Opal.Expander.Error
  ( -- * ExpandError
    ExpandError (..)
    -- ** Basic Operations
  , throwBadSyntax
  )
where

import Control.Lens ((^.))

import Control.Monad.Except (MonadError (..))

import Data.Text (Text)

import Opal.Core (CoreForm)
import Opal.Error
  ( Error (..)
  , ErrorAmbiguous
  , ErrorNotBound (..)
  , ErrorNotInScope
  , ErrorBadSyntax (..)
  , ErrorNoModule
  )
import Opal.Error.ErrorCode.TH (makeErrorCode)
import Opal.Expander.Config (ExpansionContext)
import Opal.Reader (ReaderError)
import Opal.Syntax (Syntax, SyntaxInfo, stxInfoSource, syntaxInfo)
import Opal.Writer (Display (..), Doc, (<+>))
import Opal.Writer qualified as Doc

import Prelude hiding (id)

import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

-- ExpandError -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ExpandError
  = ExpandAmbiguous {-# UNPACK #-} !ErrorAmbiguous
    -- ^ TODO: docs
  | ExpandNotInScope {-# UNPACK #-} !ErrorNotInScope
    -- ^ TODO: docs
  | ExpandNotBound {-# UNPACK #-} !ErrorNotBound
    -- ^ TODO: docs
  | ErrorBadContext Syntax [ExpansionContext] ExpansionContext
    -- ^ TODO: docs
  | ExpandBadSyntax {-# UNPACK #-} !ErrorBadSyntax
    -- ^ TODO: docs
  | ExpandReaderError (ParseErrorBundle Text ReaderError)
    -- ^ TODO: docs
  | ExpandNoModule {-# UNPACK #-} !ErrorNoModule
    -- ^ TODO: docs
  deriving (Show)

-- | @since 1.0.0
instance Display ExpandError where
  display (ExpandAmbiguous x)  = display x
  display (ExpandNotInScope x) = display x
  display (ExpandNotBound x)   = display x
  display (ExpandBadSyntax x)  = display x
  display (ExpandNoModule x)   = display x
  display exn = case exn of
    ErrorBadContext stx ctxs ctx ->
      docExpandError (stx ^. syntaxInfo) "invalid expansion context"
        [ Doc.vsep
            [ "expanding the syntax object:"
            , Doc.line <> display stx
            ]
        , Doc.hsep
            [ "can only be expanded in a"
            , display ctxs
            , "context"
            ]
        , Doc.hsep
            [ "but was expanded in a"
            , display ctx
            , "context"
            ]
        ]
    ExpandReaderError x -> Doc.string (errorBundlePretty x)
    where
      docExpandError :: SyntaxInfo -> Doc -> [Doc] -> Doc
      docExpandError info msg notes =
        Doc.hsep
          [ maybe "<unknown source location>" display (info ^. stxInfoSource) <> Doc.char ':'
          , Doc.string "error:"
          , Doc.char '[' <> display (errorCode exn) <> Doc.char ']'
          , Doc.nest 2 (Doc.line <> msg <> Doc.nest 2 (Doc.line <> Doc.nest 2 (Doc.vsep (map (Doc.char '*' <+>) notes))))
          ]

-- | @since 1.0.0
instance Error ExpandError where
  errorCode (ExpandAmbiguous x)  = errorCode x
  errorCode (ExpandNotInScope x) = errorCode x
  errorCode (ExpandNotBound x)   = errorCode x
  errorCode (ExpandBadSyntax x)  = errorCode x
  errorCode (ExpandNoModule x)   = errorCode x
  errorCode ErrorBadContext   {} = $(makeErrorCode "OPAL-10005" 'ErrorBadContext)
  errorCode ExpandReaderError {} = $(makeErrorCode "OPAL-10006" 'ExpandReaderError)

-- ErrorBadSyntax - Basic Operations -------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
throwBadSyntax :: MonadError ExpandError m => CoreForm -> Syntax -> m a
throwBadSyntax form stx = throwError (ExpandBadSyntax (ErrorBadSyntax form stx))