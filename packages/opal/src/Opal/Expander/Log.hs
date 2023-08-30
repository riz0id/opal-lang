{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Expander.Log
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
module Opal.Expander.Log
  ( -- * ExpansionLog
    ExpansionLog (..)
    -- ** Basic Operations
  , writeLog
  , writeLogs
  )
where

import Control.Monad.Writer (MonadWriter (..))

import Opal.Core (CoreForm)
import Opal.Writer (Display (..), (<+>))
import Opal.Writer qualified as Doc
import Opal.Syntax (Datum, Identifier, SExp, Syntax)

import Prelude hiding (id, log)

-- ExpansionLog ----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ExpansionLog
  = LogEnterEval SExp
    -- ^ TODO: docs
  | LogEnterCoreForm CoreForm Syntax
    -- ^ TODO: docs
  | LogExitCoreForm CoreForm Syntax
    -- ^ TODO: docs
  | LogEnterMacro Syntax
    -- ^ TODO: docs
  | LogEnterMacroExpand Syntax
    -- ^ TODO: docs
  | LogEnterParse Syntax
    -- ^ TODO: docs
  | LogExitEval Datum
    -- ^ TODO: docs
  | LogExitMacro Syntax
    -- ^ TODO: docs
  | LogExitMacroExpand Syntax
    -- ^ TODO: docs
  | LogExitParse SExp
    -- ^ TODO: docs
  | LogParse Syntax
    -- ^ TODO: docs
  | LogResolveId Identifier
    -- ^ TODO: docs
  | LogVariable Identifier
    -- ^ TODO: docs
  | LogVisitSyntax Syntax
    -- ^ TODO: docs
  deriving (Show)

-- | @since 1.0.0
instance Display ExpansionLog where
  display (LogEnterEval stx)        = "enter-eval" <+> display stx
  display (LogEnterCoreForm c stx)  = Doc.parens ("enter-core-form" <+> display c) <+> display stx
  display (LogExitCoreForm c stx)   = Doc.parens ("exit-core-form" <+> display c) <+> display stx
  display (LogEnterMacro stx)       = "enter-macro" <+> display stx
  display (LogEnterMacroExpand stx) = "enter-macro-expand" <+> display stx
  display (LogEnterParse stx)       = "enter-parse" <+> display stx
  display (LogExitEval stx)         = "exit-eval" <+> display stx
  display (LogExitMacro stx)        = "exit-macro" <+> display stx
  display (LogExitMacroExpand stx)  = "exit-macro-expand" <+> display stx
  display (LogExitParse stx)        = "exit-parse" <+> display stx
  display (LogParse stx)            = "parse" <+> display stx
  display (LogResolveId id)         = "resolve-id" <+> display id
  display (LogVariable id)          = "variable" <+> display id
  display (LogVisitSyntax stx)      = "visit-syntax" <+> display stx

  displayList = Doc.vsep . map display

-- ExpansionLog - Basic Operations ---------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
writeLog :: MonadWriter [ExpansionLog] m => ExpansionLog -> m ()
writeLog log = writeLogs [log]

-- | TODO: docs
--
-- @since 1.0.0
writeLogs :: MonadWriter [ExpansionLog] m => [ExpansionLog] -> m ()
writeLogs = tell
