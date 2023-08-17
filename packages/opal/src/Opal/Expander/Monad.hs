{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Opal.Expander.Monad
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
module Opal.Expander.Monad
  ( -- * Expand
    Expand (..)
    -- ** Basic Operations
  , runExpand
  , logExpand
  , resolveId
  , lookupEnvironment
  , withTopLevelContext
  , withModuleContext
  , withModuleBeginContext
  , withDefinitionContext
  , withExpressionContext
    -- * ExpandConfig
  , ExpandConfig (..)
    -- ** Basic Operations
  , coreExpandConfig
    -- ** Lenses
  , expandEnvironment
  , expandCurrentPhase
  , expandContext
    -- * ExpandError
  , ExpandError (..)
    -- * ExpandState
  , ExpandState (..)
    -- ** Lenses
  , expandBindingStore
  , expandNamespace
  , expandIntroScopes
  , expandUsageScopes
    -- * ExpansionContext
  , ExpansionContext (..)
    -- ** Basic Operations
  , expansionContextSymbol
  , expansionContextString
    -- * ExpansionLog
  , ExpansionLog (..)
  )
where

import Control.Lens (set, use, view, (^.))

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), StateT(..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))

import Data.Function ((&))
import Data.Text (Text)

import GHC.Exts (RealWorld)

import Opal.Binding.Environment qualified as Environment
import Opal.Common.Scope (MonadScope (..))
import Opal.Common.Symbol (MonadGenSym (..), Symbol)
import Opal.Error
  ( Error (..)
  , ErrorAmbiguous
  , ErrorNotBound (..)
  , ErrorNotInScope
  )
import Opal.Error.ErrorCode.TH (makeErrorCode)
import Opal.Reader (ReaderError)
import Opal.Resolve (ResolveError (..), resolve)
import Opal.Syntax
import Opal.Syntax.CoreForm (CoreForm)
import Opal.Syntax.Transformer (Transformer(..))
import Opal.Writer (Display (..), Doc, (<+>))
import Opal.Writer qualified as Doc
import Opal.Expander.State
  ( ExpandState(..)
  , expandBindingStore
  , expandEnvironment
  , expandIntroScopes
  , expandNamespace
  , expandUsageScopes
  )
import Opal.Expander.Config
  ( ExpandConfig (..)
  , ExpansionContext (..)
  , coreExpandConfig
  , expandCurrentPhase
  , expandContext
  , expansionContextString
  , expansionContextSymbol
  )

import Prelude hiding (id)

import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

-- Expand -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Expand a = Expand
  { unExpand :: ReaderT ExpandConfig (StateT ExpandState (ExceptT ExpandError (WriterT [ExpansionLog] IO))) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError ExpandError
    , MonadReader ExpandConfig
    , MonadState ExpandState
    , MonadWriter [ExpansionLog]
    )

-- | @since 1.0.0
instance MonadGenSym Expand where
  newGenSym = liftIO newGenSym

-- | @since 1.0.0
instance MonadScope Expand where
  newScope = liftIO newScope

-- | @since 1.0.0
instance PrimMonad Expand where
  type PrimState Expand = RealWorld

  primitive = liftIO . primitive

-- Expand - Basic Operations ----------------------------------------------------

-- | Run an 'Expand' computation with the given 'ExpandConfig' and initial
-- 'ExpandState'.
--
-- @since 1.0.0
runExpand ::
  ExpandConfig ->
  ExpandState ->
  Expand a ->
  IO (Either ExpandError (a, ExpandState), [ExpansionLog])
runExpand c s0 expand =
  unExpand expand
    & flip runReaderT c
    & flip runStateT s0
    & runExceptT
    & runWriterT

-- | TODO: docs
--
-- @since 1.0.0
logExpand :: ExpansionLog -> Expand ()
logExpand x = tell [x]

-- | TODO: docs
--
-- @since 1.0.0
resolveId :: Identifier -> Expand Symbol
resolveId id = do
  phase <- view expandCurrentPhase
  store <- use expandBindingStore
  logExpand (LogResolveId id)
  case resolve phase id store of
    Left  exn -> throwError (resolveToExpandError exn)
    Right s   -> pure s
  where
    resolveToExpandError :: ResolveError -> ExpandError
    resolveToExpandError (ResolveErrorAmbiguous exn)  = ExpandAmbiguous exn
    resolveToExpandError (ResolveErrorNotInScope exn) = ExpandNotInScope exn

-- | TODO: docs
--
-- @since 1.0.0
lookupEnvironment :: Identifier -> Expand Transformer
lookupEnvironment id = do
  b   <- resolveId id
  env <- use expandEnvironment
  case Environment.lookup b env of
    Nothing -> throwError (ExpandNotBound (ErrorNotBound id b))
    Just x  -> pure x

-- | TODO: docs
--
-- @since 1.0.0
withTopLevelContext :: Expand a -> Expand a
withTopLevelContext = local (set expandContext ContextTopLevel)

-- | TODO: docs
--
-- @since 1.0.0
withModuleContext :: Expand a -> Expand a
withModuleContext = local (set expandContext ContextModule)

-- | TODO: docs
--
-- @since 1.0.0
withModuleBeginContext :: Expand a -> Expand a
withModuleBeginContext = local (set expandContext ContextModuleBegin)

-- | TODO: docs
--
-- @since 1.0.0
withDefinitionContext :: Expand a -> Expand a
withDefinitionContext = local (set expandContext ContextDefinition)

-- | TODO: docs
--
-- @since 1.0.0
withExpressionContext :: Expand a -> Expand a
withExpressionContext = local (set expandContext ContextExpression)

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
  | ErrorBadSyntax CoreForm {-# UNPACK #-} !Syntax
    -- ^ TODO: docs
  | ExpandReaderError (ParseErrorBundle Text ReaderError)
    -- ^ TODO: docs
  deriving (Show)

-- | @since 1.0.0
instance Display ExpandError where
  display (ExpandAmbiguous x)  = display x
  display (ExpandNotInScope x) = display x
  display (ExpandNotBound x)   = display x
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
    ErrorBadSyntax core stx ->
      docExpandError (stx ^. syntaxInfo) ("bad" <+> display core <+> "syntax")
        [ Doc.vsep
            [ "while expanding the syntax object:"
            , Doc.line <> display stx
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
  errorCode ErrorBadContext {}   = $(makeErrorCode "OPAL-10004" 'ErrorBadContext)
  errorCode ErrorBadSyntax {}    = $(makeErrorCode "OPAL-10005" 'ErrorBadSyntax)
  errorCode ExpandReaderError {} = $(makeErrorCode "OPAL-10006" 'ExpandReaderError)

-- ExpansionLog ----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ExpansionLog
  = LogEnterEval SExp
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
