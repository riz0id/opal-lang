{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS_HADDOCK show-extensions #-}

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
    -- * ExpandConfig
  , ExpandConfig (..)
    -- ** Basic Operations
  , coreExpandConfig
  , resolveId
  , lookupEnvironment
  , withTopLevelContext
  , withModuleContext
  , withModuleBeginContext
  , withDefinitionContext
  , withExpressionContext
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
  )
where

import Control.Lens (Lens', lens, set, use, view, (^.))

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), StateT(..))

import Data.Default (Default (..))
import Data.Function ((&))

import GHC.Generics (Generic)

import Opal.Binding.BindingStore (BindingStore)
import Opal.Binding.BindingStore qualified as BindingStore
import Opal.Binding.Environment (Environment, coreEnvironment)
import Opal.Binding.Environment qualified as Environment
import Opal.Common.Phase (Phase)
import Opal.Common.Scope (MonadScope (..))
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.Symbol (MonadGenSym (..), Symbol, stringToSymbol)
import Opal.Error (Error (..), ErrorAmbiguous, ErrorNotInScope)
import Opal.Error.ErrorCode.TH (makeErrorCode)
import Opal.Module (Namespace (..))
import Opal.Resolve (ResolveError (..), resolve)
import Opal.Syntax
import Opal.Syntax.Transformer (Transformer(..))
import Opal.Writer (Display (..), Doc, (<+>))
import Opal.Writer qualified as Doc

import Prelude hiding (id)

-- Expand -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Expand a = Expand
  { unExpand :: ReaderT ExpandConfig (StateT ExpandState (ExceptT ExpandError IO)) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError ExpandError
    , MonadReader ExpandConfig
    , MonadState ExpandState
    )

-- | @since 1.0.0
instance MonadGenSym Expand where
  newGenSym = liftIO newGenSym

-- | @since 1.0.0
instance MonadScope Expand where
  newScope = liftIO newScope

-- Expand - Basic Operations ----------------------------------------------------

-- | Run an 'Expand' computation with the given 'ExpandConfig' and initial
-- 'ExpandState'.
--
-- @since 1.0.0
runExpand ::
  ExpandConfig ->
  ExpandState ->
  Expand a ->
  IO (Either ExpandError (a, ExpandState))
runExpand c s0 expand =
  unExpand expand
    & flip runReaderT c
    & flip runStateT s0
    & runExceptT

-- | TODO: docs
--
-- @since 1.0.0
resolveId :: Identifier -> Expand Symbol
resolveId id = do
  phase <- view expandCurrentPhase
  store <- use expandBindingStore
  case resolve phase id store of
    Left  exn -> throwError (resolveToExpandError exn)
    Right s   -> pure s
  where
    resolveToExpandError :: ResolveError -> ExpandError
    resolveToExpandError (ResolveErrorAmbiguous exn)  = ExpandErrorAmbiguous exn
    resolveToExpandError (ResolveErrorNotInScope exn) = ExpandErrorNotInScope exn

-- | TODO: docs
--
-- @since 1.0.0
lookupEnvironment :: Identifier -> Expand Transformer
lookupEnvironment id = do
  binding <- resolveId id
  env     <- view expandEnvironment
  case Environment.lookup binding env of
    Nothing -> throwError (ErrorNotBound id binding)
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

-- ExpandConfig -----------------------------------------------------------------

-- | 'ExpandConfig' is the read-only state of the 'Expand' monad.
--
-- @since 1.0.0
data ExpandConfig = ExpandConfig
  { expand_environment   :: Environment
    -- ^ The expanders's compile-time environment. This maps bindings to their
    -- compile-time meanings. The 'Symbol' key is a generated symbol obtained
    -- from the expanders's binding store.
  , expand_current_phase :: {-# UNPACK #-} !Phase
    -- ^ The current expansion phase.
  , expand_context       :: ExpansionContext
    -- ^ The current expansion context.
  }
  deriving (Generic, Show)

-- | 'ExpandConfig' defaults to 'coreExpandConfig'.
--
-- @since 1.0.0
instance Default ExpandConfig where
  def = coreExpandConfig

-- ExpandConfig - Basic Operations ---------------------------------------------

-- | The default 'ExpandConfig' with bindings for the core syntactic forms.
--
-- @since 1.0.0
coreExpandConfig :: ExpandConfig
coreExpandConfig =
  ExpandConfig
    { expand_environment   = coreEnvironment
    , expand_current_phase = def
    , expand_context       = def
    }

-- ExpandConfig - Lenses -------------------------------------------------------

-- | Lens focusing on the 'expand_environment' field of 'ExpandConfig'.
--
-- @since 1.0.0
expandEnvironment :: Lens' ExpandConfig Environment
expandEnvironment = lens expand_environment \s x -> s { expand_environment = x }

-- | Lens focusing on the 'expand_current_phase' field of 'ExpandConfig'.
--
-- @since 1.0.0
expandCurrentPhase :: Lens' ExpandConfig Phase
expandCurrentPhase = lens expand_current_phase \s x -> s { expand_current_phase = x }

-- | Lens focusing on the 'expand_context' field of 'ExpandConfig'.
--
-- @since 1.0.0
expandContext :: Lens' ExpandConfig ExpansionContext
expandContext = lens expand_context \s x -> s { expand_context = x }

-- ExpandError -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ExpandError
  = ExpandErrorAmbiguous {-# UNPACK #-} !ErrorAmbiguous
    -- ^ TODO: docs
  | ExpandErrorNotInScope {-# UNPACK #-} !ErrorNotInScope
    -- ^ TODO: docs
  | ErrorNotBound {-# UNPACK #-} !Identifier {-# UNPACK #-} !Symbol
    -- ^ TODO: docs
  | ErrorBadContext Syntax [ExpansionContext] ExpansionContext
    -- ^ TODO: docs
  | ErrorBadSyntax {-# UNPACK #-} !Syntax
    -- ^ TODO: docs
  deriving (Show)

-- | @since 1.0.0
instance Display ExpandError where
  display (ExpandErrorAmbiguous  x) = display x
  display (ExpandErrorNotInScope x) = display x
  display exn = case exn of
    ErrorNotBound id s ->
      docExpandError (id ^. idtInfo) "not bound"
        [ "the identifier" <+> display id
        , "lacks a binding to the generated symbol" <+> display s
        ]
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
    ErrorBadSyntax stx ->
      docExpandError (stx ^. syntaxInfo) "bad syntax"
        [ Doc.vsep
            [ "while expanding the syntax object:"
            , Doc.line <> display stx
            ]
        ]
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
  errorCode (ExpandErrorAmbiguous x)  = errorCode x
  errorCode (ExpandErrorNotInScope x) = errorCode x
  errorCode ErrorNotBound   {} = $(makeErrorCode "EXPAND-10003" 'ErrorNotBound)
  errorCode ErrorBadContext {} = $(makeErrorCode "EXPAND-10004" 'ErrorBadContext)
  errorCode ErrorBadSyntax  {} = $(makeErrorCode "EXPAND-10005" 'ErrorBadSyntax)

-- ExpandState -----------------------------------------------------------------

-- | 'ExpandState' is the read-only state of the 'Expand' monad.
--
-- @since 1.0.0
data ExpandState = ExpandState
  { expand_binding_store :: BindingStore
    -- ^ A binding store that is threaded through the expander to substitute
    -- identifiers with the generated symbols they are bound to.
  , expand_namespace     :: {-# UNPACK #-} !Namespace
    -- ^ TODO: docs
  , expand_intro_scopes  :: ScopeSet
  -- ^ The set of scopes to be pruned at syntax forms.
  , expand_usage_scopes  :: ScopeSet
  -- ^ A subset of the current expansion context's use-stire scopes.
  }
  deriving (Generic, Show)

-- | 'ExpandState' defaults to 'coreExpandState'.
--
-- @since 1.0.0
instance Default ExpandState where
  def = coreExpandState

-- ExpandState - Basic Operations ----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
coreExpandState :: ExpandState
coreExpandState =
  ExpandState
    { expand_binding_store = BindingStore.coreBindingStore
    , expand_namespace     = def
    , expand_intro_scopes  = def
    , expand_usage_scopes  = def
    }

-- ExpandState - Lenses --------------------------------------------------------

-- | Lens focusing on the 'expand_binding_store' field of 'ExpandState'.
--
-- @since 1.0.0
expandBindingStore :: Lens' ExpandState BindingStore
expandBindingStore = lens expand_binding_store \s x -> s { expand_binding_store = x }

-- | Lens focusing on the 'expand_namespace' field of 'ExpandState'.
--
-- @since 1.0.0
expandNamespace :: Lens' ExpandState Namespace
expandNamespace = lens expand_namespace \s x -> s { expand_namespace = x }

-- | Lens focusing on the 'expand_intro_scopes' field of 'ExpandState'.
--
-- @since 1.0.0
expandIntroScopes :: Lens' ExpandState ScopeSet
expandIntroScopes = lens expand_intro_scopes \s x -> s { expand_intro_scopes = x }

-- | Lens focusing on the 'expand_usage_scopes' field of 'ExpandState'.
--
-- @since 1.0.0
expandUsageScopes :: Lens' ExpandState ScopeSet
expandUsageScopes = lens expand_usage_scopes \s x -> s { expand_usage_scopes = x }

-- ExpansionContext ------------------------------------------------------------

-- | 'ExpansionContext' is an enumeration of the various expansion contexts.
--
-- @since 1.0.0
data ExpansionContext
  = ContextDefinition
    -- ^ The definition-level expansion context.
  | ContextExpression
    -- ^ The expression-level expansion context.
  | ContextModule
    -- ^ The module-level expansion context.
  | ContextModuleBegin
    -- ^ The module body expansion context.
  | ContextTopLevel
    -- ^ The top-level expansion context.
  deriving (Bounded, Enum, Eq, Ord)

-- | @since 1.0.0
instance Default ExpansionContext where
  def = ContextTopLevel

-- | @since 1.0.0
instance Display ExpansionContext where
  display = Doc.string . expansionContextString

-- | @since 1.0.0
instance Show ExpansionContext where
  show = Doc.pretty 80 . display

-- ExpansionContext ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
expansionContextSymbol :: ExpansionContext -> Symbol
expansionContextSymbol = stringToSymbol . expansionContextString

-- | TODO: docs
--
-- @since 1.0.0
expansionContextString :: ExpansionContext -> String
expansionContextString ContextExpression  = "expression"
expansionContextString ContextTopLevel    = "top-level"
expansionContextString ContextModule      = "module"
expansionContextString ContextModuleBegin = "module-begin"
expansionContextString ContextDefinition  = "definition"
