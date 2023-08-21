{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Opal.Error
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
module Opal.Error
  ( -- * Error
    Error (..)
    -- * ErrorAmbiguous
  , ErrorAmbiguous (..)
    -- * ErrorNotBound
  , ErrorNotBound (..)
    -- * ErrorNotInScope
  , ErrorNotInScope (..)
    -- * ErrorNoModule
  , ErrorNoModule (..)
    -- * ErrorBadSyntax
  , ErrorBadSyntax (..)
  )
where

import GHC.Exts (Proxy#)
import GHC.Exts qualified as GHC
import GHC.Generics
import GHC.TypeLits (KnownNat)
import GHC.TypeLits qualified as GHC

import Opal.Binding (Binding)
import Opal.Core (CoreForm)
import Opal.Common.Symbol (Symbol)
import Opal.Error.ErrorCode (Diagnostic, ErrorCode (..))
import Opal.Syntax (Identifier, Syntax)
import Opal.Writer (Display (..), Doc, (<+>))
import Opal.Writer qualified as Doc

import Prelude hiding (id)

-- Error -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
class Error a where
  -- | TODO: docs
  --
  -- @since 1.0.0
  errorCode :: a -> ErrorCode

-- | @since 1.0.0
instance (Error1 (Rep a), Generic a) => Error (Generically a) where
  errorCode (Generically x) = errorCode1 (from x)

-- Error -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
class Error1 f where
  -- | TODO: docs
  --
  -- @since 1.0.0
  errorCode1 :: f a -> ErrorCode

-- | @since 1.0.0
instance Error1 f => Error1 (D1 c f) where
  errorCode1 (M1 x) = errorCode1 x

-- | @since 1.0.0
instance KnownNat (Diagnostic c) => Error1 (C1 ('MetaCons c i s) f) where
  errorCode1 _ = ErrorCode "OPAL" (fromIntegral (GHC.natVal' (GHC.proxy# :: Proxy# (Diagnostic c))))

-- ErrorAmbiguous --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ErrorAmbiguous = ErrorAmbiguous
  { error_ambiguous_id       :: {-# UNPACK #-} !Identifier
    -- ^ The identifier with ambiguous bindings.
  , error_ambiguous_bindings :: [Binding]
    -- ^ The ambiguous bindings bound to the identifier.
  }
  deriving (Eq, Generic, Ord)
  deriving (Error) via Generically ErrorAmbiguous

-- | @since 1.0.0
instance Display ErrorAmbiguous where
  display (ErrorAmbiguous id bindings) =
    mconcat
      [ "ambiguous bindings for the identifier" <+> display id
      , Doc.indent 2 (Doc.vsep (docBindingList bindings))
      ]
    where
      docBindingList :: [Binding] -> [Doc]
      docBindingList = map \binding -> "*" <+> display binding

-- | @since 1.0.0
instance Show ErrorAmbiguous where
  show = Doc.pretty . display

-- ErrorNotBound ---------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ErrorNotBound = ErrorNotBound
  { error_not_bound_id      :: {-# UNPACK #-} !Identifier
    -- ^ The identifier with that resolved to the generated symbol.
  , error_not_bound_binding :: {-# UNPACK #-} !Symbol
    -- ^ The generated symbol that was bound to the identifier.
  }
  deriving (Eq, Generic, Ord)
  deriving (Error) via Generically ErrorNotBound

-- | @since 1.0.0
instance Display ErrorNotBound where
  display (ErrorNotBound id b) =
    Doc.hsep
      [ "the generated symbol"
      , display b
      , "bound to the identifier"
      , display id
      , "lacks a binding in the compile-time environment"
      ]

-- | @since 1.0.0
instance Show ErrorNotBound where
  show = Doc.pretty . display

-- ErrorNotInScope -------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype ErrorNotInScope = ErrorNotInScope
  { getErrorNotInScope :: Identifier }
  deriving (Eq, Generic, Ord)
  deriving (Error) via Generically ErrorNotInScope

-- | @since 1.0.0
instance Display ErrorNotInScope where
  display (ErrorNotInScope id) = "the identifier" <+> display id <+> "is not in scope"

-- | @since 1.0.0
instance Show ErrorNotInScope where
  show = Doc.pretty . display

-- ErrorModuleNotDeclared ------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype ErrorNoModule = ErrorNoModule
  { getErrorNoModule :: Symbol }
  deriving (Eq, Generic, Ord)
  deriving (Error) via Generically ErrorNoModule

-- | @since 1.0.0
instance Display ErrorNoModule where
  display (ErrorNoModule id) = "module" <+> display id <+> "not declared"

-- | @since 1.0.0
instance Show ErrorNoModule where
  show = Doc.pretty . display

-- ErrorBadSyntax --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ErrorBadSyntax = ErrorBadSyntax
  { error_bad_syntax_form :: CoreForm
    -- ^ The identifier with that resolved to the generated symbol.
  , error_bad_syntax      :: Syntax
    -- ^ The generated symbol that was bound to the identifier.
  }
  deriving (Eq, Generic, Ord)
  deriving (Error) via Generically ErrorBadSyntax

-- | @since 1.0.0
instance Display ErrorBadSyntax where
  display (ErrorBadSyntax form stx) =
    mconcat
      [ "bad" <+> display form <+> "syntax"
      , Doc.indent 2 (display stx)
      ]

-- | @since 1.0.0
instance Show ErrorBadSyntax where
  show = Doc.pretty . display