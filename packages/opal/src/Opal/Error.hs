{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS_HADDOCK show-extensions #-}

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
  )
where

import Opal.Binding (Binding)
import Opal.Common.Symbol (Symbol)
import Opal.Error.ErrorCode (ErrorCode)
import Opal.Error.ErrorCode.TH (errorcode)
import Opal.Syntax (Identifier)
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
  deriving (Eq, Ord)

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
instance Error ErrorAmbiguous where
  errorCode _ = [errorcode| OPAL-10001 |]

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
  deriving (Eq, Ord)

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
instance Error ErrorNotBound where
  errorCode _ = [errorcode| OPAL-10003 |]

-- | @since 1.0.0
instance Show ErrorNotBound where
  show = Doc.pretty . display

-- ErrorNotInScope -------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype ErrorNotInScope = ErrorNotInScope
  { getErrorNotInScope :: Identifier }
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display ErrorNotInScope where
  display (ErrorNotInScope id) = "the identifier" <+> display id <+> "is not in scope"

-- | @since 1.0.0
instance Error ErrorNotInScope where
  errorCode _ = [errorcode| OPAL-10002 |]

-- | @since 1.0.0
instance Show ErrorNotInScope where
  show = Doc.pretty . display