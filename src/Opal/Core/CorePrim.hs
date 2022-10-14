{-# LANGUAGE OverloadedStrings #-}

module Opal.Core.CorePrim
  ( -- * Core Primitives
    CorePrim
      ( CorePrimSyntaxLocalValue
      ),
    toSymbol,
    toName,
  )
where

import Data.Data (Data)
import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | TODO
--
-- @since
data CorePrim
  = CorePrimSyntaxLocalValue
  deriving (Data, Enum, Eq, Ord, Show)

toSymbol :: CorePrim -> Symbol
toSymbol prim = Symbol.Symbol (toName prim)

toName :: CorePrim -> Name
toName CorePrimSyntaxLocalValue = "syntax-local-value"