module Opal.Core.SExp
  ( -- * S-Expressions
    SExp (..),
  )
where

import Data.Data (Data)
import Data.Kind (Type)
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data SExp (a :: Type) :: Type where
  SExpVal :: a -> SExp a
  SExpVar :: {-# UNPACK #-} !Name -> SExp a
  SExpApp :: SExp a -> [SExp a] -> SExp a
  SExpLet :: Map Name (SExp a) -> SExp a -> SExp a
  deriving (Data, Eq, Ord, Show)