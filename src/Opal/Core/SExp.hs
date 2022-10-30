{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Opal.Core.SExp (
  -- * S-Expressions
  SExp (..),
) where

import Data.Data (Data)
import Data.Map.Internal (Map (..))
import Data.List.NonEmpty (NonEmpty)

import Language.Haskell.TH.Syntax (Lift)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

--------------------------------------------------------------------------------

deriving instance (Lift a, Lift b) => Lift (Map a b)

-- | TODO
--
-- @since 1.0.0
data SExp a
  = SExpVal a
  | SExpVar {-# UNPACK #-} !Name
  | SExpApp (SExp a) [SExp a]
  | SExpLet (Map Name (SExp a)) (NonEmpty (SExp a))
  | SExpIf (SExp a) (SExp a) (SExp a)
  deriving (Data, Eq, Ord, Lift, Show)