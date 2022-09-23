{-# LANGUAGE OverloadedStrings #-}

module Opal.AST.Literal
  ( -- * TODO
    Literal (BoolLit),
  )
where

import Data.Data (Data)

import Prettyprinter (Pretty, pretty)
import Prettyprinter qualified as Print

--------------------------------------------------------------------------------

newtype Literal
  = BoolLit Bool
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance Show Literal where 
  show (BoolLit x) = if x then "#t" else "#f"
  {-# INLINE show #-}

-- | @since 1.0.0
instance Pretty Literal where
  pretty (BoolLit x) = if x then "#t" else "#f"
  {-# INLINE pretty #-}

  prettyList xs = Print.parens (Print.hsep $ map pretty xs)
  {-# INLINE prettyList #-}
