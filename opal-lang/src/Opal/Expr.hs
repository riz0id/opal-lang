{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Opal.Expr
  ( -- * Expression
    Expr (DtmExp, VarExp, AppExp),

    -- * Datum
    Datum (LitDtm, FunDtm, StxDtm, ListDtm),

    -- * TODO
    stx'exp,
    stx'new,
    stx'datum,
  )
where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

import Prettyprinter (Pretty, pretty, prettyList, (<+>))
import Prettyprinter qualified as Print

--------------------------------------------------------------------------------

import Opal.AST.Literal (Literal)

import Opal.Common.Symbol (Symbol)

import Opal.Expand.Syntax (Syntax)
import Opal.Expand.Syntax qualified as Syntax

-- Expression ------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Expr
  = DtmExp Datum
  | VarExp Symbol
  | AppExp (NonEmpty Expr)
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance Pretty Expr where
  pretty (VarExp x) = Print.viaShow x
  pretty (DtmExp x) = pretty x
  pretty (AppExp xs) = prettyList (NonEmpty.toList xs)
  {-# INLINE pretty #-}

  prettyList xs = Print.parens (Print.hsep $ map pretty xs)
  {-# INLINE prettyList #-}

-- | @since 1.0.0
instance Show Expr where
  show (VarExp x) = show x
  show (DtmExp x) = show x
  show (AppExp xs) = "(" ++ unwords (map show $ NonEmpty.toList xs) ++ ")"
  {-# INLINE show #-}

-- Datum -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Datum
  = LitDtm Literal
  | StxDtm Syntax
  | FunDtm [Symbol] Expr
  | ListDtm [Datum]
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance Pretty Datum where
  pretty (LitDtm x) = pretty x
  pretty (StxDtm x) = pretty x
  pretty (FunDtm x e) = Print.parens ("λ" <+> Print.parens (pretty x) <+> pretty e)
  pretty (ListDtm xs) = prettyList xs
  {-# INLINE pretty #-}

  prettyList xs = Print.parens (Print.hsep $ map pretty xs)
  {-# INLINE prettyList #-}

-- | @since 1.0.0
instance Show Datum where
  show (LitDtm x) = show x
  show (StxDtm x) = show x
  show (FunDtm x e) = "(λ " ++ shows x " " ++ shows e ")"
  show (ListDtm xs) = "'(" ++ unwords (map show xs) ++ ")"
  {-# INLINE show #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
stx'exp :: Syntax -> Datum
stx'exp (Syntax.Lit _ lit) = LitDtm lit
stx'exp (Syntax.Idt idt) = StxDtm (Syntax.Idt idt) 
stx'exp (Syntax.App _ stxs) = ListDtm (map StxDtm stxs)

-- | TODO
--
-- @since 1.0.0
stx'new :: Symbol -> Syntax -> Syntax
stx'new symbol stx = Syntax.Idt (Syntax.StxIdt stx.context symbol)

-- | TODO
--
-- @since 1.0.0
stx'datum :: Syntax -> Datum
stx'datum (Syntax.Lit _ lit) = LitDtm lit
stx'datum (Syntax.Idt idt) = StxDtm (Syntax.Idt idt)
stx'datum (Syntax.App _ sxs) = ListDtm (map stx'datum sxs)