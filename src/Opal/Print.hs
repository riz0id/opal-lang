{-# LANGUAGE OverloadedStrings #-}

module Opal.Print (
  -- * TODO
  pprDecl,
  pprDatum,
  pprExpr,
  pprSyntax,

  -- * TODO
  docDecl,

  -- * TODO
  docExpr,
  docSExpVar,
  docSExpApp,

  -- * TODO
  docDatum,

  -- * TODO
  docSyntax,
  docStxCtx,
  docStxAtom,
  docStxList,
) where

import Data.SrcLoc (SrcLoc (coln, line))
import Data.Text (Text)
import Data.Text qualified as Text

import Text.Emit (Doc, emit, (<+>))
import Text.Emit qualified as Emit

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Name qualified as Name
import Opal.Common.Symbol (Symbol)

import Opal.Core (Decl (..), Expr,SExp (..))
import Opal.Core.Datum (Datum)
import Opal.Core.Datum qualified as Datum

import Opal.Core.Prim qualified as Core.Prim
import Opal.Expand.Syntax (StxCtx, Syntax (StxAtom, StxList, StxBool, StxPair, StxVoid))
import Opal.Expand.Syntax qualified as Syntax
import qualified Data.Map.Strict as Map

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pprDecl :: Decl -> Text
pprDecl decl = Emit.layout (docDecl decl)

-- | TODO
--
-- @since 1.0.0
pprDatum :: Datum -> Text
pprDatum dtm = Emit.layout (docDatum dtm)

-- | TODO
--
-- @since 1.0.0
pprExpr :: Expr -> Text
pprExpr sexp = Emit.layout (docExpr sexp)

-- | TODO
--
-- @since 1.0.0
pprSyntax :: Syntax -> Text
pprSyntax stx = Emit.layout (docSyntax stx)

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docExpr :: Expr -> Doc a
docExpr (SExpVal val) = docDatum val
docExpr (SExpVar var) = docSExpVar var
docExpr (SExpApp fun args) = docSExpApp fun args
docExpr (SExpIf c e0 e1) = 
  (Emit.parens . Emit.hsep)
    ["if" <+> docExpr c
    , docExpr e0
    , docExpr e1
    ]
docExpr (SExpLet vars body) = 
  (Emit.parens . Emit.hsep)
    ["let"
    , (Emit.nest 2 . Emit.vsep)
        [ Emit.parens (Emit.vsep (Map.foldMapWithKey (\var val -> [Emit.bracks (emit var <+> docExpr val)]) vars))
        , docExpr body
        ]
    ]

-- | TODO
--
-- @since 1.0.0
docSExpVar :: Name -> Doc a
docSExpVar var = Emit.text (Text.pack (Name.unpack var))

-- | TODO
--
-- @since 1.0.0
docSExpApp :: Expr -> [Expr] -> Doc a
docSExpApp fun args = Emit.parens (Emit.hsep (map docExpr (fun : args)))

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docDecl :: Decl -> Doc a
docDecl (DeclDefn defn sexp) =
  "(define-value"
    <+> emit defn.symbol
      <> Emit.nest 2 (docExpr sexp)
      <> ")"
docDecl (DeclDefnStx defn sexp) =
  "(define-syntax-value"
    <+> emit defn.symbol
      <> Emit.nest 2 (docExpr sexp)
      <> ")"
docDecl (DeclSExp sexp) = docExpr sexp

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docDatum :: Datum -> Doc a
docDatum (Datum.Stx stx) = docSyntax stx
docDatum (Datum.Atom atom) = emit atom
docDatum (Datum.Bool bool) = if bool then "#t" else "#f"
docDatum (Datum.Pair lhs rhs) =
  (Emit.parens . Emit.hsep)
    [ docDatum lhs 
    , "."
    , docDatum rhs
    ]
docDatum (Datum.Prim prim) = emit (Core.Prim.toName prim)
docDatum (Datum.Proc vars body) =
  (Emit.parens . Emit.hsep)
    [ "λ"
    , Emit.parens (Emit.hsep (map emit vars))
    , Emit.nest 2 (Emit.vsep (foldr ((:) . docExpr) [] body))
    ]
docDatum (Datum.List vals) =
  "'" <> Emit.parens (Emit.hsep (map docDatum vals))
docDatum Datum.Void = "#<void>"

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docSyntax :: Syntax -> Doc a
docSyntax (StxVoid ctx) = docStxVoid ctx 
docSyntax (StxBool ctx atom) = docStxBool ctx atom
docSyntax (StxPair ctx stx0 stx1) = docStxPair ctx stx0 stx1
docSyntax (StxAtom ctx atom) = docStxAtom ctx atom
docSyntax (StxList ctx stxs) = docStxList ctx stxs

-- | TODO
--
-- @since 1.0.0
docStxCtx :: StxCtx -> Doc a
docStxCtx ctx = 
  case ctx.location of 
    Nothing -> mempty
    Just loc -> emit loc.line <> ":" <> emit loc.coln

-- | TODO
--
-- @since 1.0.0
docStxPair :: StxCtx -> Syntax -> Syntax -> Doc a
docStxPair ctx stx0 stx1 =
  Emit.hsep 
    [ "#<syntax:" <> docStxCtx ctx <> ":" 
    , docSyntax stx0
    , docSyntax stx1 <> ">"
    ]

-- | TODO
--
-- @since 1.0.0
docStxVoid :: StxCtx -> Doc a
docStxVoid ctx = "#<syntax:" <> docStxCtx ctx <> ": #<void>>"

-- | TODO
--
-- @since 1.0.0
docStxBool :: StxCtx -> Bool -> Doc a
docStxBool ctx bool 
  | bool = "#<syntax:" <> docStxCtx ctx <> ": #t>"
  | otherwise = "#<syntax:" <> docStxCtx ctx <> ": #f>"

-- | TODO
--
-- @since 1.0.0
docStxAtom :: StxCtx -> Symbol -> Doc a
docStxAtom ctx atom = "#<syntax:" <> docStxCtx ctx <> ":" <+> emit atom <> ">"

-- | TODO
--
-- @since 1.0.0
docStxList :: StxCtx -> [Syntax] -> Doc a
docStxList ctx stxs =
  "#<syntax:"
    <> docStxCtx ctx
    <+> Emit.parens (Emit.hsep (map docStxElem stxs))
      <> ">"
  where
    docStxElem :: Syntax -> Doc a
    docStxElem (StxVoid ctx') = "#<syntax:" <> docStxCtx ctx' <> ": #<void>>" 
    docStxElem (StxBool _ bool) = if bool then "#t" else "#f"
    docStxElem (StxPair ctx' stx0 stx1) = docStxPair ctx' stx0 stx1
    docStxElem (StxAtom _ atom) = emit atom
    docStxElem (StxList _ stxs') = Emit.parens (Emit.hsep (map docStxElem stxs'))
