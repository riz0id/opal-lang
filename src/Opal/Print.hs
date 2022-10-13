{-# LANGUAGE OverloadedStrings #-}

module Opal.Print
  ( -- * TODO
    pprDatum,
    pprSExp,
    pprSyntax,

    -- * TODO
    docSExp,
    docSExpVar,
    docSExpApp,

    -- * TODO
    docDatum,
    docClause,

    -- * TODO
    docSyntax,
    docStxCtx,
    docStxAtom,
    docStxList,
  )
where

import Data.SrcLoc (SrcLoc (coln, line))
import Data.Text (Text)
import Data.Text qualified as Text

import Text.Emit (Doc, emit, (<+>))
import Text.Emit qualified as Emit

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Name qualified as Name
import Opal.Common.Symbol (Symbol)

import Opal.Core
  ( Datum (DatumAtom, DatumList, DatumPrim, DatumProc, DatumStx),
    Expr,
    Prim (PrimBoolFalse, PrimBoolTrue),
    SExp (SExpApp, SExpVal, SExpVar),
  )
import Opal.Core.Datum (Clause (Clause), Datum (DatumCase))
import Opal.Core.Prim qualified as Prim

import Opal.Expand.Syntax (StxCtx, Syntax (StxAtom, StxList))
import Opal.Expand.Syntax qualified as Syntax

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pprDatum :: Datum -> Text
pprDatum dtm = Emit.layout (docDatum dtm)

-- | TODO
--
-- @since 1.0.0
pprSExp :: Expr -> Text
pprSExp sexp = Emit.layout (docSExp sexp)

-- | TODO
--
-- @since 1.0.0
pprSyntax :: Syntax -> Text
pprSyntax stx = Emit.layout (docSyntax stx)

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docSExp :: Expr -> Doc a
docSExp (SExpVal val) = docDatum val
docSExp (SExpVar var) = docSExpVar var
docSExp (SExpApp fun args) = docSExpApp fun args

-- | TODO
--
-- @since 1.0.0
docSExpVar :: Name -> Doc a
docSExpVar var = Emit.text (Text.pack (Name.unpack var))

-- | TODO
--
-- @since 1.0.0
docSExpApp :: Expr -> [Expr] -> Doc a
docSExpApp fun args = Emit.parens (Emit.hsep (map docSExp (fun : args)))

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docDatum :: Datum -> Doc a
docDatum (DatumStx stx) = docSyntax stx
docDatum (DatumAtom atom) = emit atom
docDatum (DatumPrim prim) = docPrim prim
docDatum (DatumProc vars body) =
  "'(λ ("
    <> Emit.hsep (map emit vars)
    <> Emit.text ") ->"
    <> Emit.nest 2 (docSExp body)
    <> ")"
docDatum (DatumCase scrut cases) =
  "'(case"
    <+> Emit.parens (docSExp scrut)
    <+> "->"
    <> Emit.nest 2 (Emit.vsep (map docClause cases))
    <> ")"
docDatum (DatumList vals) =
  "'" <> Emit.parens (Emit.hsep (map docDatum vals))

-- | TODO
--
-- @since 1.0.0
docClause :: Clause -> Doc a
docClause (Clause pat body) = "'" <> Emit.bracks (docDatum pat <+> docSExp body)

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docPrim :: Prim -> Doc a
docPrim PrimBoolFalse = "#f"
docPrim PrimBoolTrue = "#t"
docPrim prim = emit (Prim.primToSymbol prim)

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
docSyntax :: Syntax -> Doc a
docSyntax (StxAtom ctx atom) = docStxAtom ctx atom
docSyntax (StxList ctx stxs) = docStxList ctx stxs

-- | TODO
--
-- @since 1.0.0
docStxCtx :: StxCtx -> Doc a
docStxCtx ctx = emit ctx.location.line <> ":" <> emit ctx.location.coln

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
    docStxElem (StxAtom _ atom) = emit atom
    docStxElem (StxList _ stxs') = Emit.parens (Emit.hsep (map docStxElem stxs'))
