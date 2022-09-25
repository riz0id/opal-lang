{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Syntax
  ( -- * Scoping Operations
    scope,
    flipscope,
    prune,

    -- * StxCtx
    StxCtx (StxCtx, stxctx'multiscopes, stxctx'srcloc),

    -- * StxIdt
    StxIdt (StxIdt, stxidt'stxctx, stxidt'symbol),

    -- * Syntax
    Syntax (Lit, Idt, App),

    -- * Re-exports
    ScopeId,
    ScopeSet,
  )
where

import Data.Data (Data, gmapT)
import Data.SrcLoc (SrcLoc, line, coln)

import GHC.Records (HasField, getField)

import Prettyprinter (Pretty, pretty, prettyList, viaShow, (<+>))
import Prettyprinter qualified as Print

import Type.Reflection
  ( TypeRep,
    eqTypeRep,
    typeOf,
    typeRep,
    (:~~:) (HRefl),
  )

--------------------------------------------------------------------------------

import Opal.AST.Literal (Literal)

import Opal.Common.Symbol (Symbol)

import Opal.Expand.Syntax.MultiScopeSet (MultiScopeSet, Phase)
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Syntax.ScopeSet (ScopeId, ScopeSet)

-- Scoping Operations ----------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
scope :: Data a => Phase -> ScopeId -> a -> a
scope ph sc =
  let scopeSetTypeRep :: TypeRep MultiScopeSet
      scopeSetTypeRep = typeRep
   in gmapT \subterm ->
        case eqTypeRep (typeOf subterm) scopeSetTypeRep of
          Nothing -> scope ph sc subterm
          Just HRefl -> MultiScopeSet.insert ph sc subterm

-- | TODO
--
-- @since 1.0.0
flipscope :: Data a => Phase -> ScopeId -> a -> a
flipscope ph sc =
  let scopeSetTypeRep :: TypeRep MultiScopeSet
      scopeSetTypeRep = typeRep
   in gmapT \subterm ->
        case eqTypeRep (typeOf subterm) scopeSetTypeRep of
          Nothing -> flipscope ph sc subterm
          Just HRefl -> MultiScopeSet.flipscope ph sc subterm

-- | TODO
--
-- @since 1.0.0
prune :: Data a => Phase -> ScopeSet -> a -> a
prune ph set =
  let scopeSetTypeRep :: TypeRep MultiScopeSet
      scopeSetTypeRep = typeRep
   in gmapT \subterm ->
        case eqTypeRep (typeOf subterm) scopeSetTypeRep of
          Nothing -> prune ph set subterm
          Just HRefl -> MultiScopeSet.prune ph set subterm

-- StxCtx ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data StxCtx = StxCtx
  { stxctx'srcloc :: {-# UNPACK #-} !SrcLoc
  , stxctx'srclen :: {-# UNPACK #-} !Int
  , stxctx'multiscopes :: MultiScopeSet
  }
  deriving (Data, Eq, Ord, Show)

-- | @since 1.0.0
instance HasField "multiscope" StxCtx MultiScopeSet where
  getField (StxCtx _ _ multiscope) = multiscope
  {-# INLINE getField #-}

-- | @since 1.0.0
instance HasField "location" StxCtx SrcLoc where
  getField (StxCtx loc _ _) = loc
  {-# INLINE getField #-}

-- | @since 1.0.0
instance Pretty StxCtx where
  pretty (StxCtx loc _ _) = viaShow loc.line <> Print.colon <> viaShow loc.coln
  {-# INLINE pretty #-}

  prettyList xs = Print.group $ Print.parens (Print.sep $ map pretty xs)
  {-# INLINE prettyList #-}

-- StxIdt ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data StxIdt = StxIdt
  { stxidt'stxctx :: {-# UNPACK #-} !StxCtx
  , stxidt'symbol :: {-# UNPACK #-} !Symbol
  }
  deriving (Data, Eq, Ord, Show)

-- | @since 1.0.0
instance HasField "context" StxIdt StxCtx where
  getField (StxIdt ctx _) = ctx
  {-# INLINE getField #-}

-- | @since 1.0.0
instance HasField "multiscope" StxIdt MultiScopeSet where
  getField (StxIdt ctx _) = ctx.multiscope
  {-# INLINE getField #-}

-- | @since 1.0.0
instance HasField "location" StxIdt SrcLoc where
  getField (StxIdt ctx _) = ctx.location
  {-# INLINE getField #-}

-- | @since 1.0.0
instance HasField "symbol" StxIdt Symbol where
  getField (StxIdt _ symbol) = symbol
  {-# INLINE getField #-}

-- | @since 1.0.0
instance HasField "syntax" StxIdt Syntax where
  getField = Idt
  {-# INLINE getField #-}

-- | @since 1.0.0
instance Pretty StxIdt where
  pretty (StxIdt ctx idt) =
    Print.flatAlt
      ("#<syntax:" <> pretty ctx <+> viaShow idt <> ">")
      ("#'" <> viaShow idt)
  {-# INLINE pretty #-}

  prettyList xs = Print.group $ Print.parens (Print.sep $ map pretty xs)
  {-# INLINE prettyList #-}

-- Syntax ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Syntax
  = Lit {-# UNPACK #-} !StxCtx Literal
  | Idt {-# UNPACK #-} !StxIdt
  | App {-# UNPACK #-} !StxCtx [Syntax]
  deriving (Data, Eq, Ord, Show)

-- | @since 1.0.0
instance HasField "context" Syntax StxCtx where
  getField (Lit ctx _) = ctx
  getField (Idt idt) = idt.context
  getField (App ctx _) = ctx
  {-# INLINE getField #-}

-- | @since 1.0.0
instance HasField "multiscope" Syntax MultiScopeSet where
  getField (Lit ctx _) = ctx.multiscope
  getField (Idt idt) = idt.multiscope
  getField (App ctx _) = ctx.multiscope
  {-# INLINE getField #-}

-- | @since 1.0.0
instance HasField "location" Syntax SrcLoc where
  getField (Lit ctx _) = ctx.location
  getField (Idt idt) = idt.location
  getField (App ctx _) = ctx.location
  {-# INLINE getField #-}

-- | @since 1.0.0
instance Pretty Syntax where
  pretty (Lit _ lit) = Print.group ("#'" <> pretty lit)
  pretty (Idt idt) = Print.group (pretty idt)
  pretty (App ctx []) =
    Print.flatAlt ("#<syntax:" <> pretty ctx <+> "()>") "#syntax<()>"
  pretty (App ctx (x : xs)) = case x of
    Idt (StxIdt _ idt)
      | idt == "lambda"
      , [Idt var, body] <- xs ->
          (Print.align . Print.fillSep)
            [ "#'(λ", viaShow var.symbol, pretty body <> ")"]
      | idt == "let-syntax"
      , [App _ [Idt var, val], body] <- xs ->
          (Print.align . Print.vsep)
            [ "#'(let-syntax"
            , Print.indent 6 (Print.brackets (viaShow var.symbol <+> Print.align (pretty val)))
            , Print.indent 4 (pretty body <> ")")
            ]
    _ ->
      "#<syntax:"
        <> pretty ctx
        <+> Print.group ("(" <> Print.align (Print.fillSep (map pretty (x : xs))) <> ")>")
  {-# INLINE pretty #-}