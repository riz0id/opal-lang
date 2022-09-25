{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Context
  ( -- * Expander Context
    Context
      ( Context,
        ctxPhase,
        ctxCoreScope,
        ctxUseScopes,
        ctxIntroScopes,
        ctxEnvironment
      ),
    shiftPhase,
    newTransformer,

    -- ** Construction
    newExpanderContext,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

--------------------------------------------------------------------------------

import Opal.AST.Literal (Literal (BoolLit))

import Opal.Common.Symbol (Symbol)

import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId), ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Transformer
  ( Transform (DtmTfm, LamTfm, LetTfm, QteTfm, StxTfm, VarTfm),
  )

import Opal.Expr (Datum (LitDtm))

-- Expander Context ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Context = Context
  { ctxPhase :: {-# UNPACK #-} !Phase
  , ctxCoreScope :: ScopeId
  , ctxUseScopes :: ScopeSet
  , ctxIntroScopes :: ScopeSet
  , ctxEnvironment :: Map Symbol Transform
  }

-- | TODO
--
-- @since 1.0.0
shiftPhase :: Context -> Context
shiftPhase ctx = ctx {ctxPhase = succ ctx.ctxPhase}

-- | TODO
--
-- @since 1.0.0
newTransformer :: Symbol -> Transform -> Context -> Context
newTransformer symbol transformer ctx =
  ctx {ctxEnvironment = Map.insert symbol transformer ctx.ctxEnvironment}

-- Expander Context - Construction ---------------------------------------------

-- | TODO
--
-- @since 1.0.0
newExpanderContext :: Context
newExpanderContext =
  let coreBinds :: Map Symbol Transform
      coreBinds =
        [ ("quote", QteTfm)
        , ("syntax", StxTfm)
        , ("lambda", LamTfm)
        , ("let-syntax", LetTfm)
        , ("#t", DtmTfm $ LitDtm $ BoolLit True)
        , ("#f", DtmTfm $ LitDtm $ BoolLit False)
        , ("syntax-e", VarTfm "syntax-e")
        , ("make-syntax", VarTfm "make-syntax")
        , ("syntax-local-value", VarTfm "syntax-local-value")
        ]
   in Context
        (Phase 0)
        (ScopeId 0)
        ScopeSet.empty
        ScopeSet.empty
        (coreBinds)
{-# INLINE CONLIKE newExpanderContext #-}