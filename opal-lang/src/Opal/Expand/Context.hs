{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Opal.Expand.Context
  ( -- * Expander Context
    Context
      ( Context,
        ctxPhase,
        ctxBindings,
        ctxUseScopes,
        ctxIntroScopes,
        ctxEnvironment
      ),
    shiftPhase,
    newTransformer,

    -- ** Construction
    empty,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

import Opal.Expand.Syntax.BindTable (BindTable)
import Opal.Expand.Syntax.BindTable qualified as BindTable
import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))
import Opal.Expand.Syntax.ScopeSet (ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Transformer (Transform)

-- Expander Context ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Context = Context
  { ctxPhase :: {-# UNPACK #-} !Phase
  , ctxBindings :: BindTable
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
empty :: Context
empty =
  Context
    (Phase 0)
    BindTable.empty
    ScopeSet.empty
    ScopeSet.empty
    Map.empty
{-# INLINE CONLIKE empty #-}