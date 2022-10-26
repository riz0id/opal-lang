{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Monad.ExpandContext (
  -- * ExpansionMode
  ExpansionMode (..),

  -- * ExpandContext
  ExpandContext (..),

  -- ** Construction
  makeExpandContext,

  -- ** Lenses
  ctxPhase,
  ctxScopes,
  ctxMode,
  ctxTransformers,

  -- ** Insertion
  extend,
  extends,
) where

import Control.Lens (Lens', lens, over)

import Data.Data (Data)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Core.Datum qualified as Datum
import Opal.Core.Form (CoreForm)
import Opal.Core.Form qualified as Core.Form
import Opal.Core.Prim (CorePrim)
import Opal.Core.Prim qualified as Core.Prim

import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))

import Opal.Expand.Namespace (Namespace)
import Opal.Expand.Syntax.ScopeSet (ScopeSet)
import Opal.Expand.Transform (Transform)
import Opal.Expand.Transform qualified as Transform
import qualified Opal.Expand.Syntax.ScopeSet as ScopeSet

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ExpansionMode
  = ExpandTopLevel
  | ExpandModule
  | ExpandExpression
  deriving (Bounded, Data, Enum, Eq, Ord, Show)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ExpandContext = ExpandContext
  { ctx'phase :: {-# UNPACK #-} !Phase
  -- ^ The current expansion phase.
  , ctx'scopes :: ScopeSet
  -- ^ A set of scopes to be pruned when expanding a quote-syntax form.
  , ctx'expanding :: ExpansionMode
  -- ^ Are we expanding at the top-level, the module-level, or expressions? This
  -- enum indicates the kind of expansion taking place.
  , ctx'namespace :: {-# UNPACK #-} !Namespace
  -- ^ The expander's namespace for module and top-levels.
  , ctx'transformers :: Map Name Transform
  -- ^ The compile time environment for local bindings.
  , ctx'only'immediate :: Bool
  }
  deriving (Data, Eq, Ord, Show)

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
makeExpandContext :: Namespace -> ExpandContext
makeExpandContext ns =
  ExpandContext
    (Phase 0)
    ScopeSet.empty
    ExpandTopLevel
    ns
    makeCoreEnv
    True

-- | TODO
--
-- @since 1.0.0
makeCoreEnv :: Map Name Transform
makeCoreEnv =
  Map.union makeCoreFormEnv makeCorePrimEnv
    & Map.insert "#t" (Transform.Dtm (Datum.Bool True))
    & Map.insert "#T" (Transform.Dtm (Datum.Bool True))
    & Map.insert "#f" (Transform.Dtm (Datum.Bool False))
    & Map.insert "#F" (Transform.Dtm (Datum.Bool False))

-- | TODO
--
-- @since 1.0.0
makeCoreFormEnv :: Map Name Transform
makeCoreFormEnv =
  foldr insertCorePrim Map.empty [minBound .. maxBound]
  where
    insertCorePrim :: CoreForm -> Map Name Transform -> Map Name Transform
    insertCorePrim core rest =
      let name = Core.Form.toName core
          form = Transform.Core core
       in Map.insert name form rest

-- | TODO
--
-- @since 1.0.0
makeCorePrimEnv :: Map Name Transform
makeCorePrimEnv =
  foldr insertCorePrim Map.empty [minBound .. maxBound]
  where
    insertCorePrim :: CorePrim -> Map Name Transform -> Map Name Transform
    insertCorePrim prim rest =
      let name = Core.Prim.toName prim
          form = Transform.Dtm (Datum.Prim prim)
       in Map.insert name form rest

-- Lenses ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
ctxPhase :: Lens' ExpandContext Phase
ctxPhase = lens ctx'phase \ctx ph -> ctx {ctx'phase = ph}

-- | TODO
--
-- @since 1.0.0
ctxScopes :: Lens' ExpandContext ScopeSet 
ctxScopes = lens ctx'scopes \ctx scps -> ctx {ctx'scopes = scps}

-- | TODO
--
-- @since 1.0.0
ctxMode :: Lens' ExpandContext ExpansionMode
ctxMode = lens ctx'expanding \ctx mode -> ctx {ctx'expanding = mode}

-- | TODO
--
-- @since 1.0.0
ctxTransformers :: Lens' ExpandContext (Map Name Transform)
ctxTransformers = lens ctx'transformers \ctx xs -> ctx {ctx'transformers = xs}

-- Insertion -------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
extend :: Name -> Transform -> ExpandContext -> ExpandContext
extend name transform = over ctxTransformers (Map.insert name transform)

-- | TODO
--
-- @since 1.0.0
extends :: [(Name, Transform)] -> ExpandContext -> ExpandContext
extends exts ctx = foldr (uncurry extend) ctx exts