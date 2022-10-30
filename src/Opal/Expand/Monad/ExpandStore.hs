{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Monad.ExpandStore (
  -- * ExpandStore
  ExpandStore (..),

  -- ** Construction
  makeExpandStore,

  -- ** Lenses
  stwBindstore,
  stwEnvironment,
  stwPhase,
  stwNextScope,
  stwNextGenId,
  stwIntroScopes,
  stwUsageScopes,
) where

import Control.Lens (Lens', lens)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Core.Datum qualified as Datum
import Opal.Core.Form (CoreForm)
import Opal.Core.Form qualified as Core.Form
import Opal.Core.Prim (CorePrim)
import Opal.Core.Prim qualified as Core.Prim

import Opal.Expand.Syntax.BindStore (BindStore)
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.ScopeSet (ScopeId, ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Transform (Transform)
import Opal.Expand.Transform qualified as Transform
import Opal.Expand.Syntax.MultiScopeSet (Phase(Phase))

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ExpandStore = ExpandStore
  { state'bindstore :: BindStore
  , state'environment :: Map Name Transform
  -- ^ The compile time environmenot for local bindings.
  , state'phase :: {-# UNPACK #-} !Phase
  -- ^ The current expansion phase.
  , state'next'scope :: {-# UNPACK #-} !ScopeId
  , state'next'genId :: {-# UNPACK #-} !Int
  , state'usage'scopes :: ScopeSet
  , state'intro'scopes :: ScopeSet
  }
  deriving (Eq, Ord, Show)

-- Construction ----------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
makeExpandStore :: ExpandStore
makeExpandStore =
  ExpandStore
    { state'bindstore = BindStore.empty
    , state'environment = makeCoreEnv
    , state'phase = Phase 0 
    , state'next'scope = ScopeSet.ScopeId 1
    , state'next'genId = 0
    , state'usage'scopes = ScopeSet.empty
    , state'intro'scopes = ScopeSet.empty
    }

-- | TODO
--
-- @since 1.0.0
makeCoreEnv :: Map Name Transform
makeCoreEnv = Map.union makeCoreFormEnv makeCorePrimEnv

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
makeCorePrimEnv = foldr insertCorePrim Map.empty [minBound .. maxBound]
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
stwBindstore :: Lens' ExpandStore BindStore
stwBindstore = lens state'bindstore \st x -> st {state'bindstore = x}

-- | TODO
--
-- @since 1.0.0
stwEnvironment :: Lens' ExpandStore (Map Name Transform)
stwEnvironment = lens state'environment \st x -> st {state'environment = x}

-- | TODO
--
-- @since 1.0.0
stwPhase :: Lens' ExpandStore Phase
stwPhase = lens state'phase \st x -> st {state'phase = x}

-- | TODO
--
-- @since 1.0.0
stwNextScope :: Lens' ExpandStore ScopeId
stwNextScope = lens state'next'scope \st x -> st {state'next'scope = x}

-- | TODO
--
-- @since 1.0.0
stwNextGenId :: Lens' ExpandStore Int
stwNextGenId = lens state'next'genId \st x -> st {state'next'genId = x}

-- | TODO
--
-- @since 1.0.0
stwUsageScopes :: Lens' ExpandStore ScopeSet
stwUsageScopes = lens state'usage'scopes \st x -> st {state'usage'scopes = x}

-- | TODO
--
-- @since 1.0.0
stwIntroScopes :: Lens' ExpandStore ScopeSet
stwIntroScopes = lens state'intro'scopes \st x -> st {state'intro'scopes = x}
