{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Monad.ExpandStore (
  -- * ExpandStore
  ExpandStore (..),

  -- ** Construction
  makeExpandStore,

  -- ** Lenses
  stwBindstore,
  stwNextScope,
  stwNextGenId,
  stwIntroScopes,
  stwUsageScopes,
) where

import Control.Lens (Lens', lens)

--------------------------------------------------------------------------------

import Opal.Expand.Syntax.BindStore (BindStore)
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.ScopeSet (ScopeId, ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ExpandStore = ExpandStore
  { state'bindstore :: BindStore
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
    , state'next'scope = ScopeSet.ScopeId 1
    , state'next'genId = 0
    , state'usage'scopes = ScopeSet.empty
    , state'intro'scopes = ScopeSet.empty
    }

-- Lenses ----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
stwBindstore :: Lens' ExpandStore BindStore
stwBindstore = lens state'bindstore \st x -> st {state'bindstore = x}

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
