
module Opal.Expand.Namespace 
  ( -- * Modules
    Module (..),
    makeModule,

    -- * Definitions 
    Definitions(..), 
    makeEmptyDefinitions,
  
    -- * Namespaces
    Namespace (..),
    makeEmptyNamespace,
    declareModule,
    namespaceToDefinitions,
    namespaceToModule,
    namespaceGetVariable,
    namespaceGetTransformer,
  )
where 

import Control.Exception (assert)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Data (Data)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)
import Opal.Common.Name (Name)

import Opal.Core.Datum (Datum)

import Opal.Expand.Syntax.Binding (Binding)
import Opal.Expand.Syntax.MultiScopeSet (MultiScopeSet, Phase)
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet

--------------------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
data Module = Module
  { self'name :: {-# UNPACK #-} !Symbol 
  , requires :: Map Phase (Set Symbol)
  , provides :: Map Phase (Map Symbol Binding)
  , min'phase :: {-# UNPACK #-} !Phase
  , max'phase :: {-# UNPACK #-} !Phase
  }
  deriving (Data, Eq, Ord, Show)

-- | TODO 
--
-- @since 1.0.0
makeModule :: Symbol -> Phase -> Phase -> Module 
makeModule name ph0 ph1 = assert (ph0 < ph1) (Module name Map.empty Map.empty ph0 ph1)

--------------------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
data Definitions = Definitions 
  { variables :: Map Name Datum
  , transformers :: Map Name Datum 
  , instantiated :: Bool
  }
  deriving (Data, Eq, Ord, Show)

-- | TODO 
--
-- @since 1.0.0
makeEmptyDefinitions :: Definitions
makeEmptyDefinitions = Definitions Map.empty Map.empty False

--------------------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
data Namespace = Namespace 
  { multiscope :: MultiScopeSet 
  , definitions :: Map Phase Definitions
  , module'declarations :: Map Name Module
  , submodule'declarations :: Map Name Module
  , module'instances :: Map Name (Map Phase Namespace)
  }
  deriving (Data, Eq, Ord, Show)

-- | TODO 
--
-- @since 1.0.0
makeEmptyNamespace :: Namespace 
makeEmptyNamespace = Namespace MultiScopeSet.empty Map.empty Map.empty Map.empty Map.empty

-- | TODO 
--
-- @since 1.0.0
declareModule :: Namespace -> Name -> Module -> Bool -> Namespace 
declareModule ns name m submodule 
  | submodule = ns{submodule'declarations = Map.insert name m ns.submodule'declarations}
  | otherwise = ns{module'declarations = Map.insert name m ns.module'declarations}

-- | TODO 
--
-- @since 1.0.0
namespaceToDefinitions :: Namespace -> Phase -> Definitions 
namespaceToDefinitions ns ph = 
  case Map.lookup ph (definitions ns) of 
    Nothing -> makeEmptyDefinitions
    Just defns -> defns

-- | TODO 
--
-- @since 1.0.0
namespaceToModule :: Namespace -> Name -> Maybe Module 
namespaceToModule ns name = 
  case Map.lookup name (module'declarations ns) of 
    Nothing -> Map.lookup name (submodule'declarations ns)
    Just rx -> Just rx

-- | TODO 
--
-- @since 1.0.0
namespaceGetVariable :: Namespace -> Phase -> Name -> Maybe Datum
namespaceGetVariable ns ph name = 
  let defns :: Definitions 
      defns = namespaceToDefinitions ns ph
   in Map.lookup name (variables defns) 

-- | TODO 
--
-- @since 1.0.0
namespaceGetTransformer :: Namespace -> Phase -> Name -> Maybe Datum
namespaceGetTransformer ns ph name = 
  let defns :: Definitions 
      defns = namespaceToDefinitions ns ph
   in Map.lookup name (transformers defns) 
