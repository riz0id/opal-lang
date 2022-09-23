{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Opal.Expand.Resolve.Class
  ( -- * TODO
    MonadResolve,
    newScopeId,
    newBind,
    resolveBind,
    resolve,
  )
where

import Opal.Common.Symbol (Symbol)
import Opal.Common.GenSym (MonadGenSym)

import Opal.Expand.Syntax.Binding (Binding)
import Opal.Expand.Syntax.ScopeSet (ScopeId)
import Opal.Expand.Syntax.MultiScopeSet (Phase)
import Opal.Expand.Syntax (StxIdt)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
class MonadGenSym m => MonadResolve m where 
  newScopeId :: m ScopeId 
  
  newBind :: Phase -> StxIdt -> m Binding

  resolveBind :: Phase -> StxIdt -> m (Maybe Binding)

  resolve :: Phase -> StxIdt -> m (Maybe Symbol)