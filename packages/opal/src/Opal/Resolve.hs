{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Resolve
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Opal.Resolve
  ( resolve
  )
where

import Control.Lens ((^.))

import Opal.Common.BindingStore
  ( BindingStore,
    largestSubset,
    lookupBindingStore
  )
import Opal.Common.Phase (Phase)
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.Symbol (Symbol)
import Opal.Syntax (Identifier, idtScopes, idtSymbol)
import Opal.Syntax.ScopeInfo qualified as ScopeInfo

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
resolve :: Phase -> Identifier -> BindingStore -> Maybe Symbol
resolve ph idt store = do
  let scps :: ScopeSet
      scps = ScopeInfo.lookup (Just ph) (idt ^. idtScopes)
   in case lookupBindingStore (idt ^. idtSymbol) store of
        Nothing       -> Nothing
        Just bindings -> do
          result <- largestSubset scps bindings
          pure (snd result)
