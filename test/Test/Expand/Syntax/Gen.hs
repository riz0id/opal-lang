module Test.Expand.Syntax.Gen
  ( -- * Scopes
    binding,
    
    -- * Scopes
    scopeId,
    scopeSet,

    -- * Re-exports
    Gen,
    Range,
  )
where

import Control.Applicative (liftA2)

import GHC.Exts (fromList)

import Hedgehog (Gen, Range)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId), ScopeSet)
import Opal.Expand.Syntax.Binding (Binding (Binding))
import Opal.Expand.Syntax.BindTable (BindTable (BindTable))

import Test.Common.Gen qualified as Gen

-- Binding ---------------------------------------------------------------------

binding :: Gen Binding
binding = liftA2 Binding scopeSet Gen.genSym

bindTable :: Gen BindTable
bindTable = 
  Gen.sized \size -> 
    let range :: Range Int 
        range = Range.constant 0 (fromIntegral size)
     in fmap BindTable (Gen.map range (liftA2 (,) Gen.symbol (Gen.map range (liftA2 (,) scopeSet Gen.genSym))))

-- Scopes ----------------------------------------------------------------------

scopeId :: Gen ScopeId
scopeId = fmap ScopeId (Gen.int Range.constantBounded)

scopeSet :: Gen ScopeSet
scopeSet =
  Gen.sized \size -> 
    let range :: Range Int 
        range = Range.constant 0 (fromIntegral size)
     in fmap fromList (Gen.list range scopeId)