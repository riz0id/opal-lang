{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Core (
  parse,
  resolveIdt,
  indexTransformers,

  -- * Phase Operations
  withPhase,
  nextPhase,

  -- * Scope Introduction
  newScopeId,
  newIntroScopeId,
  newUsageScopeId,

  -- * Scoping Operations
  scopeSyntax,
  scopeStxIdt,
  flipsSyntax,
  pruneSyntax,

  -- * Binding Introduction
  introBinding,

  -- ** Core Bindings
  introCoreBind,
  introCoreBinds,

  -- ** Core Syntactic Forms
  introCoreForms,

  -- ** Core Primitives
  introPrimBind,
  introCorePrims,
) where

import Control.Lens (over, set, (^.))

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (gets, modify', state)

import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol
import Opal.Common.GenSym qualified as GenSym
import Opal.Common.GenSym (newGenSymWith)

import Opal.Core.Prim (CorePrim)
import Opal.Core.Prim qualified as Core.Prim
import Opal.Core.Form (CoreForm)
import Opal.Core.Form qualified as Core.Form

import Opal.Expand.Monad (
  Expand,
  ExpandContext (..),
  ExpandError (..),
  state'intro'scopes, ctxPhase, stwBindstore, stwNextScope, stwIntroScopes,
 )
import Opal.Expand.Syntax (StxIdt, Syntax)
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.Binding (Binding (Binding))
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.ScopeSet (ScopeId, ScopeSet)
import Opal.Expand.Syntax.MultiScopeSet (Phase)
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Transform (Transform)
import qualified Opal.Expand.Syntax.ScopeSet as ScopeSet

import Opal.Parse (Parse)
import Opal.Parse qualified as Parse
import Opal.Expand.Resolve (resolveName)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
parse :: Parse a -> Expand a
parse px = do
  ph <- asks ctx'phase
  case Parse.runParse ph px of
    Left exn -> throwError (ExnParseError exn)
    Right sexp -> pure sexp
{-# INLINE parse #-}

-- | TODO
--
-- @since 1.0.0
resolveIdt :: StxIdt -> Expand Transform 
resolveIdt idt = indexTransformers =<< resolveName idt 

-- | TODO
--
-- @since 1.0.0
indexTransformers :: Name -> Expand Transform
indexTransformers name = do
  env <- asks ctx'transformers
  case Map.lookup name env of
    Nothing -> throwError (ExnUnboundTransformer name env)
    Just rx -> pure rx

-- Phase Operations ------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
withPhase :: Phase -> Expand a -> Expand a 
withPhase ph = local (set ctxPhase ph)

-- | TODO
--
-- @since 1.0.0
nextPhase :: Expand a -> Expand a 
nextPhase = local (over ctxPhase succ)

-- Scoping Introduction --------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newScopeId :: Expand ScopeId
newScopeId = state \store -> 
  (store^.stwNextScope , over stwNextScope succ store)

-- | TODO
--
-- @since 1.0.0
newIntroScopeId :: Expand ScopeId
newIntroScopeId = do 
  sc <- newScopeId
  modify' (over stwIntroScopes (ScopeSet.insert sc))
  pure sc

-- | TODO
--
-- @since 1.0.0
newUsageScopeId :: Expand ScopeId
newUsageScopeId = do 
  sc <- newScopeId
  modify' (over stwIntroScopes (ScopeSet.insert sc))
  pure sc

-- Scoping Operations ----------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
scopeSyntax :: ScopeId -> Syntax -> Expand Syntax
scopeSyntax sc stx = asks \ctx -> Syntax.scope (ctx'phase ctx) sc stx

-- | TODO
--
-- @since 1.0.0
scopeStxIdt :: ScopeId -> StxIdt -> Expand StxIdt
scopeStxIdt sc idt = asks \ctx -> Syntax.scopeIdt (ctx'phase ctx) sc idt

-- | TODO
--
-- @since 1.0.0
pruneSyntax :: Syntax -> Expand Syntax
pruneSyntax stx = do
  ph <- asks ctx'phase
  sc <- gets state'intro'scopes
  pure (Syntax.prune ph sc stx)

-- | TODO
--
-- @since 1.0.0
flipsSyntax :: ScopeId -> Syntax -> Expand Syntax
flipsSyntax sc stx = asks \ctx -> Syntax.flips (ctx'phase ctx) sc stx

-- Binding Introduction --------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
introBinding :: StxIdt -> Expand Name 
introBinding idt = do 
  let name = Symbol.toName idt.symbol
  binding <- makeBinding idt
  modify' (over stwBindstore (BindStore.insert name binding))
  pure binding.binder

-- | TODO
--
-- @since 1.0.0
makeBinding :: StxIdt -> Expand Binding
makeBinding idt = do
  ph <- asks ctx'phase
  nm <- makeBinder idt.symbol
  let scopes :: ScopeSet 
      scopes = MultiScopeSet.index ph idt.context.multiscope
   in pure (Binding scopes nm)

-- | TODO
--
-- @since 1.0.0
makeBinder :: Symbol -> Expand Name
makeBinder symbol = fmap GenSym.toName (newGenSymWith symbol)

-- Binding Introduction - Core Bindings  ---------------------------------------

-- | TODO
--
-- @since 1.0.0
introCoreBinds :: Expand ()
introCoreBinds = do 
  -- TODO
  introCoreForms 
  introCorePrims

  -- TODO 
  introCoreBind "#f"
  introCoreBind "#F"
  introCoreBind "#t"
  introCoreBind "#T"

-- | TODO
--
-- @since 1.0.0
introCoreBind :: Name -> Expand ()
introCoreBind name = do
  let binding :: Binding
      binding = Binding.makeCoreBinding name
   in modify' (over stwBindstore (BindStore.insert name binding))

-- Binding Introduction - Core Syntactic Forms ---------------------------------

-- | TODO
--
-- @since 1.0.0
introCoreForms :: Expand ()
introCoreForms =
  let coreForms :: [CoreForm]
      coreForms = [minBound .. maxBound]
   in traverse_ introCoreFormBind coreForms

-- | TODO
--
-- @since 1.0.0
introCoreFormBind :: CoreForm -> Expand ()
introCoreFormBind = introCoreBind . Core.Form.toName

-- Binding Introduction - Core Primitives --------------------------------------

-- | TODO
--
-- @since 1.0.0
introCorePrims :: Expand ()
introCorePrims =
  let corePrims :: [CorePrim]
      corePrims = [minBound .. maxBound]
   in traverse_ introPrimBind corePrims

-- | TODO
--
-- @since 1.0.0
introPrimBind :: CorePrim -> Expand ()
introPrimBind = introCoreBind . Core.Prim.toName