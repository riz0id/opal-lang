{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}

module Opal.Parse
  ( -- * TODO
    Parse,
    runParse,

    -- ** TODO
    ParseError (ExnMissingProc),
  )
where

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask, asks, local)

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

import Opal.Core
  ( Datum (DatumPrim, DatumProc, DatumStx),
    Expr,
    SExp (SExpApp, SExpVal, SExpVar),
    syntaxToDatum,
  )
import Opal.Core.Prim (Prim (PrimLambda, PrimQuote, PrimSyntax))

import Opal.Expand.Syntax (StxCtx (multiscope), Syntax (StxAtom, StxList))
import Opal.Expand.Syntax.BindStore (BindStore)
import Opal.Expand.Syntax.BindStore qualified as BindStore
import Opal.Expand.Syntax.Binding
  ( Binder (BindName, BindPrim),
    Binding (Binding, binder),
  )
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.MultiScopeSet (Phase)
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Syntax.ScopeSet (ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runParse :: Phase -> BindStore -> Syntax -> Either ParseError Expr
runParse ph store stx =
  case unP (pSyntax ph stx) store of
    (# exn | #) -> Left exn
    (# | exp #) -> Right exp
{-# INLINE runParse #-}

-- | TODO
--
-- @since 1.0.0
newtype Parse a = Parse
  {unP :: BindStore -> (# ParseError| a #)}

-- | @since 1.0.0
instance Functor Parse where
  fmap f (Parse k) =
    Parse \s -> case k s of
      (# e | #) -> (# e | #)
      (# | x #) -> (# | f x #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative Parse where
  pure x = Parse \_ -> (# | x #)
  {-# INLINE pure #-}

  Parse f <*> Parse g =
    Parse \s -> case f s of
      (# e | #) -> (# e | #)
      (# | k #) -> case g s of
        (# e | #) -> (# e | #)
        (# | x #) -> (# | k x #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad Parse where
  Parse k >>= f =
    Parse \s -> case k s of
      (# e | #) -> (# e | #)
      (# | x #) -> unP (f x) s
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadError ParseError Parse where
  throwError e = Parse \_ -> (# e | #)
  {-# INLINE throwError #-}

  catchError (Parse k) f =
    Parse \s -> case k s of
      (# e | #) -> unP (f e) s
      (# | x #) -> (# | x #)
  {-# INLINE catchError #-}

-- | @since 1.0.0
instance MonadReader BindStore Parse where
  ask = Parse \s -> (# | s #)
  {-# INLINE ask #-}

  local f (Parse k) = Parse \s -> k (f s)
  {-# INLINE local #-}

-- TODO ------------------------------------------------------------------------

data ParseError
  = -- | TODO
    ExnMissingProc {-# UNPACK #-} !Syntax
  | -- | TODO
    ExnParseLambda [Syntax]
  | -- | TODO
    ExnParseSyntax [Syntax]
  | -- | TODO
    ExnParseQuote [Syntax]
  deriving (Eq, Ord, Show)

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
resolve :: Phase -> StxCtx -> Symbol -> Parse Binder
resolve ph ctx symbol = do
  let name = Symbol.toName symbol
  result <- asks (BindStore.indexBindings name)
  case result of
    Nothing -> pure (BindName name)
    Just binds -> do
      let canidates = Map.foldrWithKey filterCanidates Set.empty binds
      let bindings = Binding.maximum canidates
      case Set.maxView bindings of
        Nothing -> pure (BindName $ Symbol.toName symbol)
        Just (bind, _) -> pure bind.binder
  where
    refScopes :: ScopeSet
    refScopes = MultiScopeSet.index ph ctx.multiscope

    filterCanidates :: ScopeSet -> Binder -> Set Binding -> Set Binding
    filterCanidates set bind canidates
      | ScopeSet.subset set refScopes = Set.insert (Binding set bind) canidates
      | otherwise = canidates

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pSyntax :: Phase -> Syntax -> Parse Expr
pSyntax ph (StxAtom ctx atom) = pStxAtom ph ctx atom
pSyntax ph (StxList ctx stxs)
  | null stxs = throwError (ExnMissingProc (StxList ctx stxs))
  | otherwise = pStxList ph (head stxs) (tail stxs)

-- | TODO
--
-- @since 1.0.0
pStxAtom :: Phase -> StxCtx -> Symbol -> Parse Expr
pStxAtom ph ctx atom = do
  bind <- resolve ph ctx atom
  case bind of
    BindPrim prim -> pure (SExpVal $ DatumPrim prim)
    BindName name -> pure (SExpVar name)

-- | TODO
--
-- @since 1.0.0
pStxList :: Phase -> Syntax -> [Syntax] -> Parse Expr
pStxList ph (StxAtom ctx atom) stxs = do
  bind <- resolve ph ctx atom
  case bind of
    BindPrim PrimLambda -> pStxLambda ph stxs
    BindPrim PrimSyntax -> pStxSyntax ph stxs
    BindPrim PrimQuote -> pStxQuote ph stxs
    BindPrim prim -> do
      let func = SExpVal (DatumPrim prim)
      args <- traverse (pSyntax ph) stxs
      pure (SExpApp func args)
    BindName name -> do
      let func = SExpVar name
      args <- traverse (pSyntax ph) stxs
      pure (SExpApp func args)
pStxList ph (StxList ctx stx) stxs = do
  func <- pSyntax ph (StxList ctx stx)
  args <- traverse (pSyntax ph) stxs
  pure (SExpApp func args)

-- | TODO
--
-- @since 1.0.0
pStxLambda :: Phase -> [Syntax] -> Parse Expr
pStxLambda ph stxs
  | length stxs /= 2 = do
      let exn = ExnParseLambda stxs
      throwError exn
  | otherwise = do
      vars <- case stxs List.!! 0 of
        StxAtom _ atom -> pure [Symbol.toName atom]
        StxList _ vars ->
          for vars \case
            StxAtom _ var -> pure (Symbol.toName var)
            StxList {} -> throwError (ExnParseLambda stxs)

      body <- pSyntax ph (stxs List.!! 1)

      pure (SExpVal $ DatumProc vars body)

-- | TODO
--
-- @since 1.0.0
pStxSyntax :: Phase -> [Syntax] -> Parse Expr
pStxSyntax _ stxs
  | length stxs /= 1 = throwError (ExnParseSyntax stxs)
  | otherwise = pure (SExpVal $ DatumStx $ stxs List.!! 0)

-- | TODO
--
-- @since 1.0.0
pStxQuote :: Phase -> [Syntax] -> Parse Expr
pStxQuote _ stxs
  | length stxs /= 1 = throwError (ExnParseQuote stxs)
  | otherwise = pure (SExpVal $ syntaxToDatum $ stxs List.!! 0)