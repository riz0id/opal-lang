{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Opal.Parse (
  -- * TODO
  Parse,
  evalParseProgram,
  evalParseDecl,
  evalParseExpr,
  runParseExpr,
  runParse,

  -- ** TODO
  ParseError (..),

  -- * TODO
  pProgram,
  pSyntax,
  pVariable,
  pStxIdt,
  pSyntaxModule,
  pLambdaStx,
  pLetStx,
  pStxLetSyntax,
  pStxLetSyntaxBind,
  pStxLetSyntaxBinds,
  pStxDefineValue,
) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (for)

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

import Opal.Core (Decl (..), Expr, SExp (..))
import Opal.Core.Datum qualified as Datum
import Opal.Core.Prim qualified as Core.Prim

import Opal.Expand.Resolve qualified as Resolve
import Opal.Expand.Syntax (StxCtx, StxIdt (StxIdt), Syntax (..))
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId))

import Opal.Core.Datum (Datum)
import Opal.Core.Form qualified as Core.Form
import Opal.Expand.Syntax.StxCtx qualified as StxCtx
import Opal.Parse.Monad (Parse (..), ParseContext (..), makeParseContext)
import Opal.Parse.ParseError (CoreParseError (CoreParseError), ParseError (..))

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalParseProgram :: Syntax -> Either ParseError [Decl]
evalParseProgram stx =
  runParse (Phase 0) do
    pProgram (Syntax.scope (Phase 0) (ScopeId 0) stx)

-- | TODO
--
-- @since 1.0.0
evalParseDecl :: Syntax -> Either ParseError Decl
evalParseDecl stx =
  runParse (Phase 0) do
    pDeclaration (Syntax.scope (Phase 0) (ScopeId 0) stx)

-- | TODO
--
-- @since 1.0.0
evalParseExpr :: Syntax -> Either ParseError Expr
evalParseExpr stx =
  runParse (Phase 0) do
    pSyntax (Syntax.scope (Phase 0) (ScopeId 0) stx)

-- | TODO
--
-- @since 1.0.0
runParseExpr :: Phase -> Syntax -> Either ParseError Expr
runParseExpr ph stx = runParse ph (pSyntax stx)
{-# INLINE runParseExpr #-}

-- | TODO
--
-- @since 1.0.0
runParse :: Phase -> Parse a -> Either ParseError a
runParse phase parser =
  let context :: ParseContext
      context = makeParseContext phase
   in case unP parser context of
        (# exn | #) -> Left exn
        (# | exp #) -> Right exp
{-# INLINE runParse #-}

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
resolve :: StxCtx -> Symbol -> Parse Name
resolve ctx symbol = do
  phase <- asks ctx'phase
  store <- asks ctx'bindstore
  -- TODO: Document why errors are ignore for parsing
  let idt :: StxIdt
      idt = StxIdt ctx symbol
   in case Resolve.runResolveId phase idt store of
        Left {} -> pure (Symbol.toName symbol)
        Right binding -> pure binding.binder

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pProgram :: Syntax -> Parse [Decl]
pProgram (StxList _ stxs) = traverse pDeclaration stxs
pProgram stx = fmap pure (pDeclaration stx)

-- | TODO
--
-- @since 1.0.0
pSyntaxModule :: Syntax -> Parse (StxIdt, [Syntax])
pSyntaxModule (StxList _ (stx : stxs)) = do
  moduleIdt <- pStxIdt stx
  pure (moduleIdt, stxs)
pSyntaxModule stx = do
  throwError (ExnParseModule stx)

-- | TODO
--
-- @since 1.0.0
pDeclaration :: Syntax -> Parse Decl
pDeclaration stx@(StxList _ [StxAtom ctx atom, stx1, stx2]) = do
  name <- resolve ctx atom
  case name of
    "define-value" -> do
      defn <- pStxIdt stx1
      sexp <- pSyntax stx2
      pure (DeclDefn defn sexp)
    "define-syntax-value" -> do
      defn <- pStxIdt stx1
      sexp <- pSyntax stx2
      pure (DeclDefnStx defn sexp)
    _ -> do
      fmap DeclSExp (pSyntax stx)
pDeclaration stx = do
  fmap DeclSExp (pSyntax stx)

-- | TODO
--
-- @since 1.0.0
pSyntax :: Syntax -> Parse Expr
pSyntax StxVoid {} =
  pure (SExpVal Datum.Void)
pSyntax (StxBool _ bool) =
  pure (SExpVal (Datum.Bool bool))
pSyntax (StxPair _ stx0 stx1) = do
  lhs <- pSyntax stx0
  rhs <- pSyntax stx1
  pure (SExpApp (SExpVal (Datum.Prim Core.Prim.Cons)) [lhs, rhs])
pSyntax (StxAtom ctx atom) =
  pStxAtom ctx atom
pSyntax (StxList ctx stxs) = case stxs of
  [] -> throwError (ExnMissingProc (StxList ctx stxs))
  stx : stxs' -> pStxList stx stxs'

-- | TODO
--
-- @since 1.0.0
pVariable :: Syntax -> Parse Name
pVariable stx = do
  idt <- pStxIdt stx
  pure (Symbol.toName idt.symbol)

-- | TODO
--
-- @since 1.0.0
pStxIdt :: Syntax -> Parse StxIdt
pStxIdt (StxAtom ctx atom) = pure (StxIdt ctx atom)
pStxIdt stx = throwError (ExnParseStxIdt stx)

-- | TODO
--
-- @since 1.0.0
pStxAtom :: StxCtx -> Symbol -> Parse Expr
pStxAtom ctx atom = fmap SExpVar (resolve ctx atom)

-- | TODO
--
-- @since 1.0.0
pStxList :: Syntax -> [Syntax] -> Parse Expr
pStxList stx@(StxAtom ctx atom) stxs = do
  resolve ctx atom >>= \case
    "quote" -> do
      pStxQuote stxs
    "quote-syntax" -> do
      pStxSyntax stxs
    "quasisyntax" -> do
      pStxSyntax stxs
    "if" -> do
      case stxs of
        [stx'c, stx'e0, stx'e1] -> do
          expr'c <- pSyntax stx'c
          expr'e0 <- pSyntax stx'e0
          expr'e1 <- pSyntax stx'e1
          pure (SExpIf expr'c expr'e0 expr'e1)
        _ -> do
          throwError (ExnParseIf (StxAtom ctx atom) stxs)
    "let" -> do
      let stx = StxList ctx (StxAtom ctx atom : stxs)
      (vars, body) <- pLetExpr stx
      pure (SExpLet vars body)
    "lambda" -> do
      (args, body) <- pLambdaExpr (StxList StxCtx.empty (stx : stxs))
      pure (SExpVal $ Datum.Proc args body)
    name -> do
      let func = SExpVar name
      args <- traverse pSyntax stxs
      pure (SExpApp func args)
pStxList stx stxs = do
  func <- pSyntax stx
  args <- traverse pSyntax stxs
  pure (SExpApp func args)

-- | TODO
--
-- @since 1.0.0
pStxLetSyntax :: [Syntax] -> Parse ([(StxIdt, Syntax)], Syntax)
pStxLetSyntax [stx, stx'] = do
  bindings <- pStxLetSyntaxBinds stx
  pure (bindings, stx')
pStxLetSyntax stxs =
  throwError (ExnParseLetSyntax stxs)

-- | TODO
--
-- @since 1.0.0
pStxLetSyntaxBind :: Syntax -> Parse (StxIdt, Syntax)
pStxLetSyntaxBind (StxList _ [stx, stx']) = do
  idt <- pStxIdt stx
  pure (idt, stx')
pStxLetSyntaxBind stx = do
  throwError (ExnParseLetSyntaxBind stx)

-- | TODO
--
-- @since 1.0.0
pStxLetSyntaxBinds :: Syntax -> Parse [(StxIdt, Syntax)]
pStxLetSyntaxBinds (StxList _ stxs) = traverse pStxLetSyntaxBind stxs
pStxLetSyntaxBinds stx = throwError (ExnParseLetSyntaxBind stx)

-- | TODO
--
-- @since 1.0.0
pStxDefineValue :: [Syntax] -> Parse (StxIdt, Syntax)
pStxDefineValue [StxAtom ctx atom, stx] = pure (StxIdt ctx atom, stx)
pStxDefineValue stxs = throwError (ExnParseDefineValue stxs)

-- | TODO
--
-- @since 1.0.0
pStxSyntax :: [Syntax] -> Parse Expr
pStxSyntax [stx] = pure (SExpVal $ Datum.Stx stx)
pStxSyntax stxs = throwError (ExnParseSyntax stxs)

-- | TODO
--
-- @since 1.0.0
pStxQuote :: [Syntax] -> Parse Expr
pStxQuote stxs
  | length stxs /= 1 = throwError (ExnParseQuote stxs)
  | otherwise = pure (SExpVal $ Datum.syntaxToDatum $ head stxs)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pLambdaExpr :: Syntax -> Parse ([Name], NonEmpty Expr)
pLambdaExpr stx = do
  (stx'args, stx'body) <- pLambdaStx stx
  let args = map (Symbol.toName . Syntax.symbol) stx'args
  body <- traverse pSyntax stx'body
  pure (args, body)

-- | TODO
--
-- @since 1.0.0
pLambdaStx :: Syntax -> Parse ([StxIdt], NonEmpty Syntax)
pLambdaStx stx =
  case Datum.syntaxExpr stx of
    Datum.List (Datum.Stx (StxAtom ctx atom) : Datum.Stx stx'args : rest) -> do
      name <- resolve ctx atom

      unless (name == "lambda") do
        raiseLambdaParseError stx Nothing

      case NonEmpty.nonEmpty rest of
        Nothing -> do
          let datum = Datum.List rest
          raiseLambdaParseError stx (Just datum)
        Just datums -> do
          vars <- pLambdaFormalStx stx stx'args
          body <- for datums \case
            Datum.Stx stx'body -> pure stx'body
            other -> raiseLambdaParseError stx (Just other)
          pure (vars, body)
    _ ->
      raiseLambdaParseError stx Nothing

-- | TODO
--
-- @since 1.0.0
pLambdaFormalStx :: Syntax -> Syntax -> Parse [StxIdt]
pLambdaFormalStx full stx =
  case Datum.syntaxExpr stx of
    Datum.List datums ->
      for datums \case
        Datum.Stx (StxAtom ctx atom) -> pure (StxIdt ctx atom)
        other -> raiseLambdaParseError full (Just other)
    other ->
      raiseLambdaParseError full (Just other)

raiseLambdaParseError :: Syntax -> Maybe Datum -> Parse a
raiseLambdaParseError stx origin =
  let exn :: CoreParseError
      exn = CoreParseError Core.Form.Lambda stx origin
   in throwError (ExnParseCore exn)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pLetExpr :: Syntax -> Parse (Map Name Expr, NonEmpty Expr)
pLetExpr stx = do
  (stx'vars, stx'body) <- pLetStx stx
  vars <- Map.foldrWithKey' pLetBindExprs (pure Map.empty) stx'vars
  body <- traverse pSyntax stx'body
  pure (vars, body)
  where
    pLetBindExprs ::
      StxIdt ->
      Syntax ->
      Parse (Map Name Expr) ->
      Parse (Map Name Expr)
    pLetBindExprs stx'var stx'rhs rest = do
      let var = Symbol.toName (Syntax.symbol stx'var)
      rhs <- pSyntax stx'rhs
      fmap (Map.insert var rhs) rest

-- | TODO
--
-- @since 1.0.0
pLetStx :: Syntax -> Parse (Map StxIdt Syntax, NonEmpty Syntax)
pLetStx stx =
  case Datum.syntaxExpr stx of
    Datum.List (Datum.Stx (StxAtom ctx atom) : Datum.Stx stx'vars : rest) -> do
      name <- resolve ctx atom

      unless (name == "let") do
        raiseLetParseError stx Nothing

      case NonEmpty.nonEmpty rest of
        Nothing -> do
          let datum = Datum.List rest
          raiseLetParseError stx (Just datum)
        Just datums -> do
          vars <- pLetBindStxs stx'vars
          body <- for datums \case
            Datum.Stx stx'body -> pure stx'body
            other -> raiseLetParseError stx (Just other)
          pure (vars, body)
    _ ->
      raiseLetParseError stx Nothing

-- | TODO
--
-- @since 1.0.0
pLetBindStxs :: Syntax -> Parse (Map StxIdt Syntax)
pLetBindStxs (StxList _ stxs) = do
  vars <- traverse pLetBindStx stxs
  pure (Map.fromList vars)
pLetBindStxs stx =
  throwError (ExnParseLetBind stx)

-- | TODO
--
-- @since 1.0.0
pLetBindStx :: Syntax -> Parse (StxIdt, Syntax)
pLetBindStx (StxList _ [StxAtom ctx atom, stx']) = do
  pure (StxIdt ctx atom, stx')
pLetBindStx stx =
  throwError (ExnParseLetBind stx)

raiseLetParseError :: Syntax -> Maybe Datum -> Parse a
raiseLetParseError stx origin =
  let exn :: CoreParseError
      exn = CoreParseError Core.Form.Let stx origin
   in throwError (ExnParseCore exn)