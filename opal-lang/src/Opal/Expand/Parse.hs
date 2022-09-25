{-# LANGUAGE ScopedTypeVariables #-}

module Opal.Expand.Parse
  ( -- * TODO
    runParseM,
    pSyntax,
    pLambda,
    pIdentifierList,
    arity1,
    arity2,
  )
where

import Control.Arrow ((^>>))

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, asks)

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Traversable (for)

--------------------------------------------------------------------------------

import Opal.Expand.Parse.Monad
  ( ParseCtx (ParseCtx),
    ParseError (ParseError),
    ParseErrorSort
      ( ExnBadCallArity,
        ExnBadSyntax,
        ExnLiteralProc,
        ExnMissingProc
      ),
    ParseM (P),
    ctx'focus,
    set'focus,
  )
import Opal.Expand.Resolve.Class (resolve)
import Opal.Expand.Syntax (StxCtx, StxIdt, Syntax)
import Opal.Expand.Syntax qualified as Syntax

import Opal.AST.Literal (Literal)
import Opal.Expand.Context (ctxEnvironment, ctxPhase)
import Opal.Expand.Monad (Expand (Expand))
import Opal.Expand.Transformer (Transform (..))
import Opal.Expr (Expr)
import Opal.Expr qualified as Expr

--------------------------------------------------------------------------------

runParseM :: e -> ParseM (ParseCtx e) a -> Expand a
runParseM e (P k) =
  Expand \ctx -> do
    result <- k (ParseCtx e ctx.ctxPhase ctx.ctxEnvironment)
    case result of
      Left exn -> error (show exn)
      Right rx -> pure (Right rx)

-- | TODO
--
-- @since 1.0.0
pResolveBind :: ParseM (ParseCtx StxIdt) Transform
pResolveBind = do
  -- TODO: explain why an unbound identifier is not an error here.
  ParseCtx idt ph binds <- ask
  bind <- resolve ph idt
  pure case bind of
    Nothing -> VarTfm idt.symbol
    Just rx -> fromMaybe (VarTfm idt.symbol) (Map.lookup rx binds)
{-# INLINE pResolveBind #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pSyntax :: ParseM (ParseCtx Syntax) Expr
pSyntax = do
  asks ctx'focus >>= \case
    Syntax.Lit _ lit ->
      const lit ^>> pLiteralSyntax
    Syntax.Idt idt ->
      set'focus idt ^>> pIdtSyntax
    Syntax.App ctx stxs ->
      case NonEmpty.nonEmpty stxs of
        Nothing ->
          throwError (ParseError (Syntax.App ctx stxs) ExnMissingProc)
        Just stxs' -> do
          set'focus stxs' ^>> pProcedureCall ctx

-- | TODO
--
-- @since 1.0.0
pSyntaxes :: Traversable t => ParseM (ParseCtx (t Syntax)) (t Expr)
pSyntaxes = do
  stxs <- asks ctx'focus
  for stxs \stx ->
    set'focus stx ^>> pSyntax
{-# SPECIALIZE INLINE pSyntaxes :: ParseM (ParseCtx (NonEmpty Syntax)) (NonEmpty Expr) #-}
{-# SPECIALIZE INLINE pSyntaxes :: ParseM (ParseCtx [Syntax]) [Expr] #-}

-- | TODO
--
-- @since 1.0.0
pLiteralSyntax :: ParseM Literal Expr
pLiteralSyntax = asks (Expr.DtmExp . Expr.LitDtm)
{-# INLINE pLiteralSyntax #-}

-- | TODO
--
-- @since 1.0.0
pProcedureCall :: StxCtx -> ParseM (ParseCtx (NonEmpty Syntax)) Expr
pProcedureCall ctx = do
  stxs <- asks ctx'focus
  case NonEmpty.head stxs of
    stx@Syntax.Lit {} ->
      throwError (ParseError stx ExnLiteralProc)
    Syntax.App {} -> do
      exps <- set'focus stxs ^>> pSyntaxes
      pure (Expr.AppExp exps)
    Syntax.Idt idt -> do
      let args = NonEmpty.tail stxs
      set'focus args ^>> pIdtCall ctx idt

-- | TODO
--
-- @since 1.0.0
pIdtCall :: StxCtx -> StxIdt -> ParseM (ParseCtx [Syntax]) Expr
pIdtCall ctx idt = do
  transformer <- fmap (const idt) ^>> pResolveBind
  case transformer of
    LamTfm -> do
      asks ctx'focus >>= \case
        [Syntax.App _ vars, stx] -> do
          vars' <- for vars \case 
            Syntax.Idt var -> pure var.symbol
            var -> error ("unexpected syntax in lambda binding position: " ++ show var)
          body <- set'focus stx ^>> pSyntax
          makeFunCall (Expr.DtmExp $ Expr.FunDtm vars' body)
        _ -> raiseCallArity 2
    StxTfm -> do
      asks ctx'focus >>= \case
        [stx] -> pure (Expr.DtmExp $ Expr.StxDtm stx)
        _ -> raiseCallArity 1
    QteTfm -> do
      undefined
    LetTfm -> do
      undefined
    VarTfm var ->
      makeFunCall (Expr.VarExp var)
    DtmTfm val -> do
      makeFunCall (Expr.DtmExp val)
    StopTfm tfm ->
      undefined
  where
    raiseCallArity :: Int -> ParseM (ParseCtx [Syntax]) a
    raiseCallArity n = do
      len <- asks (length . ctx'focus)
      raiseParseError (ExnBadCallArity n len)

    raiseParseError :: ParseErrorSort -> ParseM (ParseCtx [Syntax]) a
    raiseParseError sort = do
      args <- asks ctx'focus
      let stx = Syntax.App ctx (Syntax.Idt idt : args)
      throwError (ParseError stx sort)

    makeFunCall :: Expr -> ParseM (ParseCtx [Syntax]) Expr
    makeFunCall fun = do
      args <- pSyntaxes
      pure (Expr.AppExp (fun :| args))

-- | TODO
--
-- @since 1.0.0
pIdentifier :: ParseM Syntax StxIdt
pIdentifier = 
  ask >>= \case 
    Syntax.Idt idt -> pure idt
    stx -> throwError (ParseError stx ExnBadSyntax)

-- | TODO
--
-- @since 1.0.0
pIdentifierList :: ParseM Syntax [StxIdt]
pIdentifierList = 
  ask >>= \case 
    Syntax.Idt idt -> 
      pure [idt]
    Syntax.App _ stxs -> 
      for stxs \stx -> 
        const stx ^>> pIdentifier
    stx -> throwError (ParseError stx ExnBadSyntax)

-- | TODO
--
-- @since 1.0.0
pLambda :: ParseM (ParseCtx Syntax) ([StxIdt], Syntax)
pLambda = do
  (stx, stx') <- arity2 
  vars <- const stx ^>> pIdentifierList
  pure (vars, stx')

-- | TODO
--
-- @since 1.0.0
arity1 :: ParseM (ParseCtx Syntax) Syntax
arity1 = 
  asks ctx'focus >>= \case 
    Syntax.App _ [stx] -> pure stx
    stx@(Syntax.App _ stxs) -> throwError (ParseError stx $ ExnBadCallArity 1 (length stxs))
    stx -> throwError (ParseError stx ExnBadSyntax)

-- | TODO
--
-- @since 1.0.0
arity2 :: ParseM (ParseCtx Syntax) (Syntax, Syntax)
arity2 = 
  asks ctx'focus >>= \case 
    Syntax.App _ [stx, stx'] -> pure (stx, stx')
    stx@(Syntax.App _ stxs) -> throwError (ParseError stx $ ExnBadCallArity 2 (length stxs))
    stx -> throwError (ParseError stx ExnBadSyntax)

-- | TODO
--
-- @since 1.0.0
pIdtSyntax :: ParseM (ParseCtx StxIdt) Expr
pIdtSyntax =
  pResolveBind >>= \case
    LamTfm -> raiseBadSyntax
    StxTfm -> raiseBadSyntax
    QteTfm -> raiseBadSyntax
    LetTfm -> raiseBadSyntax
    VarTfm var -> pure (Expr.VarExp var)
    DtmTfm val -> pure (Expr.DtmExp val)
    StopTfm tfm ->
      undefined
  where
    raiseBadSyntax :: ParseM (ParseCtx StxIdt) Expr
    raiseBadSyntax = do
      stx <- asks (Syntax.Idt . ctx'focus)
      throwError (ParseError stx ExnBadSyntax)