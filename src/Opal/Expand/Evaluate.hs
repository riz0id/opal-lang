{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Evaluate (
  exprEval,
) where

import Control.Lens (over, view, set)

import Control.Monad.State.Strict (modify', gets)
import Control.Monad (unless)

import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List.NonEmpty (NonEmpty ((:|)))

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.GenSym (newGenSymWith, newGenSym)
import Opal.Common.GenSym qualified as GenSym

import Opal.Core (SExp (..), Expr)
import Opal.Core.Datum (Datum)
import qualified Opal.Core.Datum as Datum
import Opal.Core.Prim (CorePrim)
import Opal.Core.Prim qualified as Core.Prim

import Opal.Expand.Core (indexTransformers)
import Opal.Expand.Monad (Expand, stwEnvironment)
import Opal.Expand.Resolve.Class ( resolveName )
import Opal.Expand.Syntax (Syntax (..), StxIdt (..))
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Transform (Transform (..))
import Opal.Expand.Transform qualified as Transform
import Data.Foldable (foldr')

--------------------------------------------------------------------------------


-- | TODO
--
-- @since 1.0.0
exprEval :: Expr -> Expand Datum
exprEval (SExpVal val) = pure val
exprEval (SExpVar name) = varEval name
exprEval (SExpApp func args) = appEval func args
exprEval (SExpIf c e0 e1) = ifEval c e0 e1
exprEval (SExpLet vars body) = letEval vars body
{-# INLINE exprEval #-}

-- | TODO
--
-- @since 1.0.0
varEval :: Name -> Expand Datum
varEval var = fmap Transform.toDatum (indexTransformers var)

-- | TODO
--
-- @since 1.0.0
appEval :: Expr -> [Expr] -> Expand Datum
appEval (SExpVar func) args = do
  func' <- varEval func
  appDatumEval func' args
appEval (SExpVal val) args =
  appDatumEval val args
appEval (SExpApp fun args') args = do
  val <- appEval fun args'
  appDatumEval val args
appEval (SExpIf c e0 e1) args = do
  val <- ifEval c e0 e1
  appDatumEval val args
appEval (SExpLet vars body) args = do
  val <- letEval vars body
  appDatumEval val args
{-# INLINE appEval #-}

-- | TODO
--
-- @since 1.0.0
ifEval :: Expr -> Expr -> Expr -> Expand Datum
ifEval c e0 e1 = do 
  val <- exprEval c
  if val == Datum.Bool False 
    then exprEval e1 
    else exprEval e0

-- | TODO
--
-- @since 1.0.0
letEval :: Map Name Expr -> NonEmpty Expr -> Expand Datum
letEval vars (b :| bs) = do
  vars' <- traverse exprEval vars
  let varBinds :: [(Name, Transform)]
      varBinds = Map.foldrWithKey (\var val rest -> (var, Transform.Dtm val) : rest) [] vars'
  old <- gets (view stwEnvironment )
  modify' (over stwEnvironment \env -> foldr (uncurry Map.insert) env varBinds)
  ret <- foldr' ((>>) . exprEval) (exprEval b) bs 
  modify' (set stwEnvironment old)
  pure ret 

-- | TODO
--
-- @since 1.0.0
appDatumEval :: Datum -> [Expr] -> Expand Datum
appDatumEval fun@(Datum.Proc vars body) args = do
  checkCallArity (length vars) fun args
  vals <- traverse exprEval args
  let argBinds :: [(Name, Transform)]
      argBinds = zipWith (\var val -> (var, Transform.Dtm val)) vars vals
  old <- gets (view stwEnvironment )
  modify' (over stwEnvironment \env -> foldr (uncurry Map.insert) env argBinds)
  ret <- foldr ((>>) . exprEval) (exprEval (NonEmpty.head body)) body
  modify' (set stwEnvironment old)
  pure ret
appDatumEval (Datum.Prim prim) args = do
  appCorePrimEval prim args
appDatumEval val args = do
  let expr = SExpApp (SExpVal val) args
  error ("evaluation error: application to literal: " ++ show expr)
{-# INLINE appDatumEval #-}

-- | TODO
--
-- @since 1.0.0
appCorePrimEval :: CorePrim -> [Expr] -> Expand Datum
appCorePrimEval Core.Prim.Apply = primApplyEval
appCorePrimEval Core.Prim.Cons = primConsEval
appCorePrimEval Core.Prim.DatumToSyntax = primDatumToSyntaxEval
appCorePrimEval Core.Prim.GenSym = primGenSymEval
appCorePrimEval Core.Prim.Head = primHeadEval
appCorePrimEval Core.Prim.List = primListEval
appCorePrimEval Core.Prim.IsProcedure = primIsProcedureEval
appCorePrimEval Core.Prim.IsList = primIsListEval
appCorePrimEval Core.Prim.IsPair = primIsPairEval
appCorePrimEval Core.Prim.IsSyntax = primDatumIsSyntaxEval
appCorePrimEval Core.Prim.IsSymbol = primDatumIsSymbolEval
appCorePrimEval Core.Prim.Map = primMapEval
appCorePrimEval Core.Prim.SetBang = primSetBangEval
appCorePrimEval Core.Prim.SyntaxToDatum = primSyntaxToDatumEval
appCorePrimEval Core.Prim.SyntaxLocalValue = primSyntaxLocalValueEval
appCorePrimEval Core.Prim.SyntaxExpr = primSyntaxExprEval
appCorePrimEval Core.Prim.Tail = primTailEval

-- | TODO
--
-- @since 1.0.0
primApplyEval :: [Expr] -> Expand Datum
primApplyEval [arg1, arg2] = do
  fun <- exprEval arg1
  val <- exprEval arg2
  case val of
    Datum.List args -> appDatumEval fun (map SExpVal args)
    _ -> pure (error ("evaluation error: contract violation: expected arg 2# of 'apply' to be list: " ++ show val))
primApplyEval args = do
  pure (error ("evaluation error: unexpected number of arguments to apply: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primConsEval :: [Expr] -> Expand Datum 
primConsEval [arg1, arg2] = do 
  val1 <- exprEval arg1
  val2 <- exprEval arg2
  case val2 of 
    Datum.List rest -> pure (Datum.List (val1 : rest))
    _ -> pure (Datum.Pair val1 val2)
primConsEval args = do
  pure (error ("evaluation error: unexpected number of arguments to cons: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primGenSymEval :: [Expr] -> Expand Datum
primGenSymEval [] = do
  fmap (Datum.Atom . GenSym.toSymbol) newGenSym
primGenSymEval [arg] = do
  arg' <- exprEval arg
  case arg' of
    Datum.Atom atom -> do
      gensym <- newGenSymWith atom
      pure (Datum.Atom (GenSym.toSymbol gensym))
    _ -> pure (error ("evaluation error: contract violation: expected arg 2# of 'apply' to be list: " ++ show arg'))
primGenSymEval args = do
  pure (error ("evaluation error: unexpected number of arguments to gensym: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primListEval :: [Expr] -> Expand Datum
primListEval args = do
  elts <- traverse exprEval args
  pure (Datum.List elts)

-- | TODO
--
-- @since 1.0.0
primHeadEval :: [Expr] -> Expand Datum
primHeadEval [arg] = do
  exprEval arg >>= \case
    Datum.List (x : _) -> pure x
    _ -> pure (Datum.Bool False)
primHeadEval _ = do
  pure (Datum.Bool False)

-- | TODO
--
-- @since 1.0.0
primTailEval :: [Expr] -> Expand Datum
primTailEval [arg] = do
  exprEval arg >>= \case
    Datum.List (_ : xs) -> pure (Datum.List xs)
    _ -> pure (Datum.Bool False)
primTailEval _ = do
  pure (Datum.Bool False)

-- | TODO
--
-- @since 1.0.0
primIsProcedureEval :: [Expr] -> Expand Datum
primIsProcedureEval [arg] = do
  elts <- exprEval arg
  pure (Datum.Bool (Datum.isProcDatum elts))
primIsProcedureEval args = do
  pure (error ("evaluation error: unexpected arguments to procedure?: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primDatumIsSymbolEval :: [Expr] -> Expand Datum
primDatumIsSymbolEval [arg] = do
  elts <- exprEval arg
  pure (Datum.Bool (Datum.isSymbolDatum elts))
primDatumIsSymbolEval args = do
  pure (error ("evaluation error: unexpected arguments to symbol?: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primIsPairEval :: [Expr] -> Expand Datum
primIsPairEval [arg] = do
  elts <- exprEval arg
  pure (Datum.Bool (Datum.isPairDatum elts))
primIsPairEval args = do
  pure (error ("evaluation error: unexpected arguments to pair?: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primIsListEval :: [Expr] -> Expand Datum
primIsListEval [arg] = do
  elts <- exprEval arg
  pure (Datum.Bool (Datum.isListDatum elts))
primIsListEval args = do
  pure (error ("evaluation error: unexpected arguments to list?: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primMapEval :: [Expr] -> Expand Datum
primMapEval [arg1, arg2] = do
  arg1' <- exprEval arg1
  case arg1' of
    func@Datum.Proc {} -> do
      arg2' <- exprEval arg2 
      case arg2' of 
        Datum.List xs -> do 
          xs' <- traverse (appDatumEval func . pure . SExpVal) xs
          pure (Datum.List xs')
        Datum.Pair x y -> do 
          x' <- appDatumEval func [SExpVal x] 
          y' <- appDatumEval func [SExpVal y] 
          pure (Datum.Pair x' y')
        _ -> do 
          appDatumEval func [SExpVal arg2']
    _ -> pure (error ("evaluation error: contract violation: expected arg 1# of 'map' to be a procedure: " ++ show arg1'))
primMapEval args = do
  pure (error ("evaluation error: unexpected arguments to map?: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primDatumIsSyntaxEval :: [Expr] -> Expand Datum
primDatumIsSyntaxEval [arg] = do
  elts <- exprEval arg
  pure (Datum.Bool (Datum.isSyntaxDatum elts))
primDatumIsSyntaxEval args = do
  pure (error ("evaluation error: unexpected arguments to list?: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primDatumToSyntaxEval :: [Expr] -> Expand Datum
primDatumToSyntaxEval [arg1, arg2] = do
  val1 <- exprEval arg1
  case val1 of 
    Datum.Stx stx -> do 
      val2 <- exprEval arg2
      let stx' = Datum.datumToSyntax stx val2 stx.context.location
      pure (Datum.Stx stx')
    _ -> 
      pure (error ("evaluation error: contract violation datum->syntax: " ++ show val1))
primDatumToSyntaxEval args = do
  pure (error ("evaluation error: unexpected arguments to datum->syntax: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primSyntaxToDatumEval :: [Expr] -> Expand Datum
primSyntaxToDatumEval [arg] = do
  exprEval arg >>= \case
    Datum.Stx stx -> pure (Datum.syntaxToDatum stx)
    other -> pure (error ("evaluation error: contract violation syntax->datum: " ++ show other))
primSyntaxToDatumEval args = do
  pure (error ("evaluation error: unexpected arguments to syntax->datum: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primSyntaxLocalValueEval :: [Expr] -> Expand Datum
primSyntaxLocalValueEval [arg] = do
  arg' <- exprEval arg

  name <- case arg' of
    Datum.Stx (StxAtom ctx atom) -> resolveName (StxIdt ctx atom)
    _ -> pure (error ("evaluation error: unexpected argument to syntax-local-value: " ++ show arg'))

  indexTransformers name >>= \case
    Transform.Dtm val -> pure val
    Transform.Var idt -> pure (Datum.Stx idt.syntax)
    other -> pure (error ("evaluation error: (syntax-local-value " ++ show arg ++ "):  did not yield syntax: " ++ show other))
primSyntaxLocalValueEval args = do
  pure (error ("evaluation error: unexpected arguments to syntax-local-value: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primSetBangEval :: [Expr] -> Expand Datum
primSetBangEval [arg1, arg2] = do
  case arg1 of 
    SExpVar var -> do 
      val <- exprEval arg2
      env <- gets (view stwEnvironment)
      if Map.member var env 
        then do 
          modify' (over stwEnvironment (Map.insert var (Transform.Dtm val)))
          pure Datum.Void
        else do 
          pure (error ("set!: cannot set variable before it is initialized: " ++ show var))
    _ -> 
      pure (error ("evaluation error: contract violation set!: arg #1: " ++ show arg1))
primSetBangEval args = do
  pure (error ("evaluation error: unexpected arguments to set!: " ++ show args))

-- | TODO
--
-- @since 1.0.0
primSyntaxExprEval :: [Expr] -> Expand Datum
primSyntaxExprEval [arg] = do
  exprEval arg >>= \case
    Datum.Stx stx -> pure (Datum.syntaxExpr stx)
    other -> pure (error ("evaluation error: unexpected argument to syntax-local-value: " ++ show other))
primSyntaxExprEval args = do
  pure (error ("evaluation error: unexpected arguments to syntax-expr: " ++ show args))

-- | TODO
--
-- @since 1.0.0
checkCallArity :: Int -> Datum -> [Expr] -> Expand ()
checkCallArity n fun args =
  let numArgs :: Int
      numArgs = length args
   in unless (n == numArgs) do
        let expr = SExpApp (SExpVal fun) args
        pure $ error ("evaluation error: arity mismatch: expected " ++ show n ++ ", but got " ++ show numArgs ++ " in application " ++ show expr)
{-# INLINE checkCallArity #-}