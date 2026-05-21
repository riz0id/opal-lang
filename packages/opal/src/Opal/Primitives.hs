{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Primitives
-- Copyright   :  (c) Jacob Leach, 2026
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- The Stage-1 primitive table. Implementations of the built-in functions
-- accessible from the transformer language:
--
-- * List primitives — @car@, @cdr@, @cons@, @null?@, @pair?@.
-- * Equality — @eq?@.
-- * Syntax inspectors — @syntax-e@, @syntax->list@, @syntax->datum@,
--   @datum->syntax@, @identifier?@, @syntax?@.
--
-- Each primitive is registered under its 'Symbol' name in 'primitiveTable'.
-- 'Datum' references primitives indirectly via @DatumPrim sym@; the
-- evaluator resolves @sym@ via 'lookupPrimitive' at apply time.
--
-- Registration into the @#%core@ module's environment + binding store
-- happens in "Opal.Binding.Environment" and "Opal.Binding.BindingStore"
-- respectively — both consume 'primitiveSymbols' to enumerate the names.
--
-- @since 1.0.0
module Opal.Primitives
  ( -- * Primitive table
    primitiveTable
  , lookupPrimitive
  , primitiveSymbols
  )
where

import Control.Lens (preview, view)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Opal.Common.Symbol (Symbol)
import Opal.Syntax
  ( Datum (..)
  , Syntax (..)
  , datumToSyntax
  , syntaxId
  , syntaxInfo
  , syntaxToDatum
  )
import Opal.Syntax.Primitive (Primitive (..))

import Prelude hiding (id, lookup)

-- Primitive table -------------------------------------------------------------

-- | The full Stage-1 primitive table, keyed by name. Symbol-to-implementation
-- lookup happens here; 'Datum.DatumPrim' carries only the symbol.
--
-- @since 1.0.0
primitiveTable :: Map Symbol Primitive
primitiveTable = Map.fromList
  [ (prim_name p, p) | p <-
      [ primCar
      , primCdr
      , primCons
      , primNullP
      , primPairP
      , primEqP
      , primSyntaxE
      , primSyntaxToList
      , primSyntaxToDatum
      , primDatumToSyntax
      , primIdentifierP
      , primSyntaxP
      ]
  ]

-- | Look up a primitive by name. The 'DatumPrim' case in the evaluator
-- uses this to find the implementation for an applied primitive.
--
-- @since 1.0.0
lookupPrimitive :: Symbol -> Maybe Primitive
lookupPrimitive sym = Map.lookup sym primitiveTable

-- | The full set of primitive names. Consumed by
-- "Opal.Binding.Environment" and "Opal.Binding.BindingStore" to register
-- entries for each primitive in the @#%core@ module's namespace.
--
-- @since 1.0.0
primitiveSymbols :: [Symbol]
primitiveSymbols = Map.keys primitiveTable

-- Helpers ---------------------------------------------------------------------

-- | Construct a primitive from a name, arity, and an apply function.
--
-- @since 1.0.0
mkPrim :: Symbol -> Int -> ([Datum] -> Either String Datum) -> Primitive
mkPrim = Primitive

arityErr :: String -> Either String a
arityErr name = Left (name ++ ": wrong number of arguments")

typeErr :: String -> String -> Either String a
typeErr name msg = Left (name ++ ": " ++ msg)

-- List primitives -------------------------------------------------------------

primCar :: Primitive
primCar = mkPrim "car" 1 \case
  [DatumList (x : _)]                   -> Right x
  [DatumStx  (SyntaxList (s : _) _)]    -> Right (DatumStx s)
  [_]                                   -> typeErr  "car" "argument is not a non-empty list"
  _                                     -> arityErr "car"

primCdr :: Primitive
primCdr = mkPrim "cdr" 1 \case
  [DatumList (_ : xs)]                  -> Right (DatumList xs)
  [DatumStx  (SyntaxList (_ : ss) info)] -> Right (DatumStx (SyntaxList ss info))
  [_]                                   -> typeErr  "cdr" "argument is not a non-empty list"
  _                                     -> arityErr "cdr"

primCons :: Primitive
primCons = mkPrim "cons" 2 \case
  [x, DatumList xs]                     -> Right (DatumList (x : xs))
  [_, _]                                -> typeErr  "cons" "second argument must be a list"
  _                                     -> arityErr "cons"

primNullP :: Primitive
primNullP = mkPrim "null?" 1 \case
  [DatumList []]                        -> Right (DatumB True)
  [DatumStx  (SyntaxList [] _)]         -> Right (DatumB True)
  [_]                                   -> Right (DatumB False)
  _                                     -> arityErr "null?"

primPairP :: Primitive
primPairP = mkPrim "pair?" 1 \case
  [DatumList (_ : _)]                   -> Right (DatumB True)
  [DatumStx  (SyntaxList (_ : _) _)]    -> Right (DatumB True)
  [_]                                   -> Right (DatumB False)
  _                                     -> arityErr "pair?"

-- Equality --------------------------------------------------------------------

primEqP :: Primitive
primEqP = mkPrim "eq?" 2 \case
  [a, b]                                -> Right (DatumB (a == b))
  _                                     -> arityErr "eq?"

-- Syntax inspectors -----------------------------------------------------------

-- | @syntax-e@: peel one layer of syntax wrap. For a list-shaped syntax,
-- returns a list of its element syntaxes. For atomic syntax, returns the
-- underlying datum (boolean, char, symbol, number, lambda).
primSyntaxE :: Primitive
primSyntaxE = mkPrim "syntax-e" 1 \case
  [DatumStx (SyntaxList stxs _)]        -> Right (DatumList (map DatumStx stxs))
  [DatumStx (SyntaxVal  val  _)]        -> Right (DatumVal val)
  [DatumStx (SyntaxLam  fun  _)]        -> Right (DatumLam fun)
  [_]                                   -> typeErr  "syntax-e" "argument is not a syntax object"
  _                                     -> arityErr "syntax-e"

-- | @syntax->list@: for a list-shaped syntax, returns a list of element
-- syntaxes. For a non-list syntax, returns @#f@ (Racket convention).
primSyntaxToList :: Primitive
primSyntaxToList = mkPrim "syntax->list" 1 \case
  [DatumStx (SyntaxList stxs _)]        -> Right (DatumList (map DatumStx stxs))
  [DatumStx _]                          -> Right (DatumB False)
  [_]                                   -> typeErr  "syntax->list" "argument is not a syntax object"
  _                                     -> arityErr "syntax->list"

-- | @syntax->datum@: recursively strip all syntax wrappers, returning a pure
-- datum.
primSyntaxToDatum :: Primitive
primSyntaxToDatum = mkPrim "syntax->datum" 1 \case
  [DatumStx stx]                        -> Right (syntaxToDatum stx)
  [_]                                   -> typeErr  "syntax->datum" "argument is not a syntax object"
  _                                     -> arityErr "syntax->datum"

-- | @datum->syntax ctxt v@: lift a datum @v@ to a syntax object using
-- @ctxt@'s lexical context (scope set + source info).
primDatumToSyntax :: Primitive
primDatumToSyntax = mkPrim "datum->syntax" 2 \case
  [DatumStx ctxt, datum]                ->
    Right (DatumStx (datumToSyntax (view syntaxInfo ctxt) datum))
  [_, _]                                -> typeErr  "datum->syntax" "first argument must be a syntax object"
  _                                     -> arityErr "datum->syntax"

primIdentifierP :: Primitive
primIdentifierP = mkPrim "identifier?" 1 \case
  [DatumStx stx]
    | Just _ <- preview syntaxId stx    -> Right (DatumB True)
    | otherwise                         -> Right (DatumB False)
  [_]                                   -> Right (DatumB False)
  _                                     -> arityErr "identifier?"

primSyntaxP :: Primitive
primSyntaxP = mkPrim "syntax?" 1 \case
  [DatumStx _]                          -> Right (DatumB True)
  [_]                                   -> Right (DatumB False)
  _                                     -> arityErr "syntax?"

