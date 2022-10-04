module Opal.Print
  ( -- * TODO
    pprSyntax,
    pprStxCtx,
    pprStxAtom,
    pprStxList,
  )
where

import Data.SrcLoc (SrcLoc (coln, line))

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

import Opal.Expand.Syntax (StxCtx, Syntax (StxAtom, StxList))
import Opal.Expand.Syntax qualified as Syntax

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pprSyntax :: Syntax -> String
pprSyntax (StxAtom ctx atom) = pprStxAtom ctx atom
pprSyntax (StxList ctx stxs) = pprStxList ctx stxs

-- | TODO
--
-- @since 1.0.0
pprStxCtx :: StxCtx -> String
pprStxCtx ctx = shows ctx.location.line ":" ++ show ctx.location.coln

-- | TODO
--
-- @since 1.0.0
pprStxAtom :: StxCtx -> Symbol -> String
pprStxAtom ctx atom = "#<syntax:" ++ pprStxCtx ctx ++ ": " ++ shows atom ">"

-- | TODO
--
-- @since 1.0.0
pprStxList :: StxCtx -> [Syntax] -> String
pprStxList ctx stxs =
  "#<syntax:" ++ pprStxCtx ctx ++ " (" ++ unwords (map pprStxElem stxs) ++ ")>"
  where
    pprStxElem :: Syntax -> String
    pprStxElem (StxAtom _ atom) = Symbol.unpack atom
    pprStxElem (StxList _ stxs') = "(" ++ unwords (map pprStxElem stxs') ++ ")"
