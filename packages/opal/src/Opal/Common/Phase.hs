{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.Phase
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Module phases, phase levels, and phase shifts.
--
-- == Phases
--
-- A 'Phase' referred to as just a "phase" when the phase is associated with a
-- module. A module's "phase" is the phase at which that module is instantiated.
-- In the simpliest case, a top-level module would be instantiated at phase 0.
--
-- @
-- ;; The module "example" is instantiated at phase 0.
-- (module example opal
--   (begin-for-syntax
--     ;; The definition "one" is defined at phase level 1.
--     (define one 1))
--
--   ;; The definition "hello-world" is defined at phase level 0.
--   (define hello-world "Hello, World!"))
-- @
--
-- == Phase Levels
--
-- The 'Phase' type is also used to represent "phase levels". The term "phase
-- level" is only used when the 'Phase' is associated to a module-level form.
-- This is because a "phase level" is a relative to a "phase" of the enclosing
-- module's body.
--
-- The terms "phase" and "phase level" are sometimes used interchangably when
-- discussing modules or submodules. This is because modules can be declared
-- within modules or imported at a phase level other than 0
--
-- @
-- ;; Instantiated module "A" at phase 0.
-- (module A opal
--   ;; Define "x" at phase level 0.
--   (define x 1)
--   (export x))
--
-- ;; Instantiated module "B" at phase 0.
-- (module B opal
--   ;; Import module "B" at phase level 1.
--   (import (for-syntax A))
--   ;; "x" has a binding at phase level 1, but not phase level 0.
--   (quote-syntax x))
-- @
--
-- == Phase Shifts
--
-- The 'PhaseShift' type is used to represent a "phase shift". Phase shifts can
-- be combine with a 'Phase' to produce a "shifted" 'Phase' via 'phasePlus' and
-- 'phaseMinus'.
--
-- @since 1.0.0
module Opal.Common.Phase
  ( -- * Phase
    Phase (..)
    -- ** Basic Operations
  , phaseZero
  , phasePlus
  , phaseMinus
    -- ** Query
  , isPhaseZero
    -- * PhaseShift
  , PhaseShift (..)
  ) where

import Control.DeepSeq (NFData)

import Data.Default (Default (..))

import Language.Haskell.TH (Pat(..))
import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.TH (Pattern (..))
import Opal.Writer (Display(..), (<+>))
import Opal.Writer qualified as Doc

-- Phase -----------------------------------------------------------------------

-- | The 'Phase' type represents a "phase" or a "phase levels", depending on
-- the context in which the 'Phase' is used.
--
-- @since 1.0.0
newtype Phase = Phase
  { getPhase :: Word }
  deriving newtype (Eq, NFData, Ord, Show)
  deriving (Lift)

-- | 'Phase' defaults to 'phaseZero', the base phase.
--
-- @since 1.0.0
instance Default Phase where
  def = phaseZero

-- | @since 1.0.0
instance Display Phase where
  display (Phase ph) = Doc.group (Doc.string "(#%phase" <+> display ph <> Doc.char ')')

-- | @since 1.0.0
instance Pattern Phase where
  liftPat (Phase ph) = fmap (\x -> ConP 'Phase [] [x]) (liftPat ph)

-- Phase - Basic Operations ----------------------------------------------------

-- | Phase 0 is the phase reserved for plain (or "runtime") definitions.
--
-- >>> phaseZero
-- 0
--
-- @since 1.0.0
phaseZero :: Phase
phaseZero = Phase 0

-- | Shifts the given phase /up/ by the amount of the phase shift.
--
-- >>> phasePlus phaseZero 1
-- 1
--
-- @since 1.0.0
phasePlus :: Phase -> PhaseShift -> Phase
phasePlus (Phase p) (PhaseShift shift) = Phase (p + shift)

-- | Shifts the given phase /down/ by the amount of the phase shift.
--
-- @since 1.0.0
phaseMinus :: Phase -> PhaseShift -> Phase
phaseMinus (Phase p) (PhaseShift shift)
  | shift < p = Phase (p - shift)
  | otherwise = phaseZero

-- Phase - Query ---------------------------------------------------------------

-- | Is the given 'Phase' equivalent to phase level 0?
--
-- >>> isPhaseZero phaseZero
-- True
--
-- @since 1.0.0
isPhaseZero :: Phase -> Bool
isPhaseZero p = p == phaseZero

-- PhaseShift ------------------------------------------------------------------

-- | The 'PhaseShift' type is used to represent a delta that can be combined
-- with a 'Phase' to produce a "shifted" 'Phase'.
--
-- @since 1.0.0
newtype PhaseShift = PhaseShift Word
  deriving newtype (Eq, NFData, Num, Ord, Show)

-- | @since 1.0.0
instance Default PhaseShift where
  def = PhaseShift 0

-- | @since 1.0.0
instance Display PhaseShift where
  display (PhaseShift p) = Doc.group (Doc.hsep [Doc.string "phase-shift", display p])
