{-# LANGUAGE ImplicitParams #-}

module TestCruiseControl where

import Prelude hiding (until)

import Data.List

import Test.QuickCheck

import Zelus
import CyphyUtils
import PastLTL

-- Overshoot
-- always ([ref_up, ref_down) -> v - ref <. 1.02 * ref)
prop_overshoot = undefined
  where
    ref_up = up ref
    ref_down = down ref
-- Undershoot

-- Stable regime: not over set speed after 5 seconds
-- Steady state error < 1km/h

-- Rise time
-- Från refändring till mellan 98% och 102% av refvärde
--
