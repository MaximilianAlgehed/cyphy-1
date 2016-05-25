{-# LANGUAGE ImplicitParams #-}

module TestCruiseControl where

import Prelude hiding (until)

import Data.List

import Test.QuickCheck

import Zelus
import CyphyUtils
import PastLTL

h :: Double
h = 0.05

tmax :: Double
tmax = 100

samples :: Int
samples = t2s tmax

t2s :: Double -> Int
t2s = round . (/h)

-- Overshoot
-- always ([ref_up, ref_down) -> v - ref <. 1.02 * ref)
prop_overshoot_bool = isTrue . prop_overshoot_dool
prop_overshoot_dool = foldl1 (&&.) . take samples . prop_overshoot_dools
prop_overshoot_dools ????? = step_up =>: overshoot <? max_overshoot
  where
    ref_up = up ref
    ref_down = down ref
    step_up = ref_up `intervals` ref_down
    overshoot = (v - ref) / ref
    max_overshoot 0.05
    
-- Undershoot 
prop_undershoot_bool = isTrue . prop_undershoot_dool
prop_undershoot_dool = foldl1 (&&.) . take samples . prop_undershoot_dools
prop_undershoot_dools ???? = step_down =>: undershoot <? max_undershoot
  where
    ref_up = up ref
    ref_down = down ref
    step_down = ref_down `intervals` ref_up
    undershoot = (ref - v) / ref
    max_undershoot 0.05

-- Stable regime: not over set speed after 5 seconds
-- Steady state error < 1km/h
prop_stable_bool = isTrue . prop_stable_dool
prop_stable_dool = foldl1 (&&.) . take samples . prop_stable_dools
prop_stable_dools ????? = nts is_settling =>: err <? max_err
  where
    settling_time = t2s 5
    is_settling = holds (change ref ||: start) settling_time
    err = abs (ref - v)
    max_err = 0.3

-- Rise time
-- Från refändring till ett intervall inom 1 km/h från ref
-- prop_rise_bool = isTrue . prop_rise_dool
-- prop_rise_dool = foldl1 (&&.) . take samples . prop_rise_dools
-- prop_rise_dools ????? = undefined
--   where
--     rise_time = t2s 5
--     is_rising = holds (change ref ||: start) rise_time
--     abs_err = abs ((ref - v))
--     ref_reached = (abs_err < max_err) `intervals` change ref
--     max_err = 0.3
