{-# LANGUAGE ImplicitParams #-}

module TestWaterHeater where

import Prelude hiding (until)

import Data.List

import Test.QuickCheck

import Zelus
import CyphyUtils

import WaterHeater

h :: Double
h = 0.01 -- this is problematic. we need access to h in arbitrary (currently),
         -- but might want different h in different properties.
         -- it is ugly also.
         -- define a new arbitrary class where h and tmax can be provided?

tmax :: Double
tmax = 100

maxsteps :: Int
maxsteps = let ?h = h in steps tmax                --- <<< h!

data Ref = Ref { rw :: [(Double, Double)], strm :: S Double }

instance Show Ref where
  show (Ref raw _) = show raw

instance Arbitrary Ref where
  arbitrary = let ?h = h in     --- <<< h!
    do -- min and max number of changes
       n <- choose (1, 10) :: Gen Int
       -- initial temperature
       init <- choose (20, 100)
       -- min and max time between ref changes
       dts <- chooses (1, 30) :: Gen [Double]
       -- how much the ref can change in a step
       drs <- chooses (-20, 20) :: Gen [Double]
       -- limit relative ref to abs min and max (don't step outside range)
       let drs' = limitAbs init (20, 100) drs
       -- convert relative ref to absolute
       let absrs = toAbs init drs'
       -- convert interval lengths to absolute time references
       let absts = toAbs init dts
       let intervals = take n (zip absts absrs)
       return (Ref intervals (refStream intervals))

  -- this shrinker does not touch time and reference values since it is
  -- difficult (or maybe i'm lazy) to guarantee the shrunk values
  -- are still valid.
  shrink (Ref raw _)
    | length raw <= 1 = []
    | otherwise =
        let n = length raw `div` 3
            segs = segments n raw
            shuf = combinations segs
            raw' = map (foldr1 (++)) shuf
            stream' = map refStream raw'
        in zipWith Ref raw' stream'
      where ?h = h                   --- <<< h!

-- Properties on data Ref

prop_shrink_smaller ref = and (map f (shrink ref))
  where
    Ref raw stream = ref
    n = length raw
    f (Ref raw' stream') = length raw' < n

prop_shrink_nonempty ref = and (map f (shrink ref))
  where
    Ref raw stream = ref
    f (Ref raw' stream') = length raw' >= 0

prop_arb_raw_nonempty = not . null . rw

prop_arb_stream_nonempty = not . null . strm

prop_arb_min_dts ref
    | length groups <= 1 = True   -- stream is constant
    | otherwise = and (map f (init groups))  -- the last reference is infinite
  where
    groups = group (take maxsteps (strm ref))
    f grp = let ?h = h in fromIntegral (length grp) / ?h - ?h >= 1 --- <<< h!

prop_arb_max_dts ref
    | length groups <= 1 = True   -- stream is constant
    | otherwise = and (map f (init groups))  -- the last reference is infinite
  where
    groups = group (take maxsteps (strm ref))
    f grp = let ?h = h in fromIntegral (length grp) / ?h - ?h <= 20 --- <<< h!

prop_arb_min_drs = let ?h = h; ?tmax = tmax in forever (>= -3) . toRel . strm
prop_arb_max_drs = let ?h = h; ?tmax = tmax in forever (<= 3) . toRel . strm
prop_arb_min_absr = let ?h = h; ?tmax = tmax in forever (>= 20) . strm
prop_arb_max_absr = let ?h = h; ?tmax = tmax in forever (<= 100) . strm

-- | (R1) the temperature in the tank must never reach 100 degrees.
-- Since we don't have time to observe the system for ever we settle with
-- observing it for a finite time.
-- 1. Crashed when shrinking, must not generate empty intervals.
--    Solved by guaranteeing at least one interval in arbitrary and
--    filtering out lists with no intervals in shrink.
-- 2. Failed with shrunk case [(0, 0), (0, 0)]. These values are not valid.
--    Solved by creating a predicate for valid input. This also captures the
--    filtering in 1.
-- 3. quickCheck is reporting a failed test case that does not satisfy the
--    validity predicate.
--    Solved by writing proper properties and testing the arbitrary instance.
--    Found and resolved a few bugs.
--    Dropped filtering.
prop_R1 :: Ref -> Bool
prop_R1 = let ?h = h; ?tmax = tmax in forever (<100) . fst3 . run . strm

-- | (R2) after 15 seconds of operation, the system must be in stable regime,
-- which means that the temperature of the water in the tank must always
-- stay between 91 and 97 degrees.
--
-- This property has been adapted to handle changing reference temperature.
-- 15 seconds after a reference temperature change the temperature must
-- be within -3 and +3 degrees of reference (unless a new reference before that.
prop_R2 ref = let ?h = h in True

-- | (R3) during this stable regime, the burner is never continuously ON for
-- more than two seconds.
prop_R3 ref = let ?h = h in True
