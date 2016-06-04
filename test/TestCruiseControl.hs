{-# LANGUAGE ImplicitParams #-}

module TestCruiseControl where

--import Prelude hiding (until)

--import Data.List

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Dool
import DPtLTL

import CruiseControl

data Case =
  Case
    { rawMode :: [(Double, Mode)]
    , rawRef :: [(Double, Double)]
    , rawSlope :: Double --[(Double, Double)]
    , initV :: Double -- ^ Initial speed, m/s
    , mode :: [Mode]      -- ^ Controller mode
    , ref :: [Double]     -- ^ Cruise control speed setting, m/s
    , slope :: [Double]   -- ^ Road slope (disturbance), rad
    }

instance Show Case where
  show (Case rawMode rawRef rawSlope initV _ _ _) =
    unlines
     [ "rawMode =  " ++ show (take 10 rawMode)
     , "rawRef = " ++ show (take 10 rawRef)
     , "rawSlope = " ++ show rawSlope
     , "initV =  " ++ show initV
     ]

instance Arbitrary Case where
  arbitrary =
    do rawMode <- listOf1 (
         do dt <- choose (5, 25)
            m <- elements [Eco, Normal]
            return (dt, m))
       rawRef <- listOf1 (
         do dt <- choose (5, 25)
            v <- choose (3, 40)
            return (dt, v))
       rawSlope <- choose (-0.13, 0.13)
       initV <- choose (0, 30)
       return
         (Case rawMode rawRef rawSlope
            initV (mkRef rawMode) (mkRef rawRef) (repeat rawSlope))

  shrink (Case rawMode rawRef rawSlope initV _ _ slope) =
    let rawModes = shrinks rawMode
        rawRefs = shrinks rawRef
    in [Case rawMode' rawRef' rawSlope initV
         (mkRef rawMode') (mkRef rawRef') slope
           | rawMode' <- rawModes, rawRef' <- rawRefs]

shrinks :: [a] -> [[a]]
shrinks xs = filter p [xs1, xs2, xs3, xs4, xs5, xs6, xs7, xs8, xs9]
  where
    p x = let l = length x in 0 < l && l < m
    m = length xs
    n = m `div` 3
    xs1 = take n xs
    xs2 = take n (drop n xs)
    xs3 = drop (2*n) xs
    xs4 = xs1 ++ xs2
    xs5 = xs1 ++ xs3
    xs6 = xs2 ++ xs3
    xs7 = xs2 ++ xs1
    xs8 = drop 1 xs
    xs9 = take (m - 1) xs

mkRef :: [(Double, a)] -> [a]
mkRef [] = error "raw must be non-empty"
mkRef [(_, r)] = repeat r
mkRef ((dt, r):dtrs) = replicate (round (tmax/h)) r ++ mkRef dtrs

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
prop_overshoot_dools (Case _ _ _ initV mode ref slope) =
    step_up =>: overshoot <: max_overshoot
  where
    v = let ?h = h in run initV mode ref slope
    ref_up = (up ref ||: start) &&: ref >: v
    ref_down = down ref
    step_up = ref_up `intervals` ref_down
    overshoot = (v - ref) / ref
    max_overshoot = 0.03

-- Undershoot
prop_undershoot_bool = isTrue . prop_undershoot_dool
prop_undershoot_dool = foldl1 (&&.) . take samples . prop_undershoot_dools
prop_undershoot_dools (Case _ _ _ initV mode ref slope) =
    step_down =>: undershoot <: max_undershoot
  where
    v = let ?h = h in run initV mode ref slope
    ref_up = up ref
    ref_down = (down ref ||: start) &&: ref <: v
    step_down = ref_down `intervals` ref_up
    undershoot = (ref - v) / ref
    max_undershoot = 0.03

-- Stable regime: not over set speed after 5 seconds
-- Steady state error < 1km/h
prop_stable_bool = isTrue . prop_stable_dool
prop_stable_dool = foldl1 (&&.) . take samples . prop_stable_dools
prop_stable_dools (Case _ _ _ initV mode ref slope) =
    nts is_settling =>: err <: max_err
  where
    v = let ?h = h in run initV mode ref slope
    settling_time = t2s 5
    is_settling = holds (change ref ||: start) settling_time
    err = map abs (ref - v)
    max_err = 0.3

-- Rise time
-- Från refändring till ett intervall inom 1 km/h från ref
prop_rise_bool = isTrue . prop_rise_dool
prop_rise_dool = foldl1 (&&.) . take samples . prop_rise_dools
prop_rise_dools (Case _ _ _ initV mode ref slope) =
    nts is_rising =>: ref_reached
  where
    v = let ?h = h in run initV mode ref slope
    rise_time = t2s 5
    is_rising = holds (change ref ||: start) rise_time
    err = map abs (ref - v)
    ref_reached = (err <: max_err) `intervals` change ref
    max_err = 0.3

main = defaultMain $ testGroup "test cruise control" [props]

props = testGroup "properties"
  [ testProperty "overshoot" prop_overshoot_bool
  , testProperty "undershoot" prop_undershoot_bool
  , testProperty "stable regime" prop_stable_bool
  , testProperty "rise time" prop_rise_bool
  ]
