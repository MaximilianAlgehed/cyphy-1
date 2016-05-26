{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TestWaterHeater where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Zelus (val, isEvent, unE)

import Dool
import DPtLTL

import WaterHeater

data Case =
  Case
    { raw :: [(Double,Double)]
    , dy :: Double
    , dz :: Double
    , ref :: [Double]
    }

instance Show Case where
  show (Case raw dy dz _) =
    unlines
     [ "dy  =  " ++ show dy
     , "dz  =  " ++ show dz
     , unlines (zipWith (++) ("raw = " : repeat "      ") (map show raw))
     ]

instance Arbitrary Case where
  arbitrary =
    do dy <- choose (0.9, 1.1)
       dz <- choose (0.9, 1.1)
       raw <- listOf1 (
         do dt <- choose (5, 25)
            r <- choose (21, 99)
            return (dt, r))
       return (Case raw dy dz (mkRef raw))

  shrink (Case raw dy dz ref) =
    let dys = take 3 (dy `shrinkTo` 1)
        dzs = take 3 (dz `shrinkTo` 1)
        raws = shrinks raw -- no shrinking dt or r currently
    in [Case raw' dy dz (mkRef raw') | raw' <- raws, not (null raw')]
         ++ [Case raw dy' dz ref | dy' <- dys]
           ++ [Case raw dy dz' ref | dz' <- dzs]

shrinks :: [a] -> [[a]]
shrinks xs = filter ((<m).length) [xs1, xs2, xs3, xs4, xs5, xs6, xs7]
  where
    m = length xs
    n = m `div` 3
    xs1 = take n xs
    xs2 = take n (drop n xs)
    xs3 = drop (2*n) xs
    xs4 = xs1 ++ xs2
    xs5 = xs1 ++ xs3
    xs6 = xs2 ++ xs3
    xs7 = xs2 ++ xs1

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

limitTo :: [a] -> Int -> [a]
xs `limitTo` n = go xs
  where
    m = length xs `div` n
    go [] = []
    go (x:xs) = x : go (drop m xs)

shrinkTo :: (Arbitrary a, Ord a, Num a) => a -> a -> [a]
x `shrinkTo` root = filter bounded xs  -- filter is a hack
  where
    bounded x' = x' <= max x root && x' >= min x root
    xs = map (+root) (shrink (x - root))

prop_not_x :: Double -> Double -> Bool
prop_not_x x root = all (/=x) (x `shrinkTo` root)

prop_non_empty :: Double -> Double -> Property
prop_non_empty x root = x /= root ==> not (null (x `shrinkTo` root))

prop_in_range :: Double -> Double -> Bool
prop_in_range x root =
  all (\y -> y <= max x root && y >= min x root) (x `shrinkTo` root)

mkRef :: [(Double, Double)] -> [Double]
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

-- | (R1) the temperature in the tank must never reach 100 degrees.
prop_R1_bool = isTrue . prop_R1_dool
prop_R1_dool = foldl1 (&&.) . take samples . prop_R1_dools
prop_R1_dools (Case _ dy dz ref) = always (temp <=: constant 100)
  where
    (temp, _) = let ?h = h in run dy dz ref


-- | (R2) after 15 seconds of operation, the system must be in stable regime,
-- which means that the temperature of the water in the tank must always
-- stay between 91 and 97 degrees.
--
-- This property has been adapted to handle changing reference temperature.
-- 15 seconds after a reference temperature change the temperature must
-- be within -3 and +3 degrees of reference (unless a new
-- reference before that).
prop_R2_bool = isTrue . prop_R2_dool
prop_R2_dool = foldl1 (&&.) . take samples . prop_R2_dools
prop_R2_dools (Case _ dy dz ref) = nts settling =>: stable
  where
    settling = holds (change ref ||: start) n
    stable = ref - constant 3 <=: temp &&: temp <=: ref + constant 3
    n = t2s 15
    (temp, _) = let ?h = h in run dy dz ref


-- | (R3) during this stable regime, the burner is never continuously ON for
-- more than two seconds.
prop_R3_bool = isTrue . prop_R3_dool
prop_R3_dool = foldl1 (&&.) . take samples . prop_R3_dools
prop_R3_dools (Case _ dy dz ref) =
    nts is_settling  =>: (nts is_on ||: on_timer)
  where
    turned_on = fromBools (burner `isEvent` val ON)
    turned_off = fromBools (burner `isEvent` val OFF)
    is_on = turned_on `intervals` turned_off

    settling_time = t2s 15
    is_settling = holds (change ref ||: start) settling_time

    resets = end is_settling &&: is_on ||: nts is_settling &&: turned_on
    max_on = t2s 2
    on_timer = holds resets max_on

    (temp, burner) = let ?h = h in run dy dz ref

prop_alternating_on_off_bool = isTrue . prop_R3_dool
prop_alternating_on_off_dool =
  foldl1 (&&.) . take samples . prop_alternating_on_off_dools
prop_alternating_on_off_dools (Case _ dy dz ref) =
    (ons `intervalw` offs) &&: (offs `intervalw` ons)
  where
    ons = fromBools (burner `isEvent` val ON)
    offs = fromBools (burner `isEvent` val OFF)
    (_, burner) = let ?h = h in run dy dz ref

main = defaultMain $ testGroup "test water heater" [props]

props = testGroup "properties"
  [ testProperty "never 100 degrees" prop_R1_bool
  , testProperty "stable 15s after reference change" prop_R2_bool
  , testProperty "never on more than 2s at a time when stable" prop_R3_bool
  ]
