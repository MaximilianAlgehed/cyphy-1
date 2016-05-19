{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TestWaterHeater where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Zelus (val,  isEvent)
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
    let dys = (dy `shrinkTo` 1) `limitTo` 3
        dzs = (dz `shrinkTo` 1) `limitTo` 3
        raws = shrinkList (\x -> [x]) raw -- no shrinking dt or r currently
    in [Case raw' dy dz (mkRef raw') | raw' <- raws, not (null raw')]
         ++ [Case raw dy' dz ref | dy' <- dys]
           ++ [Case raw dy dz' ref | dz' <- dzs]

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
h = 0.1

tmax :: Double
tmax = 100

samples :: Int
samples = round (tmax/h)


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
    settling = holdw (change ref) n
    stable = ref - constant 3 <=: temp &&: temp <=: ref + constant 3
    n = round (15/h)
    (temp, _) = let ?h = h in run dy dz ref


-- | (R3) during this stable regime, the burner is never continuously ON for
-- more than two seconds.
prop_R3_bool = isTrue . prop_R3_dool
prop_R3_dool = foldl1 (&&.) . take samples . prop_R3_dools
prop_R3_dools (Case _ dy dz ref) = nts settling =>: stable
  where
    limit = round (2/h)
    time = burner `isEvent` val ON
    settling = holdw (change ref) n
    stable = ref - constant 3 <=: temp &&: temp <=: ref + constant 3
    n = round (10/h)
    (temp, burner) = let ?h = h in run dy dz ref


main = defaultMain $ testGroup "test water heater" [props]

props = testGroup "properties"
  [ testProperty "never 100 degrees" prop_R1_bool
  , testProperty "stable 15s after reference change" prop_R2_bool
  , testProperty "never on more than 2s at a time when stable" prop_R3_bool
  ]
