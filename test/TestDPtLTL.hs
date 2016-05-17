module TestDPtLTL where

import Test.QuickCheck

import Zelus ()
import Dool

import

data Case = Case { ds :: [Dool] } deriving (Show)

instance Arbitrary Case where
  arbitrary =
    do xs <- arbitrary
       return (Case (take 100 (xs >=: 0)))



prop_1 :: Case -> Bool
prop_1 c = once (ds c) == trues `sinces` (ds c)
