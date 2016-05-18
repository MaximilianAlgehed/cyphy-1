-- | Testing the Past time LTL.
-- The article lists and proves 16 properties (p.5) for the PtLTL.
-- Corresponding tests in the suite below.

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TestDPtLTL where

import Test.Tasty
import Test.Tasty.QuickCheck hiding (once)

import Dool
import DPtLTL

instance Arbitrary Dool where
  arbitrary = elements [true, false]

instance Arbitrary [Dool] where
  arbitrary = listOf1 (frequency [(9, arbitrary), (1, arbitrary)])

prop_1 f = once f == trues `sinces` f
prop_2 f = always f == nts (once (nts f))
prop_3 f1 f2 = f1 `sincew` f2 == (always f1 ||: (f1 `sinces` f2))
prop_4 f = always f == f `sincew` falses
prop_5 f = once f == nts (always (nts f))
prop_6 f1 f2 = f1 `sinces` f2 == (once f2 &&: (f1 `sincew` f2))
prop_7 f = begin f == (f &&: nts (prev f))
prop_8 f = end f == (nts f &&: prev f)
prop_9 f1 f2 = f1 `intervals` f2 == (nts f2 &&: (prev (nts f2) `sinces` f1))
prop_10 f1 f2 = f1 `intervalw` f2 == (nts f2 &&: (prev (nts f2) `sincew` f1))
prop_11 f = end f == begin (nts f)
prop_12 f = begin f == end (nts f)
prop_13 f1 f2 = f1 `intervalw` f2 == (always (nts f2) ||: (f1 `intervals` f2))
prop_14 f1 f2 = f1 `intervals` f2 == (once f1 &&: (f1 `intervalw` f2))
prop_15 f = init (prev f) == ((f =>: nts (begin f)) &&: (nts f =>: end f))
prop_16 f1 f2 = f1 `sinces` f2 == (f2 ||: (prev f2 `intervals` nts f1))

prop_abs_unchanged_strong f n = n >= 0 ==> dabs f == dabs (holds f n)
prop_abs_unchanged_weak f n = n >= 0 ==> dabs f == dabs (holdw f n)

main = defaultMain $
  testGroup "test dptltl"
    [ baseprops
    , holdprops
    ]

baseprops = testGroup "base properties"
  [ testProperty "prop 1" prop_1
  , testProperty "prop 2" prop_2
  , testProperty "prop 3" prop_3
  , testProperty "prop 4" prop_4
  , testProperty "prop 5" prop_5
  , testProperty "prop 6" prop_6
  , testProperty "prop 7" prop_7
  , testProperty "prop 8" prop_8
  , testProperty "prop 9" prop_9
  , testProperty "prop 10" prop_10
  , testProperty "prop 11" prop_11
  , testProperty "prop 12" prop_12
  , testProperty "prop 13" prop_13
  , testProperty "prop 14" prop_14
  , testProperty "weakened prop 15" prop_15
  , testProperty "prop 16" prop_16
  ]

holdprops = testGroup "hold properties"
  [ testProperty "abs unchanged strong" prop_abs_unchanged_strong
  , testProperty "abs unchanged weak" prop_abs_unchanged_weak
  ]

-- utility

dabs f = f ||: nts f
