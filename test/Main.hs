module Main where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified TestWaterHeater as TWH

main :: IO ()
main = defaultMain $ testGroup "All the tests!"
  [ twh
  ]

-----------------------------------------------
----- Water heater test groups
---

twhMain = defaultMain twh

twh = testGroup "WaterHeater tests"
  [ twhArbitraryTests
  , twhPropertyTests
  ]

twhArbitraryTests = testGroup "Arbitrary instance tests"
  [ testProperty "Ref shrink strictly smaller"
      (TWH.prop_shrink_smaller :: TWH.Ref -> Bool)
  , testProperty "Ref shrink nonempty"
      (TWH.prop_shrink_nonempty :: TWH.Ref -> Bool)
  , testProperty "Raw nonempty"
      (TWH.prop_arb_raw_nonempty :: TWH.Ref -> Bool)
  , testProperty "Stream nonempty"
      (TWH.prop_arb_stream_nonempty :: TWH.Ref -> Bool)
  , testProperty "Minimum dts is respected"
      (TWH.prop_arb_min_dts :: TWH.Ref -> Bool)
  , testProperty "Maximum dts is respected"
      (TWH.prop_arb_max_dts :: TWH.Ref -> Bool)
  , testProperty "Minimum drs is respected"
      (TWH.prop_arb_min_drs :: TWH.Ref -> Bool)
  , testProperty "Maximum drs is respected"
      (TWH.prop_arb_max_drs :: TWH.Ref -> Bool)
  , testProperty "Minimum absolute r is respected"
      (TWH.prop_arb_min_absr :: TWH.Ref -> Bool)
  , testProperty "Maximum absolute r is respected"
      (TWH.prop_arb_max_absr :: TWH.Ref -> Bool)
  ]

twhPropertyTests = testGroup "Property tests"
  [ testProperty "Temperature never reaches 100 degrees"
      (TWH.prop_R1 :: TWH.Ref -> Bool)
  , testProperty "Stable regime within 15s after reference change"
      (TWH.prop_R2 :: TWH.Ref -> Bool)
  , testProperty "Burner not continously ON for more than 2s while stable"
      (TWH.prop_R2 :: TWH.Ref -> Bool)
  ]
