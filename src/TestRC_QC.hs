module TestRC_QC where

import Test.QuickCheck
import Zelus
import RailroadCrossing

safetyDistance = 250
steps = 3500
ddStep = 0.5
dxStep = 0.05

valid (S ddA ddN cS _) =
  -45 >= ddA && ddA >= -55 && -35 >= ddN && ddN >= -50 && 0.9 <= cS && cS <= 1.1

data Speeds = S Double Double Double Double deriving (Eq, Show)

instance Arbitrary Speeds where
  arbitrary =
    do
      ddApproach <- choose (-45, -55)
      ddNear <- choose (-35, -50)
      closingSpeed <- choose (0.9, 1.1)
      let composite = (ddApproach + ddNear) / closingSpeed
      return (S ddApproach ddNear closingSpeed composite)

  shrink (S ddA ddN cS _) =
    let
      ddA' = ddA + ddStep
      ddN' = ddN + ddStep
      cS' = cS + dxStep
      com = (ddA' + ddN') / cS'
    in reverse $ filter valid
      [ S ddA ddN cS' com
      , S ddA ddN' cS com
      , S ddA ddN' cS' com
      , S ddA' ddN cS com
      , S ddA' ddN cS' com
      , S ddA' ddN' cS com
      , S ddA' ddN' cS' com
      ]

prop_fail :: Speeds -> Bool
prop_fail (S ddApproach ddNear closingSpeed _) =
  not . or $ take steps $ map failed result
  where
    result = run ddApproach ddNear closingSpeed safetyDistance
    failed (d, g) = abs d <= 10 && g /= Closed
