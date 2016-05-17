module TestRC_NQC where

import NotQuickCheck
import Zelus
import RailroadCrossing

safetyDistance = 250
steps = 3500
ddStep = 0.5
dxStep = 0.05

valid (S ddA ddN cS) =
  -45 >= ddA && ddA >= -55 && -35 >= ddN && ddN >= -50 && 0.9 <= cS && cS <= 1.1

data Speeds = S Double Double Double deriving (Eq, Show)

instance Arbitrary Speeds where
  arbitrary =
    do
      ddApproach <- choose (-45, -55)
      ddNear <- choose (-35, -50)
      closingSpeed <- choose (0.9, 1.1)
      return (S ddApproach ddNear closingSpeed)

  shrink (S ddA ddN cS) =
    let
      ddA' = ddA + ddStep
      ddN' = ddN + ddStep
      cS' = cS + dxStep
    in reverse $ filter valid
      [ S ddA ddN cS'
      , S ddA ddN' cS
      , S ddA ddN' cS'
      , S ddA' ddN cS
      , S ddA' ddN cS'
      , S ddA' ddN' cS
      , S ddA' ddN' cS'
      ]

prop_fail :: Speeds -> Double
prop_fail (S ddApproach ddNear closingSpeed) =
  minimum $  -- instead of `and`ing the list together; find the worst value
    take steps $ map failed result
  where
    result = run ddApproach ddNear closingSpeed safetyDistance
    failed (d, g) = if abs d <= 10 && g /= Closed then -(d + 10) else 0
                    --if abs d <= 10 && g /= Closed -- fail condition
                    --then abs d - 10 -- badness value, distance from the crossing
                    --else 0 -- non-negative is true
