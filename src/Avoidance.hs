module Main where

import Zelus
import Plot
import Test.QuickCheck

data Mode = Before | Dance | After
 deriving ( Eq, Ord, Show )

airplane :: Double -> (Double,Double) -> S Bool -> (S Double, S Double) 
airplane angle0 (x0,y0) avoid = (x,y)
 where
  -- constants
  v     = tscale 100
  omega = tscale 1 -- this affects the radius of the dance

  -- mode-independent
  x      = integ ((v * cos angle) `in1t` val x0)
  y      = integ ((v * sin angle) `in1t` val y0)
  z      = integ (dz              `in1t` 0)
  angle  = integ (dangle          `in1t` val angle0)
  
  -- mode-dependent
  dangle = (mode ==? val Dance ? (omega, 0))
         + ((  mode `took` (Before --> Dance)
           ||? mode `took` (Dance  --> After)
            ) ? (-(pi / 2), 0))
  dz     = (mode ==? val Dance ? (tscale 1, 0))
  
  mode = automaton
         [ Before >-- avoid      --> Dance
         , Dance  >-- (z >=? pi) --> After
         ]

tscale :: S Double -> S Double
tscale t = 0.001 * t

data Plane = Plane Double (Double,Double)
 deriving ( Eq, Ord, Show )

instance Arbitrary Plane where
  arbitrary =
    do angle <- choose (0,2*pi)
       x0    <- choose (0,1000)
       y0    <- choose (0,1000)
       return (Plane angle (x0,y0))

  shrink (Plane angle (x0,y0)) = []
  {-
    [ Plane angle' (x0,y0) | angle' <- shrink angle ] ++
    [ Plane angle (x0',y0) | x0'    <- shrink x0 ] ++
    [ Plane angle (x0,y0') | y0'    <- shrink y0 ]
  -}

main = quickCheck prop_Avoidance

prop_Avoidance (Plane anglea0 (xa0,ya0), Plane angleb0 (xb0,yb0)) =
  whenFail (plot "airplane" n
            [ ("A", (xa,ya))
            , ("B", (xb,yb))
            , ("C", (xc,yc))
            ]) $
  for 100 (nt avoid) ==>
    for n (nt collision)
 where
  n = 10000
 
  (xa,ya) = airplane anglea0 (xa0,ya0) avoid
  (xb,yb) = airplane angleb0 (xb0,yb0) avoid

  distance = sqrt ((xa-xb)^2 + (ya-yb)^2)

  avoid = val False |-> (distance <=? 100)
  collision = distance <=? 20

  xc = [ (xa+xb) / 2 | (c,(xa,xb)) <- take n collision `zip` (xa `zip` xb), c ]
  yc = [ (ya+yb) / 2 | (c,(ya,yb)) <- take n collision `zip` (ya `zip` yb), c ]

