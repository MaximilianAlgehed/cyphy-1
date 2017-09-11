module Main where

import Zelus
import Plot
import VBool as V
import Test.QuickCheck
import Numeric.GSL.Minimization

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
    do x0 <- choose (0,wx)
       y0 <- choose (0,wy)
       let angle = atan (((wy/2)-y0)/((wx/2)-x0))
       --angle <- choose (0,2*pi)
       return (Plane angle (x0,y0))
   where
    wx = 700
    wy = 700  

  shrink (Plane angle (x0,y0)) =
    [ Plane angle' (x0,y0) | angle' <- shrink angle ] ++
    [ Plane angle (x0',y0) | x0'    <- shrink x0 ] ++
    [ Plane angle (x0,y0') | y0'    <- shrink y0 ]

data Planes = Planes Plane Plane
 deriving ( Eq, Ord, Show )

instance Arbitrary Planes where
  arbitrary =
    do p <- arbitrary
       q <- arbitrary
       return (Planes p q)
  
  shrink (Planes p q) =
    [ Planes p' q | p' <- shrink p ] ++
    [ Planes p q' | q' <- shrink q ]

main' = quickCheckWith stdArgs{ maxSuccess = 1000 } prop_Avoidance

prop_Avoidance (Fixed (Planes (Plane anglea0 (xa0,ya0)) (Plane angleb0 (xb0,yb0)))) =
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

  avoid = val False |-> (distance <=? 150)
  collision = distance <=? 5

  xc = [ (xa+xb) / 2 | (c,(xa,xb)) <- take n collision `zip` (xa `zip` xb), c ]
  yc = [ (ya+yb) / 2 | (c,(ya,yb)) <- take n collision `zip` (ya `zip` yb), c ]

instance Severity Planes where
  severity (Planes (Plane anglea0 (xa0,ya0)) (Plane angleb0 (xb0,yb0))) =
    for' 100 (map V.vbool $ nt avoid) V.=>.
      for' n (map neg collision)
   where
    n = 10000
   
    (xa,ya) = airplane anglea0 (xa0,ya0) avoid
    (xb,yb) = airplane angleb0 (xb0,yb0) avoid

    distance = sqrt ((xa-xb)^2 + (ya-yb)^2)

    avoid = val False |-> (distance <=? 100)
    collision = zipWith (<=.) distance 5

prop_Avoidance' :: Severe Planes -> Property
prop_Avoidance' s@(Severe (Planes (Plane anglea0 (xa0,ya0)) (Plane angleb0 (xb0,yb0))) _) =
  whenFail (plot "airplane" n
            [ ("A", (xa,ya))
            , ("B", (xb,yb))
            , ("C", (xc,yc))
            ]) $
  whenFail' (print (severity s)) $
    s
 where
  n = 10000
 
  (xa,ya) = airplane anglea0 (xa0,ya0) avoid
  (xb,yb) = airplane angleb0 (xb0,yb0) avoid

  distance = sqrt ((xa-xb)^2 + (ya-yb)^2)

  avoid = val False |-> (distance <=? 100)
  collision = distance <=? 5

  xc = [ (xa+xb) / 2 | (c,(xa,xb)) <- take n collision `zip` (xa `zip` xb), c ]
  yc = [ (ya+yb) / 2 | (c,(ya,yb)) <- take n collision `zip` (ya `zip` yb), c ]

for' n xs = foldr (V.&&+) V.true (take n xs)

badness :: [Double] -> Double
badness = trueness . severity . mkPlanes

opt = minimize NMSimplex2 0.001 10000 box badness cex --star

mkPlanes :: [Double] -> Planes
mkPlanes [a,x,y,a',x',y'] = Planes (Plane a (x,y)) (Plane a' (x',y'))

cex  = [1,2,3,4,5,6]
cex2 = [0,0,0,pi,700,700]
cex0 = [8.23920696000889e-2,134.6420247014044,330.0,-1.4950451121053046,343.77815301355736,431.9781125110596]
cex1 = [0.4085934941250741,23.214851195628604,208.51509404938193,-0.3622175113124993,15.12035300015051,476.8981739449264]

--star = [0,200,200,pi,500,500]
box  = [1,10,10,1,10,10]

main =
  do print opt
     putStrLn ("point0  =" ++ show cex)
     putStrLn ("point   =" ++ show (fst opt))
     putStrLn ("badness0=" ++ show (badness cex))
     putStrLn ("badness =" ++ show (badness (fst opt)))
     displayPlanes (mkPlanes (fst opt))

displayPlanes :: Planes -> IO ()
displayPlanes (Planes (Plane anglea0 (xa0,ya0)) (Plane angleb0 (xb0,yb0))) =
  plot "airplane" n
      [ ("A", (xa,ya))
      , ("B", (xb,yb))
      , ("C", (xc,yc))
      ]
 where
  n = 10000
 
  (xa,ya) = airplane anglea0 (xa0,ya0) avoid
  (xb,yb) = airplane angleb0 (xb0,yb0) avoid

  distance = sqrt ((xa-xb)^2 + (ya-yb)^2)

  avoid = val False |-> (distance <=? 100)
  collision = distance <=? 5

  xc = [ (xa+xb) / 2 | (c,(xa,xb)) <- take n collision `zip` (xa `zip` xb), c ]
  yc = [ (ya+yb) / 2 | (c,(ya,yb)) <- take n collision `zip` (ya `zip` yb), c ]

