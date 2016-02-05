module Optimize where

import Data.List( nub, maximumBy )
import Data.Ord( comparing )

--------------------------------------------------------------------------------

opti :: (Double -> Double) -> (Double,Double) -> Double
opti f (x0,x1) = search x0 (f x0) x1 (f x1)
 where
  e = 0.0000001

  search x0 y0 x1 y1
    | x0 + e >= x1 =
      fst $ maximumBy (comparing snd) ([(x0,y0),(x1,y1)] ++ xyas ++ xybs)

    | ma >= mb  = search x0 y0 xb yb
    | otherwise = search xa ya x1 y1
   where
    k    = 3
    t    = 0.33 / fromIntegral k
    xyas = (x0,y0) : take k [ (x,f x) | d <- [t,t+t..], let x = (1-d)*x0 + d*x1 ]
    xybs = (x1,y1) : take k [ (x,f x) | d <- [t,t+t..], let x = d*x0 + (1-d)*x1 ]

    ma = minimum [ y | (_,y) <- xyas ]
    mb = minimum [ y | (_,y) <- xybs ]

    (xa,ya) = last xyas
    (xb,yb) = last xybs

optiVec :: ([Double] -> Double) -> ([Double],[Double]) -> [Double]
optiVec f (v0,v1)
  | n /= length v1 = error "optiVec: vector lengths do not match"
  | otherwise      = improving v
 where
  n = length v0
  v = zipWith avg v0 v1

  x `avg` y = (x+y) / 2

  improving v
    | f v' > f v = improving v'
    | otherwise  = v
   where
    v' = improve v

  improve v = fst $ maximumBy (comparing snd) $ [ (v, f v) | v <- vs ]
   where
    vs = nub $ concat
         [ [ v =!! (i, opti (\x -> f (v =!! (i,x))) (v0!!i,v !!i))
           , v =!! (i, opti (\x -> f (v =!! (i,x))) (v !!i,v1!!i))
           ]
         | i <- [0..n-1]
         ]

  (=!!) :: [a] -> (Int,a) -> [a]
  xs =!! (i,x) = take i xs ++ [x] ++ drop (i+1) xs

fix :: Eq a => (a -> a) -> a -> a
fix f x = let y = f x in if x == y then x else fix f y

fixList :: Eq a => (a -> a) -> a -> [a]
fixList f x = let y = f x in x : if x == y then [] else fixList f y

--------------------------------------------------------------------------------

