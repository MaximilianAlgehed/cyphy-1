module Main where

import Zelus
import Optimize
import Plot

--------------------------------------------------------------------------------
-- heater + controller

type Level = Double -- pump level
type Temp  = Double -- temperature

-- computing the weighted average

weigh :: Fractional a => [(a,a)] -> a
weigh axs = sum [ a*x | (a,x) <- axs ] / sum [ a | (a,_) <- axs ]

-- the plant

plant :: S Level -> S Temp
plant pump = roomTemp
 where
  startTemp    = outsideTemp
  boilerTemp   = 90
  heaterCoeff  = 0.1
  outsideTemp  = (-5)
  outsideCoeff = 0.05
 
  -- the heater temperature is influenced by how much hot water is pumped into it
  -- and the room temperature
  heaterTemp = startTemp |-> weigh [ (1-pump,      heaterTemp)
                                   , (pump,        boilerTemp)
                                   , (heaterCoeff, roomTemp)
                                   ]

  -- the room temperature is influenced by the heater temperature and the outside
  -- temperature
  roomTemp   = startTemp |-> weigh [ (1,            roomTemp)
                                   , (heaterCoeff,  heaterTemp)
                                   , (outsideCoeff, outsideTemp)
                                   ]

-- controller

type Control = (Double, Double, Double)

controller :: Control -> S Temp -> S Temp -> S Level
controller (k_p,k_i,k_d) goalTemp roomTemp =
  (pump' >=? 0) ? ((1 >=? pump') ? (pump', 1), 0)
 where
  err   = goalTemp - roomTemp
  pump' = val k_p * err
        + val k_i * integral err
        + val k_d * deriv err

cgood :: Control
cgood = (3.997591176733649e-3,8.194771741046325e-5,5.618398605936785e-3)

--------------------------------------------------------------------------------
-- show a given controller

display :: String -> (S Temp -> S Level) -> IO ()
display name controller =
  plot name 140
  [ ("room", roomTemp)
  , ("pump", fmap (50*) pump)
  ]
 where
  roomTemp = plant pump
  pump     = controller roomTemp
 
--------------------------------------------------------------------------------
-- search

analyze :: Control -> (Integer, Double)
analyze c = stable 0 0 0 roomTemp
 where
  roomTemp = plant pump
  pump     = controller c 20 roomTemp

  stable m n k (t:ts)
    | k >= 100              = (n, m)
    | n >= 10000            = (n+1, m)
    | abs (t - 20) <= 0.01  = stable (m `max` t) n (k+1) ts
    | otherwise             = stable (m `max` t) (n+k+1) 0 ts

fit :: Control -> Double
fit c = fromInteger n / 30 + m
 where
  (n,m) = analyze c

cbest = (a,b,c)
 where
  [a,b,c] = optiVec (\[a,b,c] -> -fit (a,b,c)) (v0,v1)

  v0 = [0,0,0]
  v1 = [e,e,e]
  e  = 0.01

--------------------------------------------------------------------------------
-- main

main :: IO ()
main =
  do putStrLn "-- a good controller --"
     print cgood
     print (analyze cgood)
     print (fit cgood)
     display "good" (controller cgood goalTemp)
  
     putStrLn "-- the best (?) controller --"
     print cbest
     print (analyze cbest)
     print (fit cbest)
     display "best" (controller cbest goalTemp)
 where
  goalTemp = replicate 60 20 ++ repeat 15

--------------------------------------------------------------------------------

