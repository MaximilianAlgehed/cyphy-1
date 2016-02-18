module ACAS where

import Zelus

-- | Aircraft collision avoidance system controller.
acas :: S Double -- ^ Distance to offending aircraft.
     -> S Double -- ^ Resulting course correction.
acas = inactive

-- | Inactive mode, no course correction.
inactive :: S Double -- ^ Distance to offending aircraft.
         -> S Double -- ^ Resulting course correction.
inactive (d:ds) =
    if d < r
    then right : active ds 0
    else omega : inactive ds
  where
    right = pi/2
    omega = 0.0
    r = 5

-- | Active mode, yes course correction.
active :: S Double -- ^ Distance to offending aircraft.
       -> Double
       -> S Double -- ^ Resulting course correction.
active (d:ds) acc =
    if acc > pi
    then left : inactive ds
    else omega : active ds (acc+omega)
  where
    left  = -pi/2
    omega = 0.01
    z     = integ $ (val omega) `in1t` 0

-- | Plant modelling the aircraft.
aircraft :: S Double                   -- ^ Course correction.
         -> S Double                   -- ^ Disturbance along x axis.
         -> S Double                   -- ^ Disturbance along y axis.
         -> S (Double, Double, Double) -- ^ Resulting position and heading.
aircraft thetadot dx dy = zip3 x y theta
  where
    inittheta = val (pi/4)
    initx     = val 10
    inity     = val 10
    initv     = val 4
    theta     = integ $ thetadot `in1t` inittheta
    xdot      = initv * cos theta + dx
    ydot      = initv * sin theta + dy
    x         = integ $ xdot `in1t` initx
    y         = integ $ ydot `in1t` inity


{-  where
    rght       = val (pi/2)
    lft        = -rght
    r          = val 20
    omegadot1  = 0.0
    omegadot2  = 0.1
    zdot1      = 0.0
    zdot2      = 1.0
    mode1      = val (omegadot1, zdot1)
    mode2      = val (omegadot2, zdot2)
    triggered  = d <? r
    state      = triggered ? (mode2, mode1)
    correction = undefined
    hm         = undefined
-}
