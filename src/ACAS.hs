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
       -> Double   -- ^ Arc integrator.
       -> S Double -- ^ Resulting course correction.
active (d:ds) acc =
    if acc > pi
    then right : inactive ds
    else -omega : active ds (acc+omega)
  where
    right = pi/2
    omega = 0.01

-- | Plant modelling the aircraft.
aircraft :: S Double                   -- ^ Course correction.
         -> S Double                   -- ^ Disturbance along x axis.
         -> S Double                   -- ^ Disturbance along y axis.
         -> S (Double, Double, Double) -- ^ Resulting position and heading.
aircraft thetadot dx dy = zip3 x y theta
  where
    inittheta = pi/4
    initx     = 10
    inity     = 10
    v         = val 4
    theta     = integ $ thetadot `in1t` inittheta
    xdot      = v * cos theta + dx
    ydot      = v * sin theta + dy
    x         = integ $ xdot `in1t` initx
    y         = integ $ ydot `in1t` inity
