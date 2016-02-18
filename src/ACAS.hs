module ACAS where

import Zelus

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
