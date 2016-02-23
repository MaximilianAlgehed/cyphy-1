module ACAS where

import Zelus

data Mode = Active | Inactive deriving (Eq, Show)

tscale :: S Double -> S Double
tscale t = 0.001 * t

-- | Aircraft collision avoidance system controller.
acas :: S Bool   -- ^ Avoidance trigger.
     -> S Double -- ^ Resulting course correction.
acas trigger = correction
  where
    omega, left :: S Double
    omega = tscale 1
    left  = -pi / 2

    zdot, z :: S Double
    zdot = omega
    z    = integ (zdot `in1t` 0 `reset` (0 `when` (mode ==? val Inactive)))

    mode :: S Mode
    mode = automaton
      [ Inactive >-- trigger    --> Active
      , Active   >-- (z >=? pi) --> Inactive
      ]

    correction :: S Double
    correction = enterCorrect + correct + leaveCorrect
      where
        enterCorrect = mode `took` (Inactive --> Active) ? (left, 0)
        correct      = mode ==? val Active ? (omega, 0)
        leaveCorrect = mode `took` (Active --> Inactive) ? (left, 0)

-- | Plant modelling the aircraft.
aircraft :: (Double, Double, Double) -- ^ Initial position and heading.
         -> S Double                 -- ^ Disturbance along x axis.
         -> S Double                 -- ^ Disturbance along y axis.
         -> S Double                 -- ^ Course correction.
         -> (S Double, S Double)     -- ^ Resulting position.
aircraft (initx, inity, inittheta) dx dy thetadot = (x, y)
  where
    v         = tscale 100
    theta     = integ $ thetadot `in1t` val inittheta
    xdot      = v * cos theta + dx
    ydot      = v * sin theta + dy
    x         = integ $ xdot `in1t` val initx
    y         = integ $ ydot `in1t` val inity
