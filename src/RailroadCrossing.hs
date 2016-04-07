{-# LANGUAGE ImplicitParams #-}

-- | Based on the railroad-gate controller automaton example from
-- HyTech: a model checker for hybrid systems
-- http://dslab.konkuk.ac.kr/Class/2012/12SIonSE/Key%20Papers/HyTech%20-%20STTT.pdf
-- Page 118. Initial position is changed from 5000 to 1100 in this
-- implementation.
-- Fail condition is when gate is not closed and train distance from crossing is
-- less than or equal to 10.

module RailroadCrossing where

import Zelus

data GateState  = Open | Closing | Closed deriving (Eq, Show)
data TrainState = Far | Approaching | Near deriving (Eq, Show)

run :: Double -- ^ -55 <= ddApproach <= -45
    -> Double -- ^ -50 <= ddNear <= -35
    -> Double -- ^ 0.9 <= closingSpeed <= 1.1
    -> Double -- ^ 169.3 <= safetyDistance <= 288.2
              -- All valid choices of ddApproach, ddNear and closingSpeed will
              -- work with a safety distance greater than 288.2 and fail with
              -- distance less than 169.3.
    -> S (Double, GateState)
run ddApproach ddNear closingSpeed safetyDistance =
    let
      d = train ddApproach ddNear safetyDistance
      gState = gate d closingSpeed safetyDistance
    in zip d gState
  where
    ?h = 0.01

train :: (?h :: Double) => Double -> Double -> Double -> S Double
train ddApproach ddNear safetyDistance = d
  where
    initPos = val 1100
    a = (trainState ==? val Near &&? pre dd <? val ddNear) ? (2.5, 0)
    dd = integ (a `in1t` val ddApproach)
    d = integ (dd `in1t` initPos)
    safe = -100
    trainPresent = safe <=? d &&? d <=? val safetyDistance
    trainLeaves = d <? safe
    trainState = automaton
      [ Approaching >-- trainPresent --> Near
      , Near >-- trainLeaves --> Far
      ]

gate :: (?h :: Double) => S Double -> Double -> Double -> S GateState
gate d closingSpeed safetyDistance = gateState
  where
    dx = gateState ==? val Closing ? (val closingSpeed, val 0)
    safe = -100
    trainPresent = safe <=? d &&? d <=? val safetyDistance
    trainLeaves = d <? safe
    x = integ (dx `in1t` 0) -- Time counter
    gateState = automaton
      [ Open >-- trainPresent --> Closing
      , Closing >-- (x >=? 5) --> Closed
      , Closed >-- trainLeaves --> Open
      ]
