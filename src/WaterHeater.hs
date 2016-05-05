{-# LANGUAGE ImplicitParams #-}

-- | Based on An Introduction to Hybrid Automata by Jean-Francois Raskin
-- http://www.cmi.ac.in/~madhavan/courses/acts2010/Raskin_Intro_Hybrid_Automata.pdf

module WaterHeater where

import Control.Applicative ((<|>))

import Zelus
import CyphyUtils

data TankState = T1 | T2 | T3 | T4 deriving (Eq, Show)

data BurnerEvent = ON | OFF deriving (Eq, Show)

data BurnerState = B1 | B2 | B3 | B4 deriving (Eq, Show)

data ThermoEvent = UP95 | DW93 deriving (Eq, Show)

run :: S Double -> (S Double, E ThermoEvent, E BurnerEvent)
run ref_temp =
    let
      (temperature, _) = unzip (tank burner_events)
      burner_events = burner thermo_events
      thermo_events = thermo ref_temp temperature
    in (temperature, thermo_events, burner_events)
  where
    ?h = 0.01

-- tank :: (?h :: Double) => E BurnerEvent -> S Double
tank burner_event = zip temperature state
  where
    max_temp = 100
    min_temp = 20
    init_temp = 99 --min_temp
    k = 0.075
    heat = 150

    dtemp =  map k1 state * pre temperature + map m state
    temperature = integ (dtemp `in1t` init_temp)

    m T1 = k * heat
    m _  = 0

    k1 T1 = -k
    k1 T3 = -k
    k1 _  = 0

    state = automaton
      [ T1 >-- temperature >=? max_temp --> T2
      , T1 >-- burner_event `isEvent` val OFF --> T3
      , T2 >-- burner_event `isEvent` val OFF --> T3
      , T3 >-- burner_event `isEvent` val ON --> T1
      , T3 >-- temperature <=? min_temp --> T4
      , T4 >-- burner_event `isEvent` val ON --> T1
      ]

burner :: (?h :: Double) => E ThermoEvent -> E BurnerEvent
burner thermo_event = on <|> off
  where
    delay = 0.1

    dy B1 = 0
    dy B2 = 1
    dy B3 = 0
    dy B4 = 1

    on = val ON `when` (state `took` (B1 --> B2))
    off = val OFF `when` (state `took` (B3 --> B4))

    y = integ (map dy state `in1t` 0 `reset` (0 `whenEvent` (on <|> off)))

    state = automaton
      [ B1 >-- thermo_event `isEvent` val DW93 --> B2
      , B2 >-- y >=? val delay --> B3
      , B3 >-- thermo_event `isEvent` val UP95 --> B4
      , B4 >-- y >=? val delay --> B1
      ]

thermo :: (?h :: Double) => S Double -> S Double -> E ThermoEvent
thermo ref_temp temperature = (up <|> down)
  where
    max_temp = ref_temp + 1
    min_temp = ref_temp - 1

    frequency = 0.1

    samples = val False |-> (z >=? frequency)

    up = val UP95 `when` (temperature >=? max_temp &&? samples)
    down = val DW93 `when` (temperature <=? min_temp &&? samples)

    dz = 1

    z = integ (dz `in1t` 0 `reset` (0 `when` samples))

-----------------------------------------------
----- Examples
---

tex = let ?h = 0.01 in thermo tempdown

tempdown = [100 - t*0.1 | t <- [0..200]]
tempup = [80 + t*0.1 | t <- [0..200]]
