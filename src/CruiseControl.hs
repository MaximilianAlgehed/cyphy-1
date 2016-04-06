{-# LANGUAGE ImplicitParams #-}

module CruiseControl where

import Zelus

--------------------------------------------------------------------------------
----- Vehicle
---

data Gear = One | Two | Three | Four | Five deriving (Eq, Show)

vehicle :: S Double -- ^ Velocity
        -> S Double -- ^ Accelerator ratio
        -> S Double -- ^ Decelerator ratio
        -> S Double -- ^ Slope
        -> S Double -- ^ Resulting acceleration, m/s^2
vehicle v u_a u_b theta = acc
  where
    tm = 400    -- engine torque constant, Nm
    wm = 400    -- peak torque rate, rad/sec
    beta = 0.4  -- torque coefficient
    cr = 0.03   -- coefficient of rolling friction
    rho = 1.29  -- density of air, kg/m^3
    cd = 0.28   -- drag coefficient
    a = 2.8     -- car area, m^2
    g = 9.81    -- gravitational constant
    m = 1700    -- vehicle mass, kg
    tm_b = 2800 -- maximum brake torque, Nm

    wheel_radius = 0.381 -- m

    gear_ratio One = 13.52
    gear_ratio Two = 7.6
    gear_ratio Three = 5.08
    gear_ratio Four = 3.8
    gear_ratio Five = 3.08

    omega = (v * map gear_ratio gear) / (wheel_radius * pi) * 60 / 9.55 -- rad/s

    t_e = u_a * tm * (1 - beta*(omega/wm - 1)^2)
    t_b = u_b * tm_b

    f_fric = ((t_e * map gear_ratio gear) - (t_b * signum v)) / wheel_radius
    f_g = m * g * sin theta
    f_r = m * g * cr * signum v
    f_a = 0.5 * rho * cd * a * v^2

    acc = (f_fric - f_g - f_r - f_a) / m

    up_shift = 3000 / 9.55
    down_shift = 1000 / 9.55

    gear = automaton
      [ One >-- omega >? up_shift --> Two
      , Two >-- omega <? down_shift --> One
      , Two >-- omega >? up_shift --> Three
      , Three >-- omega <? down_shift --> Two
      , Three >-- omega >? up_shift --> Four
      , Four >-- omega <? down_shift --> Three
      , Four >-- omega >? up_shift --> Five
      , Five >-- omega <? down_shift --> Four
      ]

--------------------------------------------------------------------------------
----- Simulation
---

controller :: (?h :: Double) => S Double -> S Double -> (S Double, S Double)
controller v ref = (u_a, u_b)
  where
    kp = 0.06
    ki = 0.002
    kd = 0.0

    err = ref - v
    i_err = integ (err `in1t` 0)
    d_err = deriv err

    pid = kp*err + ki*i_err + kd*d_err

    u_a = pid >? 0 ? (mn pid 1, 0)
    u_b = pid <? 0 ? (mn (-pid) 1, 0)

run :: Double -> S Double -> S Double -> S Double
run v0 ref slope =
    let
      (u_a, u_b) = controller (pre v) ref
      acc = vehicle (pre v) u_a u_b slope
      v = integ (acc `in1t` val v0)
    in v
  where
    ?h = 0.01
