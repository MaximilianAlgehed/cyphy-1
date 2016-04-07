{-# LANGUAGE ImplicitParams #-}

-- | Based on Cruise control system from
-- http://www.cds.caltech.edu/~murray/amwiki/index.php/Cruise_control

module CruiseControl where

import Zelus

data Gear = One | Two | Three | Four | Five deriving (Eq, Show)

run :: Double   -- ^ Initial speed, m/s
    -> S Double -- ^ Cruise control speed setting, m/s
    -> S Double -- ^ Road slope (disturbance), rad
    -> S Double -- ^ Resulting speed,m/s
run v0 ref road_slope =
    let
      (u_a, u_b) = controller (pre v_stable) ref
      acc = vehicle (pre v_stable) u_a u_b road_slope
      v = integ (acc `in1t` val v0)
      v_stable = (v <? 0.005) ? (0,v)
    in v_stable
  where
    ?h = 0.01

vehicle :: S Double -- ^ Velocity, m/s
        -> S Double -- ^ Accelerator ratio, [0, 1]
        -> S Double -- ^ Decelerator ratio, [0, 1]
        -> S Double -- ^ Road slope, rad
        -> S Double -- ^ Resulting acceleration, m/s^2
vehicle v u_a u_b road_slope = acc
  where
    t_m = 400     -- engine torque constant, Nm
    omega_m = 400 -- peak torque rate, rad/sec
    beta = 0.4    -- torque coefficient
    cr = 0.03     -- coefficient of rolling friction
    rho = 1.29    -- density of air, kg/m^3
    cd = 0.28     -- drag coefficient
    a = 2.8       -- car area, m^2
    g = 9.81      -- gravitational constant
    m = 1700      -- vehicle mass, kg
    t_m_b = 2800  -- maximum brake torque, Nm

    wheel_radius = 0.381 -- m

    gear_ratio One = 13.52
    gear_ratio Two = 7.6
    gear_ratio Three = 5.08
    gear_ratio Four = 3.8
    gear_ratio Five = 3.08

    -- engine speed, rad/s
    omega = (v * map gear_ratio gear) / (wheel_radius * pi) * 60 / 9.55

    t_e = u_a * t_m * (1 - beta*(omega/omega_m - 1)^2) -- engine tourque
    t_b = u_b * t_m_b                                  -- brake tourque

    -- friction
    f_fric = ((t_e * map gear_ratio gear) - (t_b * signum v)) / wheel_radius
    f_g = m * g * sin road_slope   -- gravitation
    f_r = m * g * cr * signum v    -- rolling resistance
    f_a = 0.5 * rho * cd * a * v^2 -- air drag

    acc = (f_fric - f_g - f_r - f_a) / m

    up_shift = 3000 / 9.55   -- rad/s
    down_shift = 1000 / 9.55 -- rad/s

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

controller :: (?h :: Double) => S Double -> S Double -> (S Double, S Double)
controller v ref = (u_a, u_b)
  where
    kp = 0.8
    ki = 0.04
    kd = 0.0

    err = ref - v
    i_err = integ $ ((abs (pre pid) >? 1) ? (0, err)) `in1t` 0
    d_err = deriv err

    pid = kp*err + ki*i_err + kd*d_err

    -- accelerate when positive and break when negative
    u_a = pid >? 0 ? (mn pid 1, 0)
    u_b = pid <? 0 ? (mn (-pid) 1, 0)
