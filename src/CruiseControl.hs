module CruiseControl where

import Zelus

--------------------------------------------------------------------------------
----- Vehicle
---

data Gear = One | Two | Three | Four | Five deriving (Eq, Show)

vehicle :: S Double -- ^ Velocity
        -> S Double -- ^ Accelerator ratio
        -> S Double -- ^ Decelerator ratio
        -> S Gear   -- ^ Gear
        -> S Double -- ^ Slope
        -> S Double -- ^ Resulting acceleration
vehicle v u_a u_b gear theta = acc
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

    -- omega = v.*gear_ratio(gear)./(wheel_radius*pi)*60/9.55; % rad/s
    omega = (v * map gear_ratio gear) / (wheel_radius * pi) * 60 / 9.55 -- rad/s

    -- T_e = u*Tm.*(1-beta.*(omega./wm-1).^2);
    t_e = u_a * tm * (1 - beta*(omega/wm - 1)^2)
    -- T_b = u_b*Tm_b;
    t_b = u_b * tm_b

    -- F_fric = (T_e*gear_ratio(gear) - T_b*sign(v))./wheel_radius;
    f_fric = ((t_e * map gear_ratio gear) - (t_b * signum v)) / wheel_radius
    -- F_g = m.*g.*sin(theta);
    f_g = m * g * sin theta
    -- F_r = m.*g.*Cr.*sign(v);
    f_r = m * g * cr * signum v
    -- F_a = 0.5.*rho.*Cd.*A.*v.^2;
    f_a = 0.5 * rho * cd * a * v^2

    -- acc = (F_fric - F_g - F_r - F_a)./m;
    acc = (f_fric - f_g - f_r - f_a) / m

--------------------------------------------------------------------------------
----- Simulation
---
