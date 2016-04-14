{-# LANGUAGE ImplicitParams #-}

module Overshoot where

import Zelus
import Plot

plotos ref d = plot file n [(lref, gref), (lnod, gnod), (ld, gd)]
  where
    n = 10000
    lref = "reference"
    gref = graph (val ref)
    lnod = "no disturbance"
    gnod = graph (run ref 0)
    ld = "with disturbance"
    gd =   graph (run ref d)
    file = "plot.png"

run :: Double -> Double -> S Double
run ref disturbance =
  let
    u = controller (pre out) (val ref)
    out = plant u (val disturbance)
  in out
  where
    ?h = 0.01

controller :: (?h :: Double) => S Double -> S Double -> S Double
controller y ref = pid
  where
    kp = 2
    ki = 0.2
    kd = 0.0

    err = ref - y
    i_err = integ $ err `in1t` 0
    d_err = deriv err

    pid = kp*err + ki*i_err + kd*d_err

plant :: (?h :: Double) => S Double -> S Double -> S Double
plant u d = y
  where
    t = 10
    k = 9
    a = 1/t
    b = k/t
    y = integ $ (a * pre y + b * pre u + b * d) `in1t` 0
