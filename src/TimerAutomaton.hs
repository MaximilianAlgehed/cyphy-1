{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# ExistentialQuantification #-}

module TimerAutomaton where

import Zelus
import CyphyUtils


timer :: S Double
timer = undefined
  where
    state = undefined



automaton :: Eq a => [(a,S Bool,a)] -> S a
automaton ts@((s0,_,_):_) = s
 where
  s = val s0 |-> trans ts

  trans []             = s
  trans ((s1,t,s2):ts) = s ==? val s1 &&? t ? (val s2, trans ts)

timerAutomaton :: Eq a
               => [( a                 -- from state
                   , Double -> Double  -- reset value
                   , Double -> Bool    -- timer trigger
                   , S Bool            -- other conditions
                   , a)]               -- to state
               -> S a
timerAutomaton ts@((s0,_,_):_) = s
 where
  s = val s0 |-> trans ts

  trans []             = s
  trans ((s1,t,s2):ts) = s ==? val s1 &&? t ? (val s2, trans ts)


{-
class TransCondition a where
  transCondition :: a -> (Double -> Double, -- reset value function
                          Double -> Bool,   -- timer trigger
                          S Bool)           -- additional condition

instance TransCondition (Double -> Bool) where
  trans t = (id, t, val True)

instance TransCondition (S Bool) where
  trans b = (id, const True, b)

instance TransCondition (Double
-}
