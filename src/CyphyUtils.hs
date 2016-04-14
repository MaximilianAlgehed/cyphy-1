{-# LANGUAGE ImplicitParams #-}

module CyphyUtils where

import Zelus
import NotQuickCheck

-----------------------------------------------
----- Arbitrary helper generators
---

data Reference = R [(Double, Double)] (S Double)

-- y either start or stop time. if start: vilket värde fram till dess?
-- om slut: vilket värde mot oändligheten?
-- lösning: antar att första tidpunkten är 0
-- | Creates a reference stream from a list of time/reference value pairs.
refStream :: (RealFrac a, ?h :: a) => [(a, b)] -> S b
refStream xs = interp 0 xs
  where
    interp _ [] = []
    interp _ [(_, r)] = repeat r
    interp t0 ((t1,r):trs) =
      let n = ceiling ((t1-t0) / ?h)
          prefix = replicate n r
          postfix = interp t1 trs
      in prefix ++ postfix


-- | Generates an infinite list of time/reference value pairs.
refGen :: (Double, Double) -- ^ upper and lower limit
       -> (Double, Double) -- ^ max and min interval
       -> (Double, Double) -- ^ max and min step
       -> Gen [(Double, Double)]
refGen = undefined

-- | Generate a stream from elements in list
elemRefGen :: RealFrac a => (a, a) -> [b] -> [(a, b)]
elemRefGen = undefined

-- elemRefGen example

-- | Generate a stream from an atomaton.
automRefGen :: (RealFrac a, Eq b)
            =>(a, a)    -- ^ min max interval length
            -> [(b, b)] -- ^ edges
            -> [(a, b)]
automRefGen = undefined

-- automRefGen example

data Gears = One | Two | Three

automEx = refStream (automRefGen bounds automata)
  where
    ?h = 0.1
    automata = [(One, Two), (Two, One), (Two, Three), (Three, Two)]
    bounds = (1, 3)

--instance Arbitrary MM where
--  arbitrary = refGen (100, 10) (5, -5) (1000, 300)

-- reference "strömmen med referensvärden"

--
