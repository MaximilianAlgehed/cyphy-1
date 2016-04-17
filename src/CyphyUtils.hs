{-# LANGUAGE ImplicitParams #-}

module CyphyUtils where

import Prelude hiding (lookup)
import Data.Map (lookup, fromList, empty, insertWith)
import System.Random (Random)

import Zelus
import NotQuickCheck

-----------------------------------------------
----- Arbitrary helper generators
---

-- Other ideas:
-- intervalGen with different distributions, uniform, binomial...
-- limits, max, min
-- more continous stuff, perlin noise etc

stepGen :: (Random a, Ord a, Num a,
            Random b, Ord b, Num b, ?h :: b)
        => Int              -- ^ number of steps
        -> (a, a)           -- ^ range (min, max)
        -> (b, b)           -- ^ interval length (min, max)
        -> Gen ([(b, a)], S a)
stepGen n range dt =
  do values <- intervalGen range
     dts <- intervalGen dt
     let refs = take n (zip dts values)
     return (refs, refStream refs)

stepLimitedGen :: (Random a, Ord a, Num a,
                   Random b, Ord b, Num b, ?h :: b)
               => Int              -- ^ number of steps
               -> (a, a)           -- ^ range (min, max)
               -> (b, b)           -- ^ interval length (min, max)
               -> (a, a)           -- ^ step (max down, max up)
               -> Gen ([(b, a)], S a)
stepLimitedGen n range dt dr =
  do r0 <- choose range
     values <- steps r0
     dts <- intervalGen dt
     let refs = take n (zip dts values)
     return (refs, refStream refs)
  where
    steps r0 =
      do r1 <- choose dr
         let limited = limit (r0 + r1) range
         rs <- steps limited
         return (limited : rs)

toAbsolute :: Num a => [(a, b)] -> [(a, b)]
toAbsolute refs = scanl1 f refs
  where
    f (t, b1) (dt, b2) = (t + dt, b2)

-- | Creates a reference stream from a list of interval/reference pairs.
refStream :: (Ord a, Num a, ?h :: a)
          => [(a, b)] -- ^ a is dt, b is ref value
          -> S b
refStream refs =
  foldr1 (++) (map (uncurry f) refs) ++ repeat (snd (last refs))
  where
    f t r
      | t <= 0 = []
      | otherwise = r : f (t - ?h) r

intervalGen :: Random a => (a, a) -> Gen (S a)
intervalGen bounds = infiniteListOf (choose bounds)

-- | Generate a stream from elements in list.
-- No element will occur twice in a row. Guaranteed. Unless it occurs
-- twice in the input.
weightedElemGen :: Ord a => a -> [(Int, a)] -> Gen (S a)
weightedElemGen start elems = weightedAutomGen start automata
  where
    automata = [(from, weight, to) | (_, from) <- elems
                                   , (weight, to) <- elems
                                   , from /= to]

elemGen :: Ord a => a -> [a] -> Gen (S a)
elemGen start elems = weightedElemGen start (zip (repeat 1) elems)

data Color = Red | Green | Blue | Yellow | Purple deriving (Show, Eq, Ord)

elemEx :: IO (S Color)
elemEx = generate (elemGen Red [Red, Green, Blue, Yellow, Purple])

-- | Generate a stream from an automata.
-- Assumes edges are unique or that biased weights doesn't matter.
-- If there is a sink in the automata and the path leads to it the stream
-- will stay in that state for ever.
weightedAutomGen :: Ord a => a -> [(a, Int, a)] -> Gen (S a)
weightedAutomGen start edges = walk start
  where
    graph = foldr f empty edges

    f (from, weight, to) g = insertWith (++) from [(weight, return to)] g

    walk current = case lookup current graph of
      Just nexts -> frequency nexts >>= walk >>= return . (current:)
      Nothing -> return (repeat current)

automGen :: Ord a => a -> [(a, a)] -> Gen (S a)
automGen start edges = weightedAutomGen start edges'
  where
    (froms, tos) = unzip edges
    edges' = zip3 froms (repeat 1) tos

data Gears = One | Two | Three deriving (Show, Eq, Ord)

automEx :: IO (S Gears)
automEx = generate (automGen One automata)
  where
    automata = [(One, Two), (Two, One), (Two, Three), (Three, Two)]

-----------------------------------------------
----- Small stuff
---

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

limit :: Ord a => a -> (a, a) -> a
limit x (lower, upper) = max (min x upper) lower

-----------------------------------------------
----- More axamples
---

stream = do
  es <- generate (elemGen 1 [1..5])
  ts <- generate (intervalGen (0, 1)) :: IO [Double]
  let ps = take 5 (zip ts es)
  return (refStream ps)
  where ?h = 0.1
