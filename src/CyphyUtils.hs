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

-- Better division:
-- Ways of creating succession of values
-- and ways of creating time intervals
-- then just zip them up.

-- Other ideas:
-- intervalGen with different distributions, uniform, binomial...
-- limits, max, min

-- | Creates a reference stream from a list of interval/reference pairs.
refStream :: (Ord a, Num a, ?h :: a) => [(a, b)] -> S b
refStream = foldr1 (++) . map (uncurry f)
  where
    f t r
      | t < 0 = []
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
