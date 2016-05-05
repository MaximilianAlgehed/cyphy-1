{-# LANGUAGE ImplicitParams #-}

module CyphyUtils where

import Prelude hiding (lookup, until)
import Data.List hiding (lookup)
import Data.Map (lookup, fromList, empty, insertWith)
import System.Random (Random)

import Zelus
import NotQuickCheck

-----------------------------------------------
----- Time related combinators
---

observeFor :: (?h :: Double) => Double -> S b -> S b
observeFor = take . steps

-- | Predicate should be satisfied within the time limit.
-- Alternative types:
-- Double -> E a -> Bool
-- Double -> S Bool -> Bool   <<< perhaps easiest to use?
-- ???
within :: (?h :: Double) => Double -> (a -> Bool) -> S a -> Bool
within t p = or . take (steps t) . map p    --- <<< h!

-- | Predicate should hold until time limit.
until :: (?h :: Double) => Double -> (a -> Bool) -> S a -> Bool
until t p = and . take (steps t) . map p     --- <<< h!

-- what is forever? needs more information than just h (i.e. max
-- observed time)
forever :: (?h :: Double, ?tmax :: Double) => (a -> Bool) -> S a -> Bool
forever = until ?tmax                  --- <<< h!

after :: (?h :: Double, ?tmax :: Double) => Double -> (a -> Bool) -> S a -> Bool
after t p = forever p . drop (steps t)    --- <<< h!



-----------------------------------------------
----- Arbitrary helper generators
---

-- Other ideas:
-- intervalGen with different distributions, uniform, binomial...
-- limits, max, min
-- more continous stuff, perlin noise etc

-- the name dr indicates a relative reference change
-- the name dt indicates time between reference changes
-- an interval is a pair (dt, dr)

stepGen :: (Random a, Ord a, Num a,
            Random b, Ord b, Num b, ?h :: b)
        => Int              -- ^ number of steps
        -> (a, a)           -- ^ range (min, max)
        -> (b, b)           -- ^ interval length (min, max)
        -> Gen ([(b, a)], S a)
stepGen n range dt =
  do values <- chooses range
     dts <- chooses dt
     let refs = take n (zip dts values)
     return (refs, refStream refs)


stepLimitedGen :: (Random value, Ord value, Num value,
                   Random time, Ord time, Num time, ?h :: time)
               => Int              -- ^ number of steps
               -> (value, value)   -- ^ range (min, max)
               -> (time, time)     -- ^ interval length (min, max)
               -> (value, value)   -- ^ step (max down, max up)
               -> Gen ([(time, value)], S value)
stepLimitedGen n range dt dr =
  do let (low, high) = range
     r0 <- choose range
     values <- steps r0
     dts <- chooses dt
     let drs = limitAbs low range (take n values)
     let intervals = zip dts drs
     return (intervals, refStream intervals)
  where
    steps r0 =
      do r1 <- choose dr
         let limited = limit range (r0 + r1)
         rs <- steps limited
         return (limited : rs)

-- | This limit strategy will adjust reference deltas to never step outside
-- the given range.
-- For example
--     limit (1, 2) [1, 1, 1]
-- will result in
--     [1, 1, 0]
limitAbs :: (Ord value, Num value)
            => value            -- ^ Absolute initial value.
            -> (value, value)   -- ^ Range to keep absolute value within.
            -> [value]          -- ^ Relative value changes.
            -> [value]
limitAbs init range = cap init
  where
    (low, high) = range
    cap _ [] = []
    cap acc (dr:drs) =
      let acc' = acc + dr
          dr' = if acc' >= high
                then high - acc
                else if acc' <= low
                     then low - acc
                     else dr
      in dr' : cap (limit range acc') drs

steps :: (Ord a, Num a, ?h :: a) => a -> Int
steps = steps' 0
  where
    steps' acc t
      | t <= 0 = 0
      | otherwise = steps' (acc + 1) (t - ?h)          --- <<< h!

toAbsolute :: Num a => [(a, b)] -> [(a, b)]
toAbsolute refs = scanl1 f refs
  where
    f (t, b1) (dt, b2) = (t + dt, b2)

toAbs :: Num a => a -> [a] -> [a]
toAbs init dxs = scanl (+) init dxs

toRel :: Num a => [a] -> [a]
toRel (x:xs) = snd (mapAccumL f x xs)
  where
    f acc x = (x, x - acc)

-- | Creates a reference stream from a list of time/reference pairs.
-- Time and reference values are assumed to be absolute.
-- Reference changes on or at the nearest sampling point after the
-- time coordinate. This means there is some uncertainty in what
-- value is held at the interval limits, but there will not be any
-- accumulation of error over time.
-- If the input list is finite the last reference value will be held
-- indefinitely.
-- Not defined for empty lists.
refStream :: (Ord time, Num time, ?h :: time)
          => [(time, value)]
          -> S value
refStream [(_, r)] = repeat r
refStream ((t0, r0):(t1, r1):refs)
  | t0 >= t1 = r1 : refStream ((t1 + ?h, r1):refs)
  | otherwise = r0 : refStream ((t0 + ?h, r0):(t1, r1):refs)

dRefStream :: (Ord time, Num time, ?h :: time, Num value)
          => [(time, value)]
          -> S value
dRefStream [(_, r)] = repeat r
dRefStream ((t0, r0):(t1, r1):refs)
  | t0 <= 0 = let absr = r0 + r1 in absr : dRefStream ((t1 - ?h, absr):refs)
  | otherwise = r0 : dRefStream ((t0 - ?h, r0):(t1, r1):refs)

chooses :: Random a => (a, a) -> Gen (S a)
chooses bounds = infiniteListOf (choose bounds)

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

limit :: Ord a => (a, a) -> a -> a
limit (lower, upper) x = max (min x upper) lower

-- | Gives all non-empty and strictly smaller combinations of the input elements
-- which preserve the order.
-- For example
--     shuffle [1,2,3]
-- results in
--     [[1],[2],[1,2],[3],[1,3],[2,3]]
combinations :: [[a]] -> [[[a]]]
combinations xs
  | length xs <= 2 = []
  | otherwise = init (tail (subsequences xs))

segments :: Int -> [a] -> [[a]]
segments _ [] = []
segments n xs | n <= 0 = []
segments n xs = let (ys, zs) = splitAt n xs in ys : segments n zs

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs) = (if a == x then b else x) : replace a b xs

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

trd4 :: (a, b, c, d) -> c
trd4 (_, _, c, _) = c

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, d) = d

printLines :: Show a => [a] -> IO ()
printLines = mapM_ print

-----------------------------------------------
----- More axamples
---

stream = do
  es <- generate (elemGen 1 [1..5])
  ts <- generate (chooses (0, 1)) :: IO [Double]
  let ps = take 5 (zip ts es)
  return (refStream ps)
  where ?h = 0.1
