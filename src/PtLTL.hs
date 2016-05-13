-- | Past time Linear Temporal Logic with notions of truthness and delays.
-- Based on http://www.kestreltechnology.com/downloads/sttt-tacas02-7.pdf
module PtLTL where

import Zelus


--------------------------------------------------------------------------------
------- Temporal operators
---

-- | f held in the previous moment.
prev :: [Bool] -> [Bool]
prev f = False:f

-- | f held at some past moment.
once :: [Bool] -> [Bool]
once f = scanl1 (||) f

-- | f has held in all past moments.
always :: [Bool] -> [Bool]
always = scanl1 (&&)

-- | Strong since.
-- "f2 held at some moment in the past and, since then, f1 held all the time"
sinces :: [Bool] -> [Bool] -> [Bool]
f1 `sinces` f2 = f2 ||? (f1 &&? (head f1 : f1 `sinces` f2))

-- | Weak since.
-- "either f1 was true all the time or f1 `sinces` f2"
sincew :: [Bool] -> [Bool] -> [Bool]
f1 `sincew` f2 = always f1 ||? (f1 `sinces` f2)

-- | If f holds in some moment then True is repeated for another
-- (samples - 1) moments.
holds :: [Bool] -> Int -> [Bool]
holds f samples = go f 0
  where
    go (True:bs) n = True : go bs (samples - 1)
    go (False:bs) 0 = False : go bs 0
    go (False:bs) n = True : go bs (n - 1)

-- | Either f was always false or holds f samples.
holdw :: [Bool] -> Int -> [Bool]
holdw f samples = always (nt f) ||? holds f samples


--------------------------------------------------------------------------------
------- Monitoring operators
---

-- | In the previus moment f held, but not in this one.
begin :: [Bool] -> [Bool]
begin f = binMap (>) f

-- | In the previous moment f didn't hold, but it does in this one.
end :: [Bool] -> [Bool]
end = binMap (<)

-- | Strong interval.
-- "f1 held at some moment in the past and f2 has not held
-- since then (inclusive)"
intervals ::  [Bool] -> [Bool] -> [Bool]
f1 `intervals` f2 = nt f2 &&? (f1 ||? (head f1 : f1 `intervals` f2))

-- | Weak interval.
-- "f2 has never been true or f1 `intervals` f2"
intervalw ::  [Bool] -> [Bool] -> [Bool]
f1 `intervalw` f2 = always (nt f2) ||? (f1 `intervals` f2)


--------------------------------------------------------------------------------
------- Cyphy specific
---

-- | After a reset the value must be in the margin of the reference within the
-- given time.
steady :: [Double] -- ^ Reference
       -> [Double] -- ^ Actual value
       -> [Bool]   -- ^ Resets, usually reference change
       -> Int      -- ^ Number of samples to hold delay
       -> Double   -- ^ Allowed deviation from reference
       -> [Bool]
steady ref act resets samples margin =
    always (holdw resets samples ||? bounded)
  where
    bounded = abs (1 - ref/act) <? val margin

-- | The value must not overshoot the reference by more than the given margin.
overshoot :: [Double] -- ^ Reference
          -> [Double] -- ^ Actual value
          -> Double   -- ^ Allowed overshoot
          -> [Bool]
overshoot ref act margin =
    always ((begin (up ref) `intervalw` end (down ref)) ->? bounded)
  where
    bounded = act/ref - 1 <? val margin

-- | The value must not undershoot the reference by more than the given margin.
undershoot :: [Double] -- ^ Reference
           -> [Double] -- ^ Actual value
           -> Double   -- ^ Allowed undershoot
           -> [Bool]
undershoot ref act margin =
    always ((begin (down ref) `intervalw` end (up ref)) ->? bounded)
  where
    bounded = ref/act - 1 <? val margin

-- | The value must rise from 5% of reference to 95% of reference within
-- the given time.
rise :: [Double] -- ^ Reference
     -> [Double] -- ^ Actual value
     -> Int      -- ^ Allowed time in samples
     -> [Bool]
rise ref act samples = always limit
  where
    lower = up (act >=? ref * 0.05)
    upper = up (act >=? ref * 0.95)
    limit = (holdw (begin lower) samples ||? once upper) `sincew` begin lower

-- | The value must fall from 5% of reference to 95% of reference within
-- the given time.
fall :: [Double] -- ^ Reference
     -> [Double] -- ^ Actual value
     -> Int      -- ^ Allowed time in samples
     -> [Bool]
fall ref act samples = always limit
  where
    lower = down (act >=? ref * 0.05)
    upper = down (act >=? ref * 0.95)
    limit = (holdw (begin upper) samples ||? once lower) `sincew` begin upper


--------------------------------------------------------------------------------
------- Utilities
---

-- | Apply a binary function to pairs of neighboring elements in a list.
--
-- > binMap f [x_1, x_2, x_3, .., x_n]
-- > =
-- > [f x_1 x_1, f x_1 x_2, f x_2 x_3, .., f x_(n-1) f x_n]
binMap :: (a -> a -> b) -> [a] -> [b]
binMap f [] = []
binMap f xs = zipWith f (head xs : xs) xs


---
------- Draxx them sklounst
--------------------------------------------------------------------------------
