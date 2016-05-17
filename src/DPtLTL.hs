-- | Past time Linear Temporal Logic with notions of truthness and delays.
-- Based on http://www.kestreltechnology.com/downloads/sttt-tacas02-7.pdf
module DPtLTL where

import Dool

--------------------------------------------------------------------------------
------- Temporal operators
---

-- | f held in the previous moment.
prev :: [Dool] -> [Dool]
prev f = false:f

-- | f held at some past moment.
once :: [Dool] -> [Dool]
once f = scanl1 (||.) f

-- | f has held in all past moments.
always :: [Dool] -> [Dool]
always = scanl1 (&&.)

-- | Strong since.
-- "f2 held at some moment in the past and, since then, f1 held all the time"
sinces :: [Dool] -> [Dool] -> [Dool]
f1 `sinces` f2 = f2 ||: (f1 &&: (head f1 : f1 `sinces` f2))

-- | Weak since.
-- "either f1 was true all the time or f1 `sinces` f2"
sincew :: [Dool] -> [Dool] -> [Dool]
f1 `sincew` f2 = always f1 ||: (f1 `sinces` f2)

-- | If f holds in some moment then true is repeated for another
-- (samples - 1) moments.
holds :: [Dool] -> Int -> [Dool]
holds f samples = go f 0
  where
    go (d:ds) n
      | isTrue d = d : go ds (samples - 1)
      | n == 0 = d : go ds 0
      | otherwise = nt d : go ds (n - 1)

-- | Either f was always false or holds f samples.
holdw :: [Dool] -> Int -> [Dool]
holdw f samples = always (nts f) ||: holds f samples


--------------------------------------------------------------------------------
------- Monitoring operators
---

-- | In the previus moment f held, but not in this one.
-- It is unclear which truth values to use.
begin :: [Dool] -> [Dool]
begin = binMap op
  where
    op last now = if isTrue last && isFalse now then last else now &&. nt now

-- | In the previous moment f didn't hold, but it does in this one.
-- It is unclear which truth values to use.
end :: [Dool] -> [Dool]
end = binMap op
  where
    op last now = if isFalse last && isTrue now then now else now &&. nt now

-- | Strong interval.
-- "f1 held at some moment in the past and f2 has not held
-- since then (inclusive)"
intervals ::  [Dool] -> [Dool] -> [Dool]
f1 `intervals` f2 = nts f2 &&: (f1 ||: (head f1 : f1 `intervals` f2))

-- | Weak interval.
-- "f2 has never been true or f1 `intervals` f2"
intervalw ::  [Dool] -> [Dool] -> [Dool]
f1 `intervalw` f2 = always (nts f2) ||: (f1 `intervals` f2)


--------------------------------------------------------------------------------
------- Utilities
---

-- | Apply a binary function to neighboring elements in a list.
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
