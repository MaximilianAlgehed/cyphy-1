{-
http://www.kestreltechnology.com/downloads/sttt-tacas02-7.pdf
-}

module PtLTL where

import Zelus

type Dool = Double

-- Propositional operators

true, false :: [Bool]
true = repeat True
false = repeat False

-- Temporal operators

prev, once, always :: [Bool] -> [Bool]
prev f = False:f
once = scanl1 (||)
always = scanl1 (&&)

sinces, sincew :: [Bool] -> [Bool] -> [Bool]
f1 `sinces` f2 = f2 ||? (f1 &&? (head f1 : f1 `sinces` f2))
f1 `sincew` f2 = always f1 ||? (f1 `sinces` f2)

holds, holdw :: [Bool] -> Int -> [Bool]
holds f samples = go f 0
  where
    go (True:bs) n = True : go bs (samples - 1)
    go (False:bs) 0 = False : go bs 0
    go (False:bs) n = True : go bs (n - 1)
holdw f samples = always (nt f) ||? holds f samples

-- Monitoring operators

begin, end :: [Bool] -> [Bool]
begin = binMap (>)
end = binMap (<)

intervals, intervalw ::  [Bool] -> [Bool] -> [Bool]
f1 `intervals` f2 = nt f2 &&? (f1 ||? (head f1 : f1 `intervals` f2))
f1 `intervalw` f2 = always (nt f2) ||? (f1 `intervals` f2)

-- Utilities

-- | Apply a binary function to pairs of neighboring elements in a list.
--
-- > binMap f [x_1, x_2, x_3, .., x_n]
-- > =
-- > [f x_1 x_1, f x_1 x_2, f x_2 x_3, .., f x_(n-1) f x_n]
binMap :: (a -> a -> b) -> [a] -> [b]
binMap f [] = []
binMap f xs = zipWith f (head xs : xs) xs

t = True
f = False

reduce :: [Bool] -> Bool
reduce = foldl1 (&&)

-- Cyphy specific

steady :: [Double] -> [Double] -> [Bool] -> Int -> Double -> [Bool]
steady ref act resets samples margin =
    always (holdw resets samples ||? bounded)
  where
    bounded = abs (1 - ref/act) <? val margin

overshoot = undefined

undershoot = undefined

rise = undefined

fall = undefined

-- Examples

ex1 = cycle [t, f, f, t, f, f, t]
ex2 = replicate 3 f ++ [t] ++ repeat f
ex3 = replicate 3 f ++ repeat t
ex4 = replicate 3 f ++ replicate 6 t ++ repeat f
ex5 = replicate 5 f ++ repeat t

-- | From the article, page 10.
-- “whenever p becomes true, then q has been true in the past,
-- and since then we have not yet seen the end of r or s”
g p q r s = always (begin p ->? (q `intervals` end (r ||? s)))

gsucc =
  g ([f,f,f,t,t,f,f,f,f,f,t,f,f,f] ++ false)
    ([f,t,f,f,t,f,f,f,t,f,f,f,f,f] ++ false)
    ([t,f,f,f,f,f,f,f,f,f,f,f,f,f] ++ false)
    ([f,f,f,f,f,f,t,f,f,f,f,f,t,t] ++ false)

gfail =
  g ([f,f,f,t,t,t,t,f,t,t,t,f,f,f] ++ false)
    ([f,t,f,f,t,f,f,f,t,f,f,f,f,f] ++ false)
    ([t,f,f,f,f,f,f,f,f,f,f,f,f,f] ++ false)
    ([f,f,f,f,f,f,t,f,f,f,f,f,t,t] ++ false)

-- | Steady state.
-- "either system is within settling time t_limit after a reference change
-- or error is less than max error"
h ref act err_max t_limit =
    always (steady ref act ref_changed t_limit err_max)
  where
    ref_changed = binMap (/=) ref

hsucc =
    h ref act err_max t_limit
  where
    ref = ([1,1,1,1,1,1,1,5,5,5,4,4,4,4,4,4,4,4,9,9,9,9,9,9,9] ++ repeat 8)
    act = map (*1.01) ref
    t_limit = 4
    err_max = 0.05

hfail =
    h ref act err_max t_limit
  where
    ref = ([1,1,1,1,1,1,1,5,5,5,4,4,4,4,4,4,4,4,9,9,9,9,9,9,9] ++ repeat 8)
    act = map (*1.06) ref
    t_limit = 4
    err_max = 0.05

resettings = every 6 `intervals` (replicate 3 False ++ every 6)
resettingw = every 6 `intervalw` (replicate 3 False ++ every 6)

-- Draxx them sklounst
