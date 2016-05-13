{-
http://www.kestreltechnology.com/downloads/sttt-tacas02-7.pdf
-}

module PtLTL where

type Dool = Double

-- Relational operators

(<:), (<=:), (>:), (>=:), (==:), (/=:) :: [Double] -> [Double] -> [Bool]
(<:) = zipWith (<)
(<=:) = zipWith (<=)
(>:) = zipWith (>)
(>=:) = zipWith (>=)
(==:) = zipWith (==)
(/=:) = zipWith (/=)

-- Propositional operators

true, false :: [Bool]
true = repeat True
false = repeat False

noot :: [Bool] -> [Bool]
noot = map not

(&&:), (||:), (->:) :: [Bool] -> [Bool] -> [Bool]
(&&:) = zipWith (&&)
(||:) = zipWith (||)
f1 ->: f2 = noot f1 ||: f2

-- Temporal operators

prev, once, always :: [Bool] -> [Bool]
prev f = False:f
once = scanl1 (||)
always = scanl1 (&&)

sinces, sincew :: [Bool] -> [Bool] -> [Bool]
f1 `sinces` f2 = f2 ||: (f1 &&: (head f1 : f1 `sinces` f2))
f1 `sincew` f2 = always f1 ||: (f1 `sinces` f2)

holds, holdw :: [Bool] -> Int -> [Bool]
holds f samples = go f 0
  where
    go (True:bs) n = True : go bs (samples - 1)
    go (False:bs) 0 = False : go bs 0
    go (False:bs) n = True : go bs (n - 1)
holdw f samples = always (noot f) ||: holds f samples

-- Monitoring operators

start, end :: [Bool] -> [Bool]
start = binMap (>)
end = binMap (<)

intervals, intervalw ::  [Bool] -> [Bool] -> [Bool]
f1 `intervals` f2 = noot f2 &&: (f1 ||: (head f1 : f1 `intervals` f2))
f1 `intervalw` f2 = always (noot f2) ||: (f1 `intervals` f2)

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

-- Examples

ex1 = cycle [t, f, f, t, f, f, t]
ex2 = replicate 3 f ++ [t] ++ repeat f
ex3 = replicate 3 f ++ repeat t
ex4 = replicate 3 f ++ replicate 6 t ++ repeat f
ex5 = replicate 5 f ++ repeat t

every n = cycle (replicate (n-1) f ++ [t])

-- | From the article, page 10.
-- “whenever p becomes true, then q has been true in the past,
-- and since then we have not yet seen the end of r or s”
g p q r s = always (start p ->: (q `intervals` end (r ||: s)))

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

-- | Made up but relevant.
-- "a t_limit after a reference change the error must not
-- exceede err_max"
h ref act err_max t_limit =
    -- hold ref_changed t_limit
    -- (err >: repeat err_max)
    (noot (holdw ref_changed t_limit) ->: (err >: repeat err_max))
    -- always (hold ref_changed t_limit `intervalw` (err >: repeat err_max))
  where
    ref_changed = binMap (/=) ref
    err = map abs (zipWith (-) ref act)

hsucc =
  h ([1,1,1,1,1,7,7,7,7,8,4,4,4,4] ++ repeat 10)
    ([1,1,1,1,1,2,3,5,6,6,5,5,5,3] ++ repeat 10)
    1
    3

resettings = every 6 `intervals` (replicate 3 False ++ every 6)
resettingw = every 6 `intervalw` (replicate 3 False ++ every 6)

-- Draxx them sklounst
