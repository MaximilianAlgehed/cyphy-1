{-# LANGUAGE ImplicitParams #-}

module Zelus where

import Control.Applicative( Alternative(..) )
import Data.List( sortOn )

infixr 0 >--, -->
infix  1 ?
infixr 2 ||?
infixr 3 &&?
infix  4 >=?, <=?, >?, <?, ==?, /=?

--------------------------------------------------------------------------------
-- streams

type S a = [a]

--------------------------------------------------------------------------------
-- basic functions on streams

-- constant stream
val :: a -> S a
val = repeat

-- delay the second stream by one step, and use the initial value from the first
(|->) :: S a -> S a -> S a
(x:_) |-> xs = x:xs

-- override the initial value of the second stream using the first
(|>) :: S a -> S a -> S a
(x:_) |> ~(_:xs) = x:xs

-- delay the stream by one step, copying the first value.
-- NOTE: this will not terminate in a loop, you MUST also use |> in that case!
pre :: S a -> S a
pre xs = xs |-> xs

--------------------------------------------------------------------------------
-- other useful functions on streams

start :: S Bool
start = val True |> val False

deriv :: (Fractional a, ?h :: a) => S a -> S a
deriv x = (x - pre x) / val ?h

up, down, changed :: Ord a => S a -> S Bool
up   x = x >? pre x
down x = x <? pre x
changed x = up x ||? down x

(?) :: S Bool -> (S a, S a) -> S a
c ? (x,y) = [ if c_ then x_ else y_ | (c_,(x_,y_)) <- c `zip` (x `zip` y) ]

nt :: S Bool -> S Bool
nt = fmap not

(&&?), (||?) :: S Bool -> S Bool -> S Bool
(&&?) = zipWith (&&)
(||?) = zipWith (||)
(->?) = zipWith ((||) . not)

(>=?), (>?), (<?), (<=?) :: Ord a => S a -> S a -> S Bool
(>=?) = zipWith (>=)
(>?)  = zipWith (>)
(<?)  = zipWith (<)
(<=?) = zipWith (<=)

(==?), (/=?) :: Eq a => S a -> S a -> S Bool
(==?) = zipWith (==)
(/=?) = zipWith (/=)

mn, mx :: Ord a => S a -> S a -> S a
mn = zipWith min
mx = zipWith max

--------------------------------------------------------------------------------
-- numeric functions on streams

instance Num a => Num [a] where
  (+)    = zipWith (+)
  (-)    = zipWith (-)
  (*)    = zipWith (*)
  abs    = map abs
  signum = map signum
  fromInteger n = let x = fromInteger n in val x

instance Fractional a => Fractional [a] where
  (/) = zipWith (/)
  fromRational q = let x = fromRational q in val x

instance Floating a => Floating [a] where
  exp     = fmap exp
  log     = fmap log
  sqrt    = fmap sqrt
  (**)    = zipWith (**)
  logBase = zipWith logBase

  pi    = val pi

  sin   = fmap sin
  cos   = fmap cos
  tan   = fmap tan

  asin  = fmap asin
  acos  = fmap acos
  atan  = fmap atan

  sinh  = fmap sinh
  cosh  = fmap cosh
  tanh  = fmap tanh

  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

--------------------------------------------------------------------------------
-- event streams

newtype E a = E{ unE :: S (Maybe a) } deriving Show

instance Applicative E where
  pure x        = E (val (Just x))
  E fs <*> E xs = E (zipWith app fs xs)
   where
    app (Just f) (Just x) = Just (f x)
    app _        _        = Nothing

instance Functor E where
  fmap f (E xs) = E (map (fmap f) xs)

instance Alternative E where
  empty       = E (val Nothing)
  E x <|> E y = E (zipWith (<|>) x y)

when :: S a -> S Bool -> E a
x `when` b = E (b ? (Just <$> x,val Nothing))

whenEvent :: S a -> E b -> E a
xs `whenEvent` E es = E (zipWith f xs es)
  where
    f _ Nothing  = Nothing
    f x (Just _) = Just x

change :: S a -> E a -> S a
~(_:xs) `change` E (Just x :rs) = x : change xs (E rs)
(x:xs)  `change` E (Nothing:rs) = x : change xs (E rs)

isEvent :: Eq a => E a -> S a -> S Bool
E (Just e : es) `isEvent` (x : xs) = (x == e) : isEvent (E es) xs
E (_ : es)      `isEvent` (_ : xs) = False : isEvent (E es) xs

--------------------------------------------------------------------------------
-- derivative-specified streams

data DerS a = DerS
  { diffs  :: S a
  , resets :: E a
  } deriving Show

in1t :: S a -> S a -> DerS a
d `in1t` x = DerS d (x `when` start)

reset :: DerS a -> E a -> DerS a
DerS d r `reset` r' = DerS d (r' <|> r)

--integ :: Num a => DerS a -> S a
--integ (DerS d r) = x
-- where
--  x = (pre x + d) `change` r
--  --x = (pre x `change` r) + d

-- | Comment implicit parameter.
integ :: (Num a, ?h :: a) => DerS a -> S a
integ (DerS d r) = x
  where
    x = (pre x + d * val ?h) `change` r

integral :: (Num a, ?h :: a) => S a -> S a
integral dx = integ (dx `in1t` 0)

rk4 :: Fractional a => (a -> a -> a) -> S a -> a -> S a
rk4 f tn x0 = x
  where
    x  = xn + k1/6 + k2/3 + k3/3 + k4/6
    xn = val x0 |-> x
    dt = tn - pre tn
    k1 = dt * zipWith f xn tn
    k2 = dt * zipWith f (xn + k1/2) (tn + dt/2)
    k3 = dt * zipWith f (xn + k2/2) (tn + dt/2)
    k4 = dt * zipWith f (xn + k3)   (tn + dt)

--------------------------------------------------------------------------------
-- automata

automaton :: Eq a => [(a,S Bool,a)] -> S a
automaton ts@((s0,_,_):_) = s
 where
  s = val s0 |-> trans ts

  trans []             = s
  trans ((s1,t,s2):ts) = s ==? val s1 &&? t ? (val s2, trans ts)

-- cute syntax
(-->) :: a -> b -> (a,b)
x --> y = (x,y)

(>--) :: a -> (b,c) -> (a,b,c)
x >-- (y,z) = (x,y,z)

took :: Eq a => S a -> (a,a) -> S Bool
took s (x,y) = pre s ==? val x &&? s ==? val y

--------------------------------------------------------------------------------
-- properties

for :: Int -> S Bool -> Bool
for n xs = and (take n xs)

shrinkFloat :: (Ord a, RealFrac a) => a -> [a]
shrinkFloat x =
  filter smaller $ sortOn abs $
  [ r
  , r+1
  , r * 0.5
  , x * 0.5
  , x * 0.75
  , x * 0.9
  ] ++
  [ fromIntegral (floor (x * k)) / k
  | k <- [10,100,1000,1000,10000,100000,1000000]
  ]
 where
  r = fromIntegral (floor x)

  smaller y
    | 0 <= y && y < x = True
    | y <  0 && x < y = True
    | fromIntegral (floor y) == y && r /= x = True
    | otherwise       = False

--------------------------------------------------------------------------------
-- some examples

{-
In Zelus, you write:

  der x = <expr>

In Zelus.hs, we write instead:

  x = integ <expr>
-}

-- count upwards by 1 from 0
example1 = integ (1 `in1t` 0) where ?h = 0.01

-- count upwards by 1 from 0, resetting to 3 every 10 steps
example2 = integ (1 `in1t` 0 `reset` (3 `when` every 10)) where ?h = 0.01

every k  = cycle (replicate (k-1) False ++ [True])

--------------------------------------------------------------------------------
