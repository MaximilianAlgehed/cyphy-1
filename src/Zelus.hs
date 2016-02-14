{-# LANGUAGE ScopedTypeVariables #-}
module Zelus where

import Control.Applicative( Alternative(..) )

import Test.QuickCheck
import Test.QuickCheck.Poly

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
(x:_) |> (_:xs) = x:xs

-- delay the stream by one step, copying the first value.
-- NOTE: this will not terminate in a loop, you MUST also use |> in that case!
pre :: S a -> S a
pre xs = xs |-> xs

-- some properties, for documentation

prop_PreFbyDelay xs (ys :: S A) =
  not (null xs) && not (null ys) ==>
    (xs |> pre ys) == (xs |-> ys)

--------------------------------------------------------------------------------
-- other useful functions on streams

start :: S Bool
start = val True |> val False

deriv :: Num a => S a -> S a
deriv x = x - pre x

up, down :: Ord a => S a -> S Bool
up   x = x >? pre x
down x = x <? pre x

(?) :: S Bool -> (S a, S a) -> S a
c ? (x,y) = [ if c_ then x_ else y_ | (c_,(x_,y_)) <- c `zip` (x `zip` y) ]

nots :: S Bool -> S Bool
nots = fmap not

(&&?), (||?) :: S Bool -> S Bool -> S Bool
(&&?) = zipWith (&&)
(||?) = zipWith (||)

(>=?), (>?), (<?), (<=?) :: Ord a => S a -> S a -> S Bool
(>=?) = zipWith (>=)
(>?)  = zipWith (>)
(<?)  = zipWith (<)
(<=?) = zipWith (<=)

(==?), (/=?) :: Eq a => S a -> S a -> S Bool
(==?) = zipWith (==)
(/=?) = zipWith (/=)

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

--------------------------------------------------------------------------------
-- event streams

newtype E a = E{ unE :: S (Maybe a) }

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

change :: S a -> E a -> S a
~(_:xs) `change` E (Just x :rs) = x : change xs (E rs)
(x:xs)  `change` E (Nothing:rs) = x : change xs (E rs)

--------------------------------------------------------------------------------
-- derivative-specified streams

data DerS a = DerS
  { diffs  :: S a
  , resets :: E a
  }

in1t :: S a -> S a -> DerS a
d `in1t` x = DerS d (x `when` start)

reset :: DerS a -> E a -> DerS a
DerS d r `reset` r' = DerS d (r <|> r')

integ :: Num a => DerS a -> S a
integ (DerS d r) = x
 where
  --x = (pre x + d) `change` r
  x = (pre x `change` r) + d

integral :: Num a => S a -> S a
integral dx = integ (dx `in1t` 0)

--------------------------------------------------------------------------------
-- some examples

{-
In Zelus, you write:

  der x = <expr>

In Zelus.hs, we write instead:

  x = integ <expr>
-}

-- count upwards by 1 from 0
example1 = integ (1 `in1t` 0)

-- count upwards by 1 from 0, resetting to 3 every 10 steps
example2 = integ (1 `in1t` 0 `reset` (3 `when` every 10))

every k  = cycle (replicate (k-1) False ++ [True])

--------------------------------------------------------------------------------
