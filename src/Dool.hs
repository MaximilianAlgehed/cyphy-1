module Dool where

infixr 1 =>. , =>:
infixr 2 ||. , ||:
infixr 3 &&. , &&:
infix 4 <=. , <. , >. , >=. , ==. , /=.
infix 4 <=: , <: , >: , >=: , ==: , /=:

type Dool = Double

true, false :: Dool
true  = 1
false = -1

trues, falses :: [Dool]
trues = repeat true
falses = repeat false

isTrue, isFalse :: Dool -> Bool
isTrue = (>0)
isFalse = (<0)

nt :: Dool -> Dool
nt = negate

nts :: [Dool] -> [Dool]
nts = fmap nt

(&&.), (||.), (=>.) :: Dool -> Dool -> Dool
(&&.) = min
(||.) = max
(=>.) = max . negate

(&&:), (||:), (=>:) :: [Dool] -> [Dool] -> [Dool]
(&&:) = zipWith (&&.)
(||:) = zipWith (||.)
(=>:) = zipWith (=>.)

(==.), (/=.) :: Dool -> Dool -> Dool
a ==. b = if a == b then true else false - abs (a-b)
a /=. b = nt (a ==. b)

(==:), (/=:) :: [Dool] -> [Dool] -> [Dool]
(==:) = zipWith (==.)
(/=:) = zipWith (/=.)

(<=.), (<.), (>.), (>=.) :: Double -> Double -> Dool
a <=. b = if a <= b then true + b-a else false - a-b
a <. b = nt (a >=. b)
a >. b = b <. a
a >=. b = b <=. a

(<=:), (<:), (>:), (>=:) :: [Double] -> [Double] -> [Dool]
(<=:) = zipWith (<=.)
(<:) = zipWith (<.)
(>:) = zipWith (>.)
(>=:) = zipWith (>=.)
