module Dool where

infixr 1 =>. , =>:
infixr 2 ||. , ||:
infixr 3 &&. , &&:
infix 4 <=. , <. , >. , >=. , ==. , /=.
infix 4 <=: , <: , >: , >=: , ==: , /=:

newtype VBool a = VBool { value :: a } deriving (Show, Eq, Ord)

type Dool = VBool Double

instance Functor VBool where
  fmap f = pure . f . value

instance Applicative VBool where
  pure = VBool
  (<*>) = fmap . value

true, false :: Dool
true  = pure 1 -- how deal with rounding errors? this fails many tests
false = nt true

trues, falses :: [Dool]
trues = repeat true
falses = repeat false

isTrue, isFalse :: Dool -> Bool
isTrue = (>= true)
isFalse = (<= false)

nt :: Dool -> Dool
nt = fmap negate

nts :: [Dool] -> [Dool]
nts = fmap nt

(&&.), (||.), (=>.) :: Dool -> Dool -> Dool
(&&.) = (<*>) . fmap min
(||.) = (<*>) . fmap max
(=>.) = (<*>) . fmap (max . negate)

(&&:), (||:), (=>:) :: [Dool] -> [Dool] -> [Dool]
(&&:) = zipWith (&&.)
(||:) = zipWith (||.)
(=>:) = zipWith (=>.)

(==.), (/=.) :: Double -> Double -> Dool
a ==. b = a <=. b &&. b <=. a
a /=. b = nt (a ==. b)

(==:), (/=:) :: [Double] -> [Double] -> [Dool]
(==:) = zipWith (==.)
(/=:) = zipWith (/=.)

(<=.), (<.), (>.), (>=.) :: Double -> Double -> Dool
a <=. b = if a <= b then pure (1 + b - a) else pure (b - a - 1)
a <. b = nt (a >=. b)
a >. b = b <. a
a >=. b = b <=. a

(<=:), (<:), (>:), (>=:) :: [Double] -> [Double] -> [Dool]
(<=:) = zipWith (<=.)
(<:) = zipWith (<.)
(>:) = zipWith (>.)
(>=:) = zipWith (>=.)
