module VBool where

import Test.QuickCheck

newtype VBool = VBool Double
 deriving ( Eq, Ord )

instance Show VBool where
  show (VBool d) = show d

false, true :: VBool
false = VBool (-1)
true  = VBool 1

vbool :: Bool -> VBool
vbool b = if b then true else false

nt :: VBool -> VBool
nt (VBool d) = VBool (-d)

(#) :: Double -> VBool -> VBool
a # (VBool d) | a > 0 = VBool (a*d)

(&&.), (||.), (=>.), (<=>.) :: VBool -> VBool -> VBool
VBool a &&. VBool b = VBool (a `min` b)
VBool a ||. VBool b = VBool (a `max` b)

p =>.  q = nt p ||. q
p <=>. q = (p =>. q) &&. (q =>. p) -- does this even make sense?

(<=.), (<.), (>.), (>=.), (==.), (/=.) :: Double -> Double -> VBool
a <=. b | a <= b    = VBool (1 + b-a)
        | otherwise = VBool (-1 - a-b)
a <.  b = nt (a >=. b)
a >=. b = b <=. a
a >.  b = b <. a
a ==. b | a == b    = VBool 1
        | otherwise = VBool (-1 - abs (a-b))
a /=. b = nt (a ==. b)


isTrue :: VBool -> Bool
isTrue (VBool d) = d > 0

trueness :: VBool -> Double
trueness (VBool d) = d

