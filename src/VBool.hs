module VBool where

import Test.QuickCheck

newtype VBool = VBool { val :: Double }
 deriving ( Eq, Ord )

instance Show VBool where
  show = show . val

false, true :: VBool
false = VBool (-1)
true  = VBool 1

vbFromBool :: Bool -> VBool
vbFromBool b = if b then true else false

vbFromDouble :: Double -> VBool
vbFromDouble = VBool

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

-- Changed definition so zero is counted as true
isTrue :: VBool -> Bool
isTrue = (>=0) . val

trueness :: VBool -> Double
trueness = val
