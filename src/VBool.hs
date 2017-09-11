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

neg :: VBool -> VBool
neg (VBool d) = VBool (-d)

(#) :: Double -> VBool -> VBool
a # (VBool d) | a >= 1 = VBool (a*d)

(&&.), (||.), (=>.), (<=>.) :: VBool -> VBool -> VBool
VBool a &&. VBool b = VBool (a `min` b)
VBool a ||. VBool b = VBool (a `max` b)

(&&+), (||+) :: VBool -> VBool -> VBool
VBool a &&+ VBool b
  | a > 0 && b > 0 = VBool (a+b)
  | a < 0 && b < 0 = VBool (a+b)
  | otherwise      = VBool (a `min` b)

x ||+ y = neg (neg x &&+ neg y)

p =>.  q = neg p ||+ q -- if trueness p > 0 then q else neg p
p <=>. q = (p =>. q) &&. (q =>. p) -- does this even make sense?

(<=.), (<.), (>.), (>=.), (==.), (/=.) :: Double -> Double -> VBool
a <=. b | a <= b    = VBool (1 + b-a)
        | otherwise = VBool (-1 - (a-b))
a <.  b = neg (a >=. b)
a >=. b = b <=. a
a >.  b = b <. a
a ==. b = (a >=. b) &&. (b >=. a)
a /=. b = (a >. b) ||. (b >. a)

isTrue :: VBool -> Bool
isTrue (VBool d) = d > 0

trueness :: VBool -> Double
trueness (VBool d) = d

--------------------------------------------------------------------------------

data Severe a = Severe a VBool
 deriving ( Eq, Ord, Show )

class Severity a where
  severity :: a -> VBool

instance Severity (Severe a) where
  severity (Severe _ v) = v

severe :: Severity a => a -> Severe a
severe x = Severe x (severity x)

instance (Arbitrary a, Severity a) => Arbitrary (Severe a) where
  arbitrary =
    do x <- arbitrary
       return (severe x)
  
  shrink (Severe x v) =
    [ Severe x' v'
    | x' <- shrink x
    , let v' = severity x'
    , trueness v' <= trueness v
    ]

instance Testable (Severe a) where
  property (Severe _ v) = property (trueness v > 0)

