module VBool where

infixr 1 =>. , =>:
infixr 2 ||. , ||:
infixr 3 &&. , &&:
infix 4 <=. , <. , >. , >=. , ==. , /=.
infix 4 <=: , <: , >: , >=: , ==: , /=:

newtype VBool = VBool { truthness :: Double }
 deriving ( Eq, Ord )

instance Show VBool where
  show = ("B:"++) . show . truthness



false, true :: VBool
false = VBool (-1)
true  = VBool 1

boolify :: VBool -> Bool
boolify = (>=0) . truthness



nt :: VBool -> VBool
nt (VBool d) = VBool (-d)

(&&.), (||.), (=>.) :: VBool -> VBool -> VBool
VBool a &&. VBool b = VBool (a `min` b)
VBool a ||. VBool b = VBool (a `max` b)
p =>.  q = nt p ||. q

(<=.), (<.), (>.), (>=.), (==.), (/=.) :: Double -> Double -> VBool
a <=. b | a <= b    = VBool (1 + b-a)
        | otherwise = VBool (-1 - a-b)
a <.  b = nt (a >=. b)
a >=. b = b <=. a
a >.  b = b <. a
a ==. b | a == b    = VBool 1
        | otherwise = VBool (-1 - abs (a-b))
a /=. b = nt (a ==. b)



nts :: [VBool] -> [VBool]
nts = map nt

(&&:), (||:), (=>:) :: [VBool] -> [VBool] -> [VBool]
(&&:) = zipWith (&&.)
(||:) = zipWith (||.)
(=>:) = zipWith (=>.)

(<=:), (<:), (>:), (>=:), (==:), (/=:) :: [Double] -> [Double] -> [VBool]
(<=:) = zipWith (<=.)
(<:) = zipWith (<.)
(>:) = zipWith (>.)
(>=:) = zipWith (>=.)
(==:) = zipWith (==.)
(/=:) = zipWith (/=.)
