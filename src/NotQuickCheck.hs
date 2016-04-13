module NotQuickCheck
       ( module Test.QuickCheck.Arbitrary
       , module Test.QuickCheck.Gen
       , quickCheck
       ) where

import qualified Test.QuickCheck as QC
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Gen.Unsafe (promote)
import Test.QuickCheck.Property (Rose (MkRose), joinRose)
import Test.QuickCheck.Random (QCGen, mkQCGen, newQCGen)

import Data.List (intersperse)

import System.Random (split)

import VBool

-----------------------------------------------
----- Result
---

data Result = MkResult{ok :: Maybe VBool, testCase :: [String]}

-----------------------------------------------
----- Property
---

newtype Prop = MkProp{unProp :: Rose Result}

newtype Property = MkProperty{unProperty :: Gen Prop}

-----------------------------------------------
----- Testable
---

class Testable a where
  property :: a -> Property

instance Testable Bool where
  property = property . vbFromBool

instance Testable Double where
  property = property . vbFromDouble

instance Testable VBool where
  property vb = property $ MkResult{ok = Just vb, testCase = []}

instance Testable Result where
  property = property . MkProp . return

instance Testable Prop where
  property = MkProperty . return

instance Testable Property where
  property = id

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where
  property pf = forAllShrink arbitrary shrink pf

-----------------------------------------------
----- Case generation and shrinking
---

forAllShrink :: (Show a, Testable prop)
             => Gen a -> (a -> [a]) -> (a -> prop) -> Property
forAllShrink gen shrinker pf =
  MkProperty $
  gen >>= \x ->
    unProperty $
    shrinking shrinker x $ \x' ->
      counterexample (show x') (pf x')

counterexample :: Testable prop => String -> prop -> Property
counterexample s = mapTotalResult (\res -> res { testCase = s : testCase res })

shrinking :: Testable prop => (a -> [a]) -> a -> (a -> prop) -> Property
shrinking shrinker x0 pf =
    MkProperty (fmap (MkProp . joinRose . fmap unProp) (promote (props x0)))
  where
    props x =
      MkRose (unProperty (property (pf x))) [ props x' | x' <- shrinker x ]

mapTotalResult :: Testable prop => (Result -> Result) -> prop -> Property
mapTotalResult f = mapRoseResult (fmap f)

mapRoseResult :: Testable prop =>
                 (Rose Result -> Rose Result) -> prop -> Property
mapRoseResult f = mapProp (\(MkProp t) -> MkProp (f t))

mapProp :: Testable prop => (Prop -> Prop) -> prop -> Property
mapProp f = MkProperty . fmap f . unProperty . property

-----------------------------------------------
----- State
---

data State = MkState
  { maxSuccess :: Int
  , maxDiscarded :: Int
  , maxShrinks :: Int
  , maxTryShrinks :: Int
  , maxSize :: Int
  , computeSize :: Int -> Int -> Int
  , numSuccess :: Int
  , numDiscarded :: Int
  , numShrinks :: Int
  , numTryShrinks :: Int
  , randomSeed :: QCGen
  }

initState :: State
initState = MkState
  { maxSuccess = mSu
  , maxDiscarded = mDc
  , maxShrinks = mSh
  , maxTryShrinks = mTSh
  , maxSize = mSi
  , computeSize = compS
  , numSuccess = 0
  , numDiscarded = 0
  , numShrinks = 0
  , numTryShrinks = 0
  , randomSeed = mkQCGen 0
  }
  where
    mSu = 100
    mDc = 200
    mSh = 100
    mTSh = 100
    mSi = 100
    compS n d
      | (n `div` mSi) * mSi <= mSu || n >= mSu || mSu `mod` mSi == 0 =
        (n `mod` mSi + d `div` 10) `min` mSi
      | otherwise =
        ((n `mod` mSi) * mSi `div` (mSu `mod` mSi) + d `div` 10) `min` mSi

-----------------------------------------------
----- Testing
---

quickCheck :: Testable prop => prop -> IO ()
quickCheck p =
  do rnd <- newQCGen
     test initState{randomSeed = rnd} (unGen (unProperty (property p)))
     return ()

test :: State -> (QCGen -> Int -> Prop) -> IO ()
test st f
  | numSuccess st >= maxSuccess st = done st
  | numDiscarded st >= maxDiscarded st = giveUp st
  | otherwise = run st f

done :: State -> IO ()
done st = putStrLn ("+++ OK! (" ++ show (numSuccess st) ++ " passed)")

giveUp :: State -> IO ()
giveUp st = putStrLn ("*** Gave up! (" ++ show (numSuccess st) ++ " passed)")

failed :: State -> Result -> IO ()
failed st MkResult{ok = Just vb, testCase = testCase} =
  do putStrLn $ concat
       [ "--- Failed! (after " ++ show (numSuccess st+1) ++ " tests"
       , " and " ++ show (numShrinks st) ++ " shrinks"
       , " with badness " ++ show vb ++ ")"
       ]
     putStrLn $ concat (intersperse "\n" testCase)
     return ()

run :: State -> (QCGen -> Int -> Prop) -> IO ()
run st f =
  do let size = computeSize st (numSuccess st) (numDiscarded st)
     let (rnd1,rnd2) = split (randomSeed st)
     let MkRose res ts = unProp (f rnd1 size)
     -- continue?
     case res of
       MkResult{ok = Nothing} ->
         test st{numDiscarded = numDiscarded st + 1, randomSeed = rnd2} f
       MkResult{ok = Just vb, testCase = testCase} ->
         if isTrue vb
         then test st{numSuccess = numSuccess st + 1, randomSeed = rnd2} f
         else localMin st res ts

-----------------------------------------------
----- Minimize failed test case
---

localMin :: State -> Result -> [Rose Result] -> IO ()
localMin st res [] = failed st res
localMin st res (t:ts)
  | numTryShrinks st >= maxTryShrinks st = failed st res
  | otherwise =
    do let MkRose res' ts' = t
       if ok res' <= ok res       ---- <<<< THIS IS WHAT IT ALL LEADS UP TO
         then localMin st{ numShrinks = numShrinks st + 1
                         , numTryShrinks = 0 } res' ts'
         else localMin st{numTryShrinks = numTryShrinks st + 1} res ts

-----------------------------------------------
----- Example properties
---

prop_1 :: [Int] -> [Int] -> Bool
prop_1 xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

prop_2 :: Int -> Int -> Double
prop_2 n m = fromIntegral n * fromIntegral m
