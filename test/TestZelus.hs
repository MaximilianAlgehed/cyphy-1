{-# LANGUAGE ScopedTypeVariables #-}
module TestZelus where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Poly

import Zelus

tests :: TestTree
tests = testGroup "Zelus"
  [ testProperty "Pre followed by override equals delay" prop_PreFbyDelay
  , testProperty "Up and down not true at the same time" prop_notUpAndDown
  , testProperty "Advance and delay a stream equals the original" prop_nextPre
  , testProperty "Derivative of linear function is constant" prop_linearDeriv
  , testProperty "Derivative of square function is increasing" prop_squareDeriv
  ]

prop_PreFbyDelay xs (ys :: S A) =
  not (null xs) && not (null ys) ==>
    (xs |> pre ys) == (xs |-> ys)

prop_notUpAndDown :: S Int -> Bool
prop_notUpAndDown x = not . or $ (up x) &&? (down x)

prop_nextPre :: S Int -> Property
prop_nextPre x = not (null x) ==> and $ next (pre x) ==? x

prop_linearDeriv :: Int -> Positive Int -> Bool
prop_linearDeriv dx (Positive len) = and $ dxs ==? next dxs
  where
    dxs = deriv $ map (*dx) [(-len)..len]

prop_squareDeriv :: Positive Int -> Positive Int -> Bool
prop_squareDeriv (Positive dx) (Positive len) = and $ dxs <? next dxs
  where
    dxs = deriv $ [x*x | x <- [(-len*dx), dx .. len*dx]]
