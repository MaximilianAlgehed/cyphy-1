module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified TestZelus

-- | Runs the test suite
main :: IO ()
main =  defaultMain $ testGroup "Test suite" [TestZelus.tests]
