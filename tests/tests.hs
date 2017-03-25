-- Testing
----------------------------------------------------------------

module Tests where

----------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck -- for property testing
import Test.Tasty.HUnit -- for case testing

import Finite.Tests

----------------------------------------------------------------
-- MAIN
----------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests" [
    finitePropertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
    finiteCaseTests]
