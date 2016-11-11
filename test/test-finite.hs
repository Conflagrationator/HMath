-- Unit Testing filter
--
----------------------------------------------------------------

module Main where

----------------------------------------------------------------

import Test.HUnit

import Finite
import Finite.Internal

import Data.Ratio

----------------------------------------------------------------
-- MAIN
----------------------------------------------------------------

main = runTestTT $ TestList [
    testArithmetic,
    testReading]

----------------------------------------------------------------
-- TEST OF ARITHMETIC
----------------------------------------------------------------

testArithmetic = TestLabel "arithmetic Test" $ TestList [
    -- Addition Tests
    testAddition (1234567*^-4) (8250*^2) (8251*^2),
    testAddition (23552342*^-4) (2324*^-2) (237847*^-2),
    testAddition (1560000*^-2) (17249*^-2) (1577249*^-2),
    testAddition (156*^2) (17249*^-2) (158*^2),
    testAddition (137*^-1) (13*^-1) (150*^-1),
    -- Subtraction Tests
    testSubtraction (137*^-1) (-13*^-1) (150*^-1),
    -- Multiplication Tests
    testMultiplication (131*^-1) (225*^-2) (295*^-1),
    testMultiplication (1310*^-2) (225*^-2) (295*^-1),
    testMultiplication (13100*^-3) (22500*^-4) (29475*^-3),
    testMultiplication (15310*^0) (23*^-1) (35*^3),
    testMultiplication (100*^-2) (1004*^-2) (100*^-1),
    testMultiplication (-100*^-2) (1004*^-2) (-100*^-1),
    testMultiplication (13100*^-3) (-22500*^-4) (-29475*^-3),
    -- Division Tests
    testDivision (3000*^-3) (2000*^-3) (1500*^-3)]

testAddition am bn result = TestCase $ assertEqual
    (show am ++ " + " ++ show bn)
    result
    (am + bn)

testSubtraction am bn result = TestCase $ assertEqual
    (show am ++ " - " ++ show bn)
    result
    (am - bn)

testMultiplication am bn result = TestCase $ assertEqual
    (show am ++ " * " ++ show bn)
    result
    (am * bn)

testDivision am bn result = TestCase $ assertEqual
    (show am ++ " / " ++ show bn)
    result
    (am / bn)

----------------------------------------------------------------
-- TEST OF READING ABILITY
----------------------------------------------------------------

testReading = TestLabel "reading Test" $ TestList [
    testZero,
    testInteger 1,
    testInteger 232,
    testInteger (-1),
    testInteger (-1013)]
    --testFloating 132.3141, -- FIXME: figure out a good way to test floating reading
    --testFloating 0.1,
    --testFloating 0.001,
    --testFloating (-0.000211),
    --testFloating (-8102341.124),
    --testFloating 12341000.000]

testZero = TestCase $ assertEqual
    "zero test"
    (Finite 0 (-16))
    (0 :: Finite)

testInteger n = TestCase $ assertEqual
    "integer is not being evaluated as a finite correctly, it should be double precision"
    n
    (base (fromInteger n) `div` 10^16)

{-testFloating n = TestCase $ assertEqual
    "floating point numerator should be equal to finite's base shifted over by how many decimal places it has"
    ()
    (base (fromRational n))-}