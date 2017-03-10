-- Testing
----------------------------------------------------------------

module Finite.Tests (finitePropertyTests, finiteCaseTests) where

----------------------------------------------------------------

import Finite

import Test.Tasty
import Test.Tasty.QuickCheck -- for property testing
import Test.Tasty.HUnit -- for case testing

----------------------------------------------------------------
-- TESTS
----------------------------------------------------------------

finitePropertyTests :: TestTree
finitePropertyTests = testGroup "Finite Property Tests" [
    testProperty "Addition Commutativity: a+b == b+a" addition_commutativity,
    testProperty "Addition Associativity: (a+b)+c == a+(b+c)" addition_associativity,
    testProperty "Multiplication Commutativity: a*b == b*a" multiplication_commutativity,
    testProperty "Multiplication Associativity: (a*b)*c == a*(b*c)" multiplication_associativity,
    testProperty "Division Inverse: a*b/b == a" (\am bn -> bn /= 0 ==> division_inverse am bn),
    testProperty "Division Reciprocal: a/b == 1/(b/a)" (\am bn -> am /= 0 && bn /= 0 ==> division_reciprocal am bn)]

finiteCaseTests :: TestTree
finiteCaseTests = testGroup "Finite Case Tests" []

----------------------------------------------------------------
-- GENERATORS
----------------------------------------------------------------

instance Arbitrary Finite where
    arbitrary = do
        a <- arbitrary
        m <- arbitrary
        return $ Finite a m

----------------------------------------------------------------
-- PROPERTIES
----------------------------------------------------------------

-- TEST ADDITION

addition_commutativity :: Finite -> Finite -> Bool
addition_commutativity am bn = am + bn == bn + am

addition_associativity :: Finite -> Finite -> Finite -> Bool
addition_associativity am bn co = (am + bn) + co == am + (bn + co)

-- TEST MULTIPLICATION

multiplication_commutativity :: Finite -> Finite -> Bool
multiplication_commutativity am bn = am * bn == bn * am

multiplication_associativity :: Finite -> Finite -> Finite -> Bool
multiplication_associativity am bn co = (am * bn) * co == am * (bn * co)

-- TEST DIVISION

division_inverse :: Finite -> Finite -> Bool
division_inverse am bn = (am * bn) / bn == am

division_reciprocal :: Finite -> Finite -> Bool
division_reciprocal am bn = am / bn == 1 / (bn / am)

-- TEST POWER



-- TEST ROOT



-- TEST LOG

----------------------------------------------------------------
-- CASES
----------------------------------------------------------------

-- CREATION

-- creation :: TestTree
-- creation = testGroup "Finite Creation" [
--     testCase "" $ 5*^5 == 50*^4 @?= True ]
