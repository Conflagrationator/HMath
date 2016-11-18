-- | The file that has the functions which manipulate the
--   abstract syntax tree
----------------------------------------------------------------

module Algebra where

----------------------------------------------------------------

import Structure
import Extensions
import Finite

import Data.Maybe

----------------------------------------------------------------
-- EXPRESSION EVALUATORS
----------------------------------------------------------------

-- | Addition Operator Definition
addition :: Evaluator
addition = ("+", addE)

addE :: [Expression] -> Expression
addE [] = error "no operands"
addE [e] = e -- unary identity operator functionality
addE [Value a u, Value b v] = if isJust (addN a b) then Value (fromJust (addN a b)) (addUnits u v) else Operator addition [Value a u, Value b v]
addE (au@(Value _ _) : bv@(Value _ _) : es) = addE ((addE [au, bv]) : es) -- associative property
addE es = Operator addition es

-- | Multiplication definitions for varous Number types
multiplication :: Evaluator
multiplication = ("*", multiplyE)

multiplyE :: [Expression] -> Expression
multiplyE [] = error "no operands"
multiplyE [e] = error "only 1 operand"
multiplyE [Value a u, Value b v] = if isJust (multiplyN a b) then Value (fromJust (multiplyN a b)) (multiplyUnits u v) else Operator multiplication [Value a u, Value b v]
multiplyE (au@(Value _ _) : bv@(Value _ _) : es) = multiplyE ((multiplyE [au, bv]) : es) -- associative property
multiplyE es = Operator multiplication es

----------------------------------------------------------------
-- NUMBER EVALUATORS
----------------------------------------------------------------

-- | Addition of Number types
addN :: Number -> Number -> Maybe Number
addN (Real a) (Real b) = Just $ Real $ a + b
addN (Real a) (Absolute b) = Just $ Real $ a + fromIntegerMatchingPrecision b a
addN (Absolute a) (Real b) = addN (Real b) (Absolute a) -- commutative property
addN (Unknown _) _ = Nothing
addN a (Unknown b) = addN (Unknown b) a -- commutative property
addN (Infinity a) (Infinity b) = Just $ if a == b then Infinity a else Undefined
addN (Infinity s) _ = Just $ Infinity s
addN a (Infinity s) = addN (Infinity s) a -- commutative property
addN (Undefined) _ = Just $ Undefined
addN a (Undefined) = addN (Undefined) a -- commutative property

-- | Multiplication of Number types
multiplyN :: Number -> Number -> Maybe Number
multiplyN (Real a) (Real b) = Just $ Real $ a * b
multiplyN (Real a) (Absolute b) = Just $ Real $ a + fromIntegerMatchingPrecision b a
multiplyN (Absolute a) (Real b) = multiplyN (Real b) (Absolute a) -- commutative property
multiplyN (Unknown _) _ = Nothing
multiplyN a (Unknown b) = multiplyN (Unknown b) a
multiplyN (Infinity a) (Infinity b) = Just $ if a == b then Infinity a else Undefined
multiplyN (Infinity s) _ = Just $ Infinity s
multiplyN a (Infinity s) = multiplyN (Infinity s) a -- commutative property
multiplyN (Undefined) _ = Just $ Undefined
multiplyN a (Undefined) = multiplyN (Undefined) a -- commutative property

----------------------------------------------------------------
-- UNIT HANDLING
----------------------------------------------------------------

-- | Addition of Units
addUnits :: Unit -> Unit -> Unit
addUnits u v = if u == v then u else error $ "units " ++ show u ++ " and " ++ show v ++ " cannot be added"

-- | Multiplication of Units
multiplyUnits :: Unit -> Unit -> Unit
multiplyUnits u v = foldUnique multiplyUnitComponents u v
  where
    multiplyUnitComponents :: (Dimension, Integer) -> (Dimension, Integer) -> Maybe (Dimension, Integer)
    multiplyUnitComponents (dim1, exp1) (dim2, exp2) = if dim1 == dim2 then Just (dim1, exp1 + exp2) else Nothing
