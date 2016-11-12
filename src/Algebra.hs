-- | The file that has the functions which manipulate the
--   abstract syntax tree
----------------------------------------------------------------

module Algebra where

----------------------------------------------------------------

import Structure
import Extensions

----------------------------------------------------------------
-- EXPRESSION CONSTRUCTORS
----------------------------------------------------------------

-- | Addition
addEX :: [Expression] -> Expression
addEX [a, b] = Operator "+" addEV [a, b]
addEX es = error $ "\"+\" operator requires 2 operands, you supplied " ++ show (length es)

----------------------------------------------------------------
-- EXPRESSION EVALUATORS
----------------------------------------------------------------

-- | Addition definitions for various Value types
addEV :: Evaluator
addEV [Value (Real a) u, Value (Real b) v] = Value (Real (a + b)) (addUnits u v)
addEV _ = undefined

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
