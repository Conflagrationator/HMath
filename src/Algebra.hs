-- | The file that has the functions which manipulate the
--   abstract syntax tree
----------------------------------------------------------------

module Algebra where

----------------------------------------------------------------

import Structure

----------------------------------------------------------------
-- EXPRESSION CONSTRUCTORS
----------------------------------------------------------------

-- | Addition
addEX :: [Expression] -> Expression
addEX [a, b] = Operator "+" (addEV [a, b])
addEX es = error $ "\"+\" operator requires 2 operands, you supplied " ++ show (length es)

----------------------------------------------------------------
-- EXPRESSION EVALUATORS
----------------------------------------------------------------

-- | Addition
addEV :: Evaluator
addEV [Value (Real a) u, Value (Real b) v] = Value (Real (a + b)) (addUnits u v)
addEV _ = Undefined

----------------------------------------------------------------
-- UNIT HANDLING
----------------------------------------------------------------

addUnits :: [Unit] -> Unit
addUnits [u, v] = if u == v then u else error $ "units " ++ show u ++ " and " ++ show v ++ " cannot be added"

multiplyUnits :: [Unit] -> Unit
