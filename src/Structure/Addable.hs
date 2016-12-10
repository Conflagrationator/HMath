-- Vector Support
----------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Structure.Addable where

----------------------------------------------------------------

import Structure

import Data.Typeable
import Data.Maybe
import Data.List

----------------------------------------------------------------
-- CLASS
----------------------------------------------------------------

class (Structure s) => Addable s where
    add :: s -> s -> s

----------------------------------------------------------------
-- EXPRESSION DEFINITION
----------------------------------------------------------------

instance Addable Expression where
    add a b = Function addition [a, b]

-- OPERATOR

addition = Operator showAddition evaluateAddition
  where
    showAddition es = intercalate "+" (map show es)
    evaluateAddition [a] = a
    -- FIXME: a & b must be same type and Addable
    --evaluateAddition es@((Value a u) : ((Value b v) : rest)) = if typeOf a == typeOf b then evaluateAddition ((Value (add a (fromJust (cast b))) (addUnits u v)) : rest) else returnAsWas es
    evaluateAddition es = returnAsWas es
    returnAsWas es = Function addition es

----------------------------------------------------------------
-- UNITS
----------------------------------------------------------------

-- | Addition of Units
addUnits :: Unit -> Unit -> Unit
addUnits u v = if u == v then u else error $ "units " ++ show u ++ " and " ++ show v ++ " cannot be added"
