-- | Number
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Value.Number where

------------------------------------------------------------------------------

import Structure
import Unit
import Constraint.Addable
import Constraint.Multipliable
import Constraint.Radicalizable

import Data.Maybe

------------------------------------------------------------------------------
-- | Number data type

data Number
    = Measure Double Unit

------------------------------------------------------------------------------
-- ALL NUMBERS ARE VALUES
-- ALL VALUES ARE EXPRESSIONS THAT EVALUATE TO THEMSELVES

instance Expression Number Number where
    evaluate a = Success a

instance Value Number

-- ALL EXPRESSIONS MUST BE SHOWABLE

instance Show Number where
    show (Measure a u) = show a ++ show u

------------------------------------------------------------------------------
-- CONSTRAINT & OPERATOR IMPLEMENTATION

instance Addable Number Number Number where
    add (Success (Measure a u)) (Success (Measure b v)) = if isJust (addUnits u v) then Success $ Measure (a + b) (fromJust (addUnits u v)) else Failure $ "tried to add numbers with two different units"
    add (Failure s) _ = Failure s
    add _ (Failure s) = Failure s

instance Multipliable Number Number Number where
    multiply (Success (Measure a u)) (Success (Measure b v)) = Success $ Measure (a * b) (multiplyUnits u v) -- cannot fail
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

instance Radicalizable Number Number Number where
    power (Success (Measure a u)) (Success (Measure b v))
        | v /= unitless = Failure "Cannot raise to a value with a unit"
        | isNothing q = Failure $ "unit could not be evaluated to an integer (" ++ show u ++ "^" ++ show b ++ " does not make sense)"
        | otherwise = Success $ Measure (a**b) (fromJust q)
      where
        q = raiseUnit u (toRational b)
    power (Failure s) _ = Failure s
    power _ (Failure s) = Failure s
    
    root (Success (Measure b v)) (Success (Measure c w))
        | v /= unitless = Failure "Cannot have a unit in the radical"
        | isNothing q = Failure $ "unit could not be evaluated to an integer (" ++ show b ++ " root " ++ show w ++ " does not make sense)"
        | otherwise = Success $ Measure (c**(1/b)) (fromJust q)
      where
        q = raiseUnit w (1/(toRational b))
    root (Failure s) _ = Failure s
    root _ (Failure s) = Failure s
    
    -- NOTE: if writing this by hand, remember the arguments are backwards from conventional notation
    logB (Success cw@(Measure c w)) (Success au@(Measure a u))
        | b == fromIntegral (round b) && q == Just w = Success $ Measure b unitless
        | otherwise = Failure $ "units do not match in logarithm. the units of " ++ show cw ++ " must be exactly " ++ show u ++ "^" ++ show b ++ " in order to be make sense"
      where
        b = logBase a c
        q = raiseUnit u (toRational b)
    logB (Failure s) _ = Failure s
    logB _ (Failure s) = Failure s
