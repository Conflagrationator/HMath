-- | Number
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Value.Number where

------------------------------------------------------------------------------

import Structure
import Unit
import Constraint.Addable
import Constraint.Multipliable
import Constraint.Powerable

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
    show (Measure a u) = "[" ++ show a ++ show u ++ "]"

------------------------------------------------------------------------------
-- CONSTRAINT & OPERATOR IMPLEMENTATION

instance Addable Number Number Number where
    add (Success (Measure a u)) (Success (Measure b v)) = if isJust (addUnits u v) then Success $ Measure (a * b) (fromJust (addUnits u v)) else Failure $ "tried to add numbers with two different units"
    add (Failure s) _ = Failure s
    add _ (Failure s) = Failure s

instance Multipliable Number Number Number where
    multiply (Success (Measure a u)) (Success (Measure b v)) = Success $ Measure (a * b) (multiplyUnits u v) -- cannot fail
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

instance Powerable Number Number Number where
    power (Success (Measure a u)) (Success (Measure b v))
        | v /= unitless = Failure "Cannot raise to a value with a unit"
        | isNothing q = Failure $ "unit could not be exponentiated to an integer (" ++ show u ++ "^" ++ show b ++ " does not make sense)"
        | otherwise = Success $ Measure (a**b) (fromJust q)
      where
        q = raiseUnit u (toRational b)
    power (Failure s) _ = Failure s
    power _ (Failure s) = Failure s
