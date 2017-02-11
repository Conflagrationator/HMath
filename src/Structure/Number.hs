-- | Number
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Structure.Number where

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
    = Absolute Integer
    | Measure Double Unit

------------------------------------------------------------------------------
-- ALL NUMBERS ARE STRUCTURES
-- ALL STRUCTURES ARE EXPRESSIONS THAT EVALUATE TO THEMSELVES

instance Expression Number Number where
    evaluate a = Success a

instance Structure Number

-- ALL EXPRESSIONS MUST BE SHOWABLE

instance Show Number where
    show (Absolute a) = "[" ++ show a ++ "]"
    show (Measure a u) = "[" ++ show a ++ show u ++ "]"

------------------------------------------------------------------------------
-- CONSTRAINT & OPERATOR IMPLEMENTATION

instance Addable Number Number Number where
    add (Success (Absolute a)) (Success (Absolute b)) = Success $ Absolute (a + b)
    add (Success (Measure a u)) (Success (Absolute b)) = Success $ Measure (a + fromIntegral b) u
    add (Success (Absolute a)) (Success (Measure b v)) = Success $ Measure (fromIntegral a + b) v
    add (Success (Measure a u)) (Success (Measure b v)) = if isJust (addUnits u v) then Success $ Measure (a * b) (fromJust (addUnits u v)) else Failure $ "tried to add numbers with two different units"
    add (Failure s) _ = Failure s
    add _ (Failure s) = Failure s

instance Multipliable Number Number Number where
    multiply (Success (Absolute a)) (Success (Absolute b)) = Success $ Absolute (a * b)
    multiply (Success (Measure a u)) (Success (Absolute b)) = Success $ Measure (a * fromIntegral b) u
    multiply (Success (Absolute a)) (Success (Measure b v)) = Success $ Measure (fromIntegral a * b) v
    multiply (Success (Measure a u)) (Success (Measure b v)) = Success $ Measure (a * b) (multiplyUnits u v) -- cannot fail
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

instance Powerable Number Number Number where
    power (Success (Absolute a)) (Success (Absolute b)) = if b > 0 then Success (Absolute (a ^ b)) else Failure "Non integer power not built in yet"
    -- TODO: supplement this function with functioning power cases for 
    power (Failure s) _ = Failure s
    power _ (Failure s) = Failure s
    power (Success a) (Success b) = Failure $ "power function not defined for the inputs " ++ show a ++ " & " ++ show b
