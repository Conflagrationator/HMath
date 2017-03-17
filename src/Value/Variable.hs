-- | Number
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Value.Variable where

------------------------------------------------------------------------------

import Structure
import Unit
import Constraint.Addable
import Constraint.Multipliable
import Constraint.Radicalizable
import Constraint.VectorSpace
import Constraint.MeasureSpace
import Constraint.InnerProductSpace

import Data.Maybe

------------------------------------------------------------------------------
-- | Number data type

data Variable
    = Variable String
    -- TODO: make an "Unevaluated Expression" form that will encapsulate all changes

------------------------------------------------------------------------------
-- ALL NUMBERS ARE VALUES
-- ALL VALUES ARE EXPRESSIONS THAT EVALUATE TO THEMSELVES

instance Expression Variable Variable where
    evaluate a = Success a

instance Value Variable

-- ALL EXPRESSIONS MUST BE SHOWABLE

instance Show Variable where
    show (Variable s) = "[" ++ s ++ "]"

------------------------------------------------------------------------------
-- CONSTRAINT & OPERATOR IMPLEMENTATION

instance Addable Variable Variable Variable where
    add (Success (Variable a)) (Success (Variable b)) = Failure "attempting to evaluate a variable"
    add (Failure s) _ = Failure s
    add _ (Failure s) = Failure s

instance Multipliable Variable Variable Variable where
    multiply (Success (Variable a)) (Success (Variable b)) = Failure "attempting to evaluate a variable"
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

instance Radicalizable Variable Variable Variable where
    power (Success (Variable a)) (Success (Variable b)) = Failure "attempting to evaluate a variable"
    power (Failure s) _ = Failure s
    power _ (Failure s) = Failure s
    root (Success (Variable a)) (Success (Variable b)) = Failure "attempting to evaluate a variable"
    root (Failure s) _ = Failure s
    root _ (Failure s) = Failure s
    logB (Success (Variable a)) (Success (Variable b)) = Failure "attempting to evaluate a variable"
    logB (Failure s) _ = Failure s
    logB _ (Failure s) = Failure s
