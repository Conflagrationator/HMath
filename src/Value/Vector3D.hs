-- | 3D Vectors
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Value.Vector3D where

------------------------------------------------------------------------------

import Structure
import Unit
import Value.Number
import Constraint.Addable
import Constraint.Multipliable
import Constraint.VectorSpace
import Constraint.MeasureSpace
import Constraint.InnerProductSpace

import Data.List
import Data.Maybe

------------------------------------------------------------------------------
-- | Vector data type

data Vector3D = Vector3D Double Double Double Unit

------------------------------------------------------------------------------
-- ALL NUMBERS ARE VALUES
-- ALL VALUES ARE EXPRESSIONS THAT EVALUATE TO THEMSELVES

instance Expression Vector3D Vector3D where
    evaluate a = Success a

instance Value Vector3D

-- ALL EXPRESSIONS MUST BE SHOWABLE

instance Show Vector3D where
    show (Vector3D a b c u) = "[" ++ intercalate ", " (map show [a, b, c]) ++ "]" ++ show u

------------------------------------------------------------------------------
-- CONSTRAINT & OPERATOR IMPLEMENTATION

-- | Vector addition
instance Addable Vector3D Vector3D Vector3D where
    add (Success (Vector3D a b c u)) (Success (Vector3D d e f v))
        | isNothing (addUnits u v) = Failure "tried to add vectors with two different units"
        | otherwise = Success $ Vector3D (a + d) (b + e) (c + f) (fromJust (addUnits u v))
    add (Failure s) _ = Failure s
    add _ (Failure s) = Failure s

-- | Vector Scalar Multiplication
instance Multipliable Number Vector3D Vector3D where
    multiply (Success (Measure a u)) (Success (Vector3D d e f v)) = Success $ Vector3D (a * d) (a * e) (a * f) (multiplyUnits u v)
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

-- | Vector Scalar Multiplication (commutative)
instance Multipliable Vector3D Number Vector3D where
    multiply (Success (Vector3D d e f v)) (Success (Measure a u)) = Success $ Vector3D (a * d) (a * e) (a * f) (multiplyUnits u v)
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s
    
-- | Vector Cross Product
instance Multipliable Vector3D Vector3D Vector3D where
    multiply (Success (Vector3D a b c u)) (Success (Vector3D d e f v)) = Success $ Vector3D (a * d) (b * e) (c * f) (multiplyUnits u v)
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

-- | Vector3D is a Vector Space
instance VectorSpace Vector3D where
    zeroVector = Vector3D 0 0 0 unitless
    negateVector (Success (Vector3D a b c u)) = Success $ Vector3D (-a) (-b) (-c) u
    negateVector (Failure s) = Failure s

-- | Vector Magnitude
instance MeasureSpace Vector3D where
    magnitude (Success (Vector3D a b c u)) = Success $ Measure (a^2 + b^2 + c^2) u
    magnitude (Failure s) = Failure s

-- | Dot Product
instance InnerProductSpace Vector3D where
    innerProduct (Success (Vector3D a b c u)) (Success (Vector3D d e f v)) = Success $ Measure (a*d + b*e + c*f) (multiplyUnits u v)
    innerProduct (Failure s) _ = Failure s
    innerProduct _ (Failure s) = Failure s
