-- | Matrices
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Value.Matrix (Matrix (), matrix) where

------------------------------------------------------------------------------

import Structure
import Value.Number
import Constraint.Addable
import Constraint.Multipliable
import Constraint.VectorSpace

import Data.List
import Data.Maybe

------------------------------------------------------------------------------
-- | Matrix data type

data Matrix r where
    Matrix :: (Expression e r, Addable r r r, Multipliable Number r r, Multipliable r Number r, Multipliable r r r, VectorSpace r) => (Int, Int) -> [[e]] -> Matrix r

matrix :: (Expression e r, Addable r r r, Multipliable Number r r, Multipliable r Number r, Multipliable r r r, VectorSpace r) => (Int, Int) -> [[e]] -> Guard (Matrix r)
matrix size list
    | sizeIsValid = Success $ Matrix size list
    | otherwise = Failure "Matrix dimensions do not agree"
  where
    sizeIsValid = length list == fst size && all (== snd size) (map length list)

rows :: Matrix r -> Int
rows (Matrix s _) = fst s

cols :: Matrix r -> Int
cols (Matrix s _) = snd s

-- getRow :: Expression e r => Int -> Matrix r -> Maybe [e]
-- getRow n m@(Matrix s a) = if 0 <= n && n <= rows m then Just (a !! n) else Nothing
-- 
-- getCol :: Expression e r => Int -> Matrix r -> Maybe [e]
-- getCol n m@(Matrix s a) = if 0 <= n && n <= cols m then Just (map (!! n) a) else Nothing

-- dot :: (Value r, VectorSpace r) => [r] -> [r] -> Guard r
-- dot a b = foldr (Success zeroVector) add (zipWith multiply (map evaluate a) (map evaluate b))

------------------------------------------------------------------------------
-- ALL MATRICES ARE VALUES
-- ALL VALUES ARE EXPRESSIONS THAT EVALUATE TO THEMSELVES

instance Expression (Matrix r) (Matrix r) where
    evaluate a = Success a

instance Value (Matrix r)

-- ALL EXPRESSIONS MUST BE SHOWABLE

instance Show (Matrix r) where
    show (Matrix _ l) = show l

------------------------------------------------------------------------------
-- CONSTRAINT & OPERATOR IMPLEMENTATION

-- | Vector addition
instance Addable (Matrix r) (Matrix r) (Matrix r) where
    add (Success (Matrix s a)) (Success (Matrix z b))
        | s == z && all (all hasSucceeded) newGuarded = Success $ Matrix s new
        | not $ all (all hasSucceeded) newGuarded = Failure "not all the sub-operations evaluated successfully"
        | otherwise = Failure "trying to add matrices with non-matching dimensions"
      where
        newGuarded = zipWith (\ra rb -> zipWith add (map evaluate ra) (map evaluate rb)) a b
        new = map (\row -> map fromSuccess row) newGuarded
    add (Failure s) _ = Failure s
    add _ (Failure s) = Failure s

-- | Matrix Scalar Multiplication
instance Multipliable Number (Matrix r) (Matrix r) where
    multiply (Success au@(Measure a u)) (Success (Matrix z b)) = Success $ Matrix z new
      where
        new = map (\row -> map (\item -> multiply (evaluate au) (evaluate item)) row) b
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

-- | Matrix Scalar Multiplication (commutative)
instance Multipliable (Matrix r) Number (Matrix r) where
    multiply (Success (Matrix s a)) (Success bv@(Measure b v)) = Success $ Matrix s new
      where
        new = map (\row -> map (\item -> multiply (evaluate item) (evaluate bv)) row) a
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s


-- | Matrix Multiplication
instance Multipliable (Matrix r) (Matrix r) (Matrix r) where
    multiply (Success ma@(Matrix s a)) (Success mb@(Matrix z b))
        | cols ma == rows mb && all (all hasSucceeded) newGuarded = Success $ Matrix (rows ma, cols mb) new
        | otherwise = Failure "trying to multiply matrices with non-matching dimensions (m⨯n * n⨯k = m⨯k)"
      where
        newGuarded = [[foldr add (Success zeroVector) (zipWith multiply (map evaluate ar) (map evaluate bc)) | bc <- transpose b] | ar <- a]
        new = map (\row -> map fromSuccess row) newGuarded
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

-- instance VectorSpace (Matrix r) where -- TODO: encode size as part of type (see: "Type Arithmetic")
--     zeroVector = 

{-
a = Matrix (2, 2) [[Measure 1 unitless, Measure 2 unitless], [Measure 3 unitless, Measure 4 unitless]]
b = Matrix (2, 2) [[Measure 2 unitless, Measure 2 unitless], [Measure 8 unitless, Measure 8 unitless]]
-}
