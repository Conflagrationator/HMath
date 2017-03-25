-- | Matrices
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Value.Matrix where

------------------------------------------------------------------------------

import Structure
import Value.Number
import Value.Vector
import Constraint.Addable
import Constraint.Multipliable
import Constraint.VectorSpace

import GHC.TypeLits
import CLaSH.Sized.Vector as V
import Prelude as P
import Data.List

------------------------------------------------------------------------------
-- | Matrix data type

data Matrix (m :: Nat) (n :: Nat) r where
    Matrix :: (Expression e r, VectorSpace r) => Vec m (Vec n e) -> Matrix m n r

------------------------------------------------------------------------------
-- ALL MATRICES ARE VALUES
-- ALL VALUES ARE EXPRESSIONS THAT EVALUATE TO THEMSELVES

instance Expression (Matrix m n r) (Matrix m n r) where
    evaluate a = Success a

instance Value (Matrix m n r)

-- ALL EXPRESSIONS MUST BE SHOWABLE

instance Show (Matrix m n r) where
    show (Matrix rows) = "[" P.++ (intercalate ", " (P.map (\col -> "[" P.++ (intercalate ", " (P.map show (toList col))) P.++ "]") (toList rows))) P.++ "]"

------------------------------------------------------------------------------
-- CONSTRAINT & OPERATOR IMPLEMENTATION

-- | Matrix Addition
instance Addable (Matrix m n r) (Matrix m n r) (Matrix m n r) where
    add (Success (Matrix a)) (Success (Matrix b)) = if hasSucceeded c then Success (Matrix (fromSuccess c)) else Failure (failureMessage c)
      where
        c = unwrapGuardedVec $ V.map (\row -> unwrapGuardedVec row) $ v -- unwrap the Guards to the outside
        v = V.zipWith (\ra rb -> V.zipWith add (V.map evaluate ra) (V.map evaluate rb)) a b
    add (Failure s) _ = Failure s
    add _ (Failure s) = Failure s

-- | Matrix Scalar Multiplication
instance Multipliable Number (Matrix m n r) (Matrix m n r) where
    multiply (Success a) (Success (Matrix b)) = if hasSucceeded c then Success (Matrix (fromSuccess c)) else Failure (failureMessage c)
      where
        c = unwrapGuardedVec $ V.map (\row -> unwrapGuardedVec row) $ v -- unwrap the Guards to the outside
        v = V.map (\row -> V.map (multiply (Success a)) (V.map evaluate row)) b
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

-- | Matrix Scalar Multiplication
instance Multipliable (Matrix m n r) Number (Matrix m n r) where
    multiply a b = multiply b a

-- | Matrix is a Vector Space
instance (KnownNat m, KnownNat n, VectorSpace r) => VectorSpace (Matrix m n r) where
    zeroVector = Matrix $ V.repeat (V.repeat zeroVector)
    negateVector (Success (Matrix a)) = if hasSucceeded c then Success (Matrix (fromSuccess c)) else Failure (failureMessage c)
      where
        c = unwrapGuardedVec $ V.map (\row -> unwrapGuardedVec row) $ v -- unwrap the Guards to the outside
        v = V.map (\row -> V.map negateVector (V.map evaluate row)) a
    negateVector (Failure s) = Failure s

-- SPECIFIC MATRIX TYPE INSTANCES

-- | Matrix Multiplication
instance (KnownNat m, KnownNat n, KnownNat k, Multipliable r r r) => Multipliable (Matrix m n r) (Matrix n k r) (Matrix m k r) where
    multiply (Success (Matrix a)) (Success (Matrix b)) = if hasSucceeded v then Success (Matrix (fromSuccess v)) else Failure $ failureMessage v
      where
        b' = V.transpose b
        dot j k = V.foldr add (Success zeroVector) $ V.zipWith multiply (V.map evaluate j) (V.map evaluate k)
        v = unwrapGuardedVec $ V.map (\r -> unwrapGuardedVec $ V.map (dot r) b') a
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

-- SPECIFIC VECTOR TYPE OPERATORS

-- | Transpose Operator
data Transpose m n r where
    Transpose :: Matrix m n r -> Transpose m n r

-- | all operators are expressions
instance (KnownNat m, KnownNat n) => Expression (Transpose m n r) (Matrix n m r) where
    --evaluate (Transpose Nil) = Success Nil
    evaluate (Transpose (Matrix vec)) = Success $ Matrix (V.transpose vec)

-- | all expressions are showable
instance Show (Transpose m n r) where
    show (Transpose a) = "(" P.++ show a P.++ "^T)"
