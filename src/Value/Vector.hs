-- | Vectors
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Value.Vector where -- TODO: block export of Vec & manipulation operations

------------------------------------------------------------------------------

import Structure
import Unit
import Value.Number
import Constraint.Addable
import Constraint.Multipliable
import Constraint.Radicalizable
import Constraint.VectorSpace
import Constraint.MeasureSpace
import Constraint.InnerProductSpace

import GHC.TypeLits
import CLaSH.Sized.Vector as V
import Prelude as P
import Data.List

------------------------------------------------------------------------------
-- | Vector data type

data Vector (n :: Nat) r where
    Vector :: (Expression e r, VectorSpace r) => Vec n e -> Vector n r

------------------------------------------------------------------------------
-- ALL VECTORS ARE VALUES
-- ALL VALUES ARE EXPRESSIONS THAT EVALUATE TO THEMSELVES

instance Expression (Vector n r) (Vector n r) where
    evaluate a = Success a

instance Value (Vector n r)

-- ALL EXPRESSIONS MUST BE SHOWABLE

instance Show (Vector n r) where
    show (Vector vec) = "(:" P.++ showVecInternals vec P.++ ":)"
      where
        showVecInternals v = intercalate ", " (P.map show (toList v))

------------------------------------------------------------------------------
-- CONSTRAINT & OPERATOR IMPLEMENTATION

-- | Vector Addition
instance Addable (Vector n r) (Vector n r) (Vector n r) where
    add (Success (Vector a)) (Success (Vector b)) = if hasSucceeded v then Success (Vector (fromSuccess v)) else Failure (failureMessage v)
      where
        v = unwrapGuardedVec $ V.zipWith add (V.map evaluate a) (V.map evaluate b)
    add (Failure s) _ = Failure s
    add _ (Failure s) = Failure s

-- | Vector Scalar Multiplication
instance Multipliable Number (Vector n r) (Vector n r) where
    multiply au@(Success (Measure a u)) (Success (Vector b)) = if hasSucceeded v then Success (Vector (fromSuccess v)) else Failure (failureMessage v)
      where
        v = unwrapGuardedVec $ V.map (multiply au) (V.map evaluate b)
    multiply (Failure s) _ = Failure s
    multiply _ (Failure s) = Failure s

-- | Vector Scalar Multiplication
instance Multipliable (Vector n r) Number (Vector n r) where
    multiply a b = multiply b a -- link to other one

-- | Vector Is a VectorSpace (obviously)
instance (KnownNat n, VectorSpace r) => VectorSpace (Vector n r) where
    zeroVector = Vector (V.repeat zeroVector)
    negateVector (Success (Vector a)) = if hasSucceeded v then Success (Vector (fromSuccess v)) else Failure (failureMessage v)
      where
          v = unwrapGuardedVec $ V.map negateVector (V.map evaluate a)
    negateVector (Failure s) = Failure s

-- SPECIFIC VECTOR TYPE INSTANCES

instance (KnownNat n) => MeasureSpace (Vector n Number) where
    magnitude (Success (Vector a)) = root (Success (Measure 2 unitless)) $ V.foldr add (Success zeroVector) $ V.map (\q -> power q (Success (Measure 2 unitless))) (V.map evaluate a)
    magnitude (Failure s) = Failure s

instance (KnownNat n) => InnerProductSpace (Vector n Number) where
    innerProduct (Success (Vector a)) (Success (Vector b)) = V.foldr add (Success zeroVector) $ V.zipWith multiply (V.map evaluate a) (V.map evaluate b)
    innerProduct (Failure s) _ = Failure s
    innerProduct _ (Failure s) = Failure s

-- SPECIFIC VECTOR TYPE OPERATORS

data CrossProduct r where
    CrossProduct :: (Multipliable r r r) => Vector 3 r -> Vector 3 r -> CrossProduct r

-- all operators are expressions
instance Expression (CrossProduct r) (Vector 3 r) where
    evaluate (CrossProduct (Vector a) (Vector b)) = if hasSucceeded v then Success $ Vector (fromSuccess v) else Failure (failureMessage v)
      where
        v = unwrapGuardedVec (x :> y :> z :> Nil)
        x = add (multiply ay bz) (negateVector (multiply az by))
        y = add (multiply az bx) (negateVector (multiply ax bz))
        z = add (multiply ax by) (negateVector (multiply ay bx))
        ax = evaluate $ a V.!! 0
        ay = evaluate $ a V.!! 1
        az = evaluate $ a V.!! 2
        bx = evaluate $ b V.!! 0
        by = evaluate $ b V.!! 1
        bz = evaluate $ b V.!! 2

-- all expressions are showable
instance Show (CrossProduct r) where
    show (CrossProduct a b) = "(" P.++ show a P.++ "×" P.++ show b P.++ ")"

--------------------------------------------------------------------------------
-- UTILITY FUNCTIONS

unwrapGuardedVec :: Vec n (Guard a) -> Guard (Vec n a) -- FIXME: find a better place to put this so Matrix doesn't have to import Vector, unless I'll use it there
unwrapGuardedVec vec = if all hasSucceeded vecAsList then Success (V.map fromSuccess vec) else Failure "not all components of vector were able to be evaluated"
  where
    vecAsList = toList vec
     -- TODO: pass along internal error message
