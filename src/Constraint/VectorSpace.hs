-- | Vector Space
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Constraint.VectorSpace where

------------------------------------------------------------------------------

import Unit
import Structure
import Value.Number
import Constraint.Addable
import Constraint.Multipliable

------------------------------------------------------------------------------
-- | The Class

class (Addable a a a, Multipliable Number a a, Multipliable a Number a) => VectorSpace a where
    zeroVector :: a -- ^ The 0 vector (definition)
    negateVector :: Guard a -> Guard a -- ^ Inverse Vector

------------------------------------------------------------------------------
-- | The Operator

data Negation a where
    Negation :: (VectorSpace a) => a -> Negation a

------------------------------------------------------------------------------
-- ALL OPERATORS ARE EXPRESSIONS

instance (Value a) => Expression (Negation a) a where
    evaluate (Negation a) = negateVector (evaluate a)

-- ALL EXPRESSIONS ARE SHOWABLE

instance Show (Negation a) where
    show (Negation a) = "(-" ++ show a ++ ")"

------------------------------------------------------------------------------
-- EXTENSIONS TO ALREADY DEFINED VALUES

instance VectorSpace Number where
    zeroVector = Measure 0 unitless
    negateVector (Success (Measure n u)) = Success $ Measure (-n) u
    negateVector (Failure s) = Failure s
