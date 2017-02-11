-- | Vector Space
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Constraint.VectorSpace where

------------------------------------------------------------------------------

import Structure
import Structure.Number
import Constraint.Addable
import Constraint.Multipliable

------------------------------------------------------------------------------
-- | The Class

class (Addable a a a, Multipliable Number a a) => VectorSpace a where
    zeroVector :: a -- ^ The 0 vector (definition)
    negateVector :: Guard a -> Guard a -- ^ Inverse Vector

------------------------------------------------------------------------------
-- | The Operator

data Negation a where
    Negation :: (VectorSpace a) => a -> Negation a

------------------------------------------------------------------------------
-- ALL OPERATORS ARE EXPRESSIONS

instance (Structure a) => Expression (Negation a) a where
    evaluate (Negation a) = negateVector (evaluate a)

-- ALL EXPRESSIONS ARE SHOWABLE

instance Show (Negation a) where
    show (Negation a) = "(-" ++ show a ++ ")"

------------------------------------------------------------------------------
-- EXTENSIONS TO ALREADY DEFINED STRUCTURES

instance VectorSpace Number where
    zeroVector = Absolute 0
    negateVector (Success (Absolute n)) = Success $ Absolute (-n)
    negateVector (Success (Measure n u)) = Success $ Measure (-n) u
    negateVector (Failure s) = Failure s
