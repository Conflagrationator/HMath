-- | Inner Product Space
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Constraint.InnerProductSpace where

------------------------------------------------------------------------------

import Structure
import Value.Number
import Constraint.MeasureSpace
import Constraint.Multipliable -- for Number instance implementation

------------------------------------------------------------------------------
-- | The Class

class (MeasureSpace a) => InnerProductSpace a where
    innerProduct :: Guard a -> Guard a -> Guard Number

------------------------------------------------------------------------------
-- | The Operator

data InnerProduct a where
    InnerProduct :: (InnerProductSpace a) => a -> a -> InnerProduct a

------------------------------------------------------------------------------
-- ALL OPERATORS ARE EXPRESSIONS

instance (Value a) => Expression (InnerProduct a) Number where
    evaluate (InnerProduct a b) = innerProduct (evaluate a) (evaluate b)

-- ALL EXPRESSIONS ARE SHOWABLE

instance Show (InnerProduct a) where
    show (InnerProduct a b) = "⟨" ++ show a ++ ", " ++ show b ++ "⟩"

------------------------------------------------------------------------------
-- EXTENSIONS TO ALREADY DEFINED VALUES

instance InnerProductSpace Number where
    innerProduct a b = multiply a b
