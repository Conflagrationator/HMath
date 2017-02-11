-- | Vector Space
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Constraint.MeasureSpace where

------------------------------------------------------------------------------

import Structure
import Structure.Number
import Constraint.VectorSpace

------------------------------------------------------------------------------
-- | The Class

class (VectorSpace a) => MeasureSpace a where
    magnitude :: Guard a -> Guard Number

------------------------------------------------------------------------------
-- | The Operator

data Magnitude a where
    Magnitude :: (MeasureSpace a) => a -> Magnitude a

------------------------------------------------------------------------------
-- ALL OPERATORS ARE EXPRESSIONS

instance (Structure a) => Expression (Magnitude a) Number where
    evaluate (Magnitude a) = magnitude (evaluate a)

-- ALL EXPRESSIONS ARE SHOWABLE

instance Show (Magnitude a) where
    show (Magnitude a) = "|" ++ show a ++ "|"

------------------------------------------------------------------------------
-- EXTENSIONS TO ALREADY DEFINED STRUCTURES

instance MeasureSpace Number where
    magnitude a = a
