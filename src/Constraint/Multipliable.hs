-- | Multiplication
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Constraint.Multipliable where

------------------------------------------------------------------------------

import Structure

------------------------------------------------------------------------------
-- | The Class

class (Value a, Value b, Value c) => Multipliable a b c | a b -> c where
    multiply :: Guard a -> Guard b -> Guard c
    -- the return type of multiplication is known

------------------------------------------------------------------------------
-- | The Operator

data Multiplication a b c where
    Multiplication :: (Expression a n, Expression b m, Multipliable n m c) => a -> b -> Multiplication a b c
    -- a & b determine n & m which determine c

------------------------------------------------------------------------------
-- ALL OPERATORS ARE EXPRESSIONS

instance (Value c) => Expression (Multiplication a b c) c where
    evaluate (Multiplication a b) = multiply (evaluate a) (evaluate b)

-- ALL EXPRESSIONS ARE SHOWABLE

instance Show (Multiplication a b c) where
    show (Multiplication a b) = "(" ++ show a ++ "*" ++ show b ++ ")"

------------------------------------------------------------------------------
-- EXTENSIONS TO ALREADY DEFINED VALUES
