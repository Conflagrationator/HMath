-- | Addable
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Constraint.Addable where

------------------------------------------------------------------------------

import Structure

------------------------------------------------------------------------------
-- | The Class

class (Value a, Value b, Value c) => Addable a b c | a b -> c where
    add :: Guard a -> Guard b -> Guard c
    -- the return type of add is known

------------------------------------------------------------------------------
-- | The Operator

data Addition a b c where
    Addition :: (Expression a n, Expression b m, Addable n m c) => a -> b -> Addition a b c
    -- a & b determine n & m which determine c

------------------------------------------------------------------------------
-- ALL OPERATORS ARE EXPRESSIONS

instance (Value c) => Expression (Addition a b c) c where
    evaluate (Addition a b) = add (evaluate a) (evaluate b)

-- ALL EXPRESSIONS ARE SHOWABLE

instance Show (Addition a b c) where
    show (Addition a b) = "(" ++ show a ++ "+" ++ show b ++ ")"

------------------------------------------------------------------------------
-- EXTENSIONS TO ALREADY DEFINED VALUES
