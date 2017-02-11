-- | Powerable
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Constraint.Powerable where

------------------------------------------------------------------------------

import Structure

------------------------------------------------------------------------------
-- | The Class

class (Structure a, Structure b, Structure c) => Powerable a b c | a b -> c where
    power :: Guard a -> Guard b -> Guard c
    -- the return type of add is known

------------------------------------------------------------------------------
-- | The Operator

data Power a b c where
    Power :: (Expression a n, Expression b m, Powerable n m c) => a -> b -> Power a b c
    -- a & b determine n & m which determine c

------------------------------------------------------------------------------
-- ALL OPERATORS ARE EXPRESSIONS

instance (Structure c) => Expression (Power a b c) c where
    evaluate (Power a b) = power (evaluate a) (evaluate b)

-- ALL EXPRESSIONS ARE SHOWABLE

instance Show (Power a b c) where
    show (Power a b) = "(" ++ show a ++ ")^(" ++ show b ++ ")"

------------------------------------------------------------------------------
-- EXTENSIONS TO ALREADY DEFINED STRUCTURES
