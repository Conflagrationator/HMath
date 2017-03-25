-- | Radicalizable
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Constraint.Radicalizable where

------------------------------------------------------------------------------

import Structure

------------------------------------------------------------------------------
-- | The Class

-- 2^3=8      -- a^b=c
-- 3 root 8=2 -- b root c=a
-- log_2(8)=3 -- log_a(c)=b

class (Value a, Value b, Value c) => Radicalizable a b c | a b -> c, b c -> a, c a -> b where
    power :: Guard a -> Guard b -> Guard c
    root :: Guard b -> Guard c -> Guard a
    logB :: Guard c -> Guard a -> Guard b
    -- the return type of these functions is known

------------------------------------------------------------------------------
-- | The Operator

data Power a b c where
    Power :: (Expression a n, Expression b m, Radicalizable n m c) => a -> b -> Power a b c
    -- a & b determine n & m which determine c

data Root b c a where
    Root :: (Expression b n, Expression c m, Radicalizable a n m) => b -> c -> Root b c a

data Logarithm c a b where
    Logarithm :: (Expression c n, Expression a m, Radicalizable m b n) => c -> a -> Logarithm c a b

------------------------------------------------------------------------------
-- ALL OPERATORS ARE EXPRESSIONS

instance (Value c) => Expression (Power a b c) c where
    evaluate (Power a b) = power (evaluate a) (evaluate b)

instance (Value a) => Expression (Root b c a) a where
    evaluate (Root b c) = root (evaluate b) (evaluate c)

instance (Value b) => Expression (Logarithm a c b) b where
    evaluate (Logarithm c a) = logB (evaluate c) (evaluate a)

-- ALL EXPRESSIONS ARE SHOWABLE

instance Show (Power a b c) where
    show (Power a b) = "(" ++ show a ++ ")^(" ++ show b ++ ")"

instance Show (Root b c a) where
    show (Root b c) = "(" ++ show b ++ " root " ++ show c ++ ")"

instance Show (Logarithm c a b) where
    show (Logarithm c a) = "(log" ++ show a ++ "(" ++ show c ++ ")" ++ ")"

------------------------------------------------------------------------------
-- EXTENSIONS TO ALREADY DEFINED VALUES
