-- | Addable
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Constraint.Addable where

------------------------------------------------------------------------------

import Structure

------------------------------------------------------------------------------
-- | The Class

class (Structure s) => Addable s where
    add :: s -> s -> s

------------------------------------------------------------------------------
-- | The Operator

data Add s where
    Add :: (Addable s) => s -> s -> Add s

instance Show (Add s) where
    show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"

instance (Addable s) => Expression (Add s) s where
    evaluate (Add a b) = add a b
