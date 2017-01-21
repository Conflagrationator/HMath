-- | The Description of the Structure of Mathematical Expression Data
------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Structure where

------------------------------------------------------------------------------

import Finite
import Extensions
import Unit

------------------------------------------------------------------------------

class (Show e, Structure s) => Expression e s | e -> s where
    evaluate :: e -> s

------------------------------------------------------------------------------

class (Show s) => Structure s

------------------------------------------------------------------------------








------------------------------------------------------------------------------

class (Structure s) => Addable s where
    add :: s -> s -> s

class (Structure s) => ScalarMultipliable s where
    smult :: Number -> s -> s

class (Structure s) => Multipliable s where
    mult :: s -> s -> s

------------------------------------------------------------------------------

data Number
    = Absolute Integer
    | Complex Integer Integer

instance Show Number where
    show (Absolute a) = "[" ++ show a ++ "]"
    show (Complex a b) = "[" ++ show a ++ "+" ++ show b ++ "i]"

instance Structure Number

instance Addable Number where
    add (Absolute a) (Absolute b) = Absolute (a + b)
    add (Absolute a) (Complex b c) = Complex (a + b) c
    add (Complex a b) (Absolute c) = Complex a (b + c)
    add (Complex a b) (Complex c d) = Complex (a + b) (c + d)

------------------------------------------------------------------------------

data Add s where
    Add :: (Addable s) => s -> s -> Add s

instance Show (Add s) where
    show (Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"

instance (Addable s) => Expression (Add s) s where
    evaluate (Add a b) = add a b
