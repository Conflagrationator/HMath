-- | Number
------------------------------------------------------------------------------

module Structure.Number where

------------------------------------------------------------------------------

import Structure
import Constraint.Addable

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
