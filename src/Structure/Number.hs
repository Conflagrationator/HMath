-- | Number
------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Structure.Number where

------------------------------------------------------------------------------

import Structure
import Constraint.Addable

------------------------------------------------------------------------------
-- | Number data type

data Number = Absolute Integer

------------------------------------------------------------------------------
-- ALL NUMBERS ARE STRUCTURES
-- ALL STRUCTURES ARE EXPRESSIONS THAT EVALUATE TO THEMSELVES

instance Expression Number Number where
    evaluate a = a

instance Structure Number

-- ALL EXPRESSIONS MUST BE SHOWABLE

instance Show Number where
    show (Absolute a) = "[" ++ show a ++ "]"

------------------------------------------------------------------------------
-- CONSTRAINT & OPERATOR IMPLEMENTATION

instance Addable Number Number Number where
    add (Absolute a) (Absolute b) = Absolute (a + b)
