-- The Number Structure
----------------------------------------------------------------

module Number where

----------------------------------------------------------------

import Finite

----------------------------------------------------------------
-- NUMBER
----------------------------------------------------------------

-- | the fundamental unit of a structure
--   you can think of them as the "scalars" in respect to the "structures"
data Number
    = Absolute Integer
    | Measure Finite
    | Variable String
    | Undefined

instance Eq Number where
    (Measure a) == (Measure b) = a == b
    (Absolute a) == (Absolute b) = a == b
    (Variable a) == (Variable b) = a == b
    (Undefined) == (Undefined) = False -- cannot determine
    _ == _ = False

instance Show Number where
    show (Measure a) = show a
    show (Absolute a) = show a
    show (Variable a) = show a
    show (Undefined) = "?"

----------------------------------------------------------------
-- EXPRESSION
----------------------------------------------------------------
