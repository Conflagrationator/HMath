-- | The Description of the Structure of Mathematical Data
----------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Structure where

----------------------------------------------------------------

import Finite

import Data.List
import Data.Typeable
import Data.Maybe

----------------------------------------------------------------
-- EXPRESSION
----------------------------------------------------------------

data Expression
    = forall s. Structure s => Value s Unit
    -- | forall o. Operator o => Function o [Expression]

instance Eq Expression where
    (Value a u) == (Value b v) = if typeOf a == typeOf b then a == fromJust (cast b) && u == v else False

instance Show Expression where
    show (Value a u) = show a ++ show u

instance Structure Expression where
    unable = Value Undefined (Unit [])

----------------------------------------------------------------
-- OPERATOR
----------------------------------------------------------------


-- want things like:

-- Function (add) [Value Absolute a u, Value Absolute b v] -> Value Absolute a+b u+v -- using new add definition defined with Number
-- Function (add) [Value Absolute a u, Value Vector b v] -> Value Undefined u+v -- using add definition defined with Vector
-- Function (mult) [Value Absolute a u, Value Vector b v] -> Value Vector (a * b) u*v -- using mult definition defined with Vector
-- Function (mult) [Value Matrix a u, Value Matrix b v] -> Value Matrix (a * b) u*v -- using mult definition defined with Matrix

----------------------------------------------------------------
-- STRUCTURE
----------------------------------------------------------------

-- | A Leaf node of the tree
--   you must be able to compare it and show it
class (Eq s, Show s, Typeable s) => Structure s where
    unable :: s -- ^ when a structure computation is unable to complete (because of a mathematical impossibility)
                --   this tells the Function in the expression to look for another way to evaluate
                --   or to simply return the expression as is

-- subclasses which structures must conform to to inherit functionality of operators

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

instance Structure Number where
    unable = Undefined

-- put other subclasses on the same level as Number

----------------------------------------------------------------
-- UNIT
----------------------------------------------------------------

-- | Compound Unit for a number
--   Consists of all dimensional components
--   * Dimension
--   * Exponent (m^2?, m^3?, etc.)
data Unit = Unit [(Dimension, Integer)]

instance Eq Unit where
    (Unit us) == (Unit vs) = us == vs

instance Show Unit where
    show (Unit us) = unitsString
      where
        unitDivisions = span (\(_, e) -> e >= 0) $ sortBy (\(_, e1) (_, e2) -> compare e2 e1) us -- split based on positive powers and negative powers (above & below the division)
        upperUnits = fst unitDivisions -- the units in the numerator
        lowerUnits = snd unitDivisions -- the units in the denominator
        showUnit (d, e) = show d ++ if e == 1 then "" else "^" ++ show (abs e) -- abs because we put it under a division later
        upperUnitsString = if upperUnits /= [] then "(" ++ intercalate "*" (map showUnit upperUnits) ++ ")" else "1"
        lowerUnitsString = if lowerUnits /= [] then "/(" ++ intercalate "*" (map showUnit lowerUnits) ++ ")" else "" -- includes the "/" (or not) here
        unitsString = if unitDivisions /= ([], []) then "{:" ++ upperUnitsString ++ lowerUnitsString ++ ":}" else ""

----------------------------------------------------------------
-- DIMENSION
----------------------------------------------------------------

-- | The SI Units, to be paired with numbers for aid in computation
data Dimension
    = Meter -- ^ Length
    | KiloGram -- ^ Mass
    | Second -- ^ Time
    | Ampere -- ^ Electric Current
    | Kelvin -- ^ Temperature
    | Mole -- ^ Amount of Substance
    | Candela -- ^ Luminous Intensity
    deriving (Eq)

instance Show Dimension where
    show Meter = "m"
    show KiloGram = "kg"
    show Second = "s"
    show Ampere = "A"
    show Kelvin = "K"
    show Mole = "mol"
    show Candela = "Cd"
