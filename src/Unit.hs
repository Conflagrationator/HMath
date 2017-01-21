-- | UNITS
------------------------------------------------------------------------------

module Unit where

------------------------------------------------------------------------------

import Extensions

import Data.List

------------------------------------------------------------------------------
-- CONVENIENCE
------------------------------------------------------------------------------

noUnit = Unit []

------------------------------------------------------------------------------
-- UNIT
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- DIMENSION
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- UNIT MATHEMATICS
------------------------------------------------------------------------------

-- | Addition of Units

-- -- | Multiplication of Units
-- multiplyUnits :: Unit -> Unit -> Unit
-- multiplyUnits u v = foldUnique multiplyUnitComponents u v
--   where
--     multiplyUnitComponents :: (Dimension, Integer) -> (Dimension, Integer) -> Maybe (Dimension, Integer)
--     multiplyUnitComponents (dim1, exp1) (dim2, exp2) = if dim1 == dim2 then Just (dim1, exp1 + exp2) else Nothing
