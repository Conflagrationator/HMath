-- | UNITS
------------------------------------------------------------------------------

module Unit where

------------------------------------------------------------------------------

import Extensions

import Data.List
import Data.Maybe
import Data.Ratio

------------------------------------------------------------------------------
-- UNIT
------------------------------------------------------------------------------

data Unit = Unit {
    meter :: Integer,
    kilogram :: Integer,
    second :: Integer,
    ampere :: Integer,
    kelvin :: Integer,
    mole :: Integer,
    candela :: Integer,
    dollar :: Integer}

-- | Must have the same dimensions
instance Eq Unit where
    a == b = meter a == meter b && kilogram a == kilogram b && second a == second b && ampere a == ampere b && kelvin a == kelvin b && mole a == mole b && candela a == candela b

-- | Straightforward showing method
instance Show Unit where
    show u = intercalate "*" $ map (\d -> if (fst d) /= 1 then (snd d) ++ "^" ++ show (fst d) else (snd d)) visible
      where
        dimensions = [(meter u, "m"), (kilogram u, "kg"), (second u, "s"), (ampere u, "A"), (kelvin u, "K"), (mole u, "mol"), (candela u, "cd")]
        visible = filter (\d -> fst d /= 0) dimensions

------------------------------------------------------------------------------
-- COMBINATION

-- | Must be same unit
addUnits :: Unit -> Unit -> Maybe Unit
addUnits a b = if a == b then Just a else Nothing

multiplyUnits :: Unit -> Unit -> Unit -- guaranteed to succeed
multiplyUnits a b = Unit {
    meter = meter a + meter b,
    kilogram = kilogram a + kilogram b,
    second = second a + second b,
    ampere = ampere a + ampere b,
    kelvin = kelvin a + kelvin b,
    mole = mole a + mole b,
    candela = candela a + candela b,
    dollar = dollar a + dollar b}

raiseUnit :: Unit -> Rational -> Maybe Unit
raiseUnit u e = if length (catMaybes [m, kg, s, a, k, mol, cd, usd]) /= 8 then Nothing else Just $ Unit {meter = fromJust m, kilogram = fromJust kg, second = fromJust s, ampere = fromJust a, kelvin = fromJust k, mole = fromJust mol, candela = fromJust cd, dollar = fromJust usd}
  where
    castToInteger :: Rational -> Maybe Integer
    castToInteger n = if denominator n == 1 then Just (numerator n) else Nothing
    m = castToInteger $ fromIntegral (meter u) * e
    kg = castToInteger $ fromIntegral (kilogram u) * e
    s = castToInteger $ fromIntegral (second u) * e
    a = castToInteger $ fromIntegral (ampere u) * e
    k = castToInteger $ fromIntegral (kelvin u) * e
    mol = castToInteger $ fromIntegral (mole u) * e
    cd = castToInteger $ fromIntegral (candela u) * e
    usd = castToInteger $ fromIntegral (dollar u) * e

------------------------------------------------------------------------------
-- DEFAULT UNITS

unitless = Unit {meter = 0, kilogram = 0, second = 0, ampere = 0, kelvin = 0, mole = 0, candela = 0, dollar = 0}

-- SI units
meters = unitless {meter = 1}
kilograms = unitless {kilogram = 1}
seconds = unitless {second = 1}
amperes = unitless {ampere = 1}
kelvins = unitless {kelvin = 1} -- plural of kelvin is kelvin, so added an s here to differentiate
moles = unitless {mole = 1}
candelas = unitless {candela = 1}
dollars = unitless {dollar = 1}
