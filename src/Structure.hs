-- | Equation Solvers for Haskell
----------------------------------------------------------------

module Structure (Number (..), Unit, Dimension (..), Expression (..), Evaluator, Identifier) where

----------------------------------------------------------------

import Finite

import Data.List

----------------------------------------------------------------
-- NUMBER
----------------------------------------------------------------

-- | The type that references all number-like values
data Number
    = Measure Finite -- ^ "normal" inputted numbers and everything measured
    | Absolute Integer -- ^ numbers as used in math for finding simplest forms
    | Infinity Bool -- ^ Infinity, whrere true is +∞ and false is -∞
    | Unknown String -- ^ the representation of variables
    | Undefined -- ^ the value when a computation cannot give a useful solution

instance Show Number where
    show (Measure a) = show a
    show (Absolute a) = show a
    show (Infinity s) = (if s then "-" else "") ++ "oo"
    show (Unknown x) = x
    show (Undefined) = "?"

----------------------------------------------------------------
-- UNIT
----------------------------------------------------------------

-- | Compound Unit for a number
--   Consists of all dimensional components
--   * Dimension
--   * Exponent (m^2?, m^3?, etc.)
type Unit = [(Dimension, Integer)]

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

----------------------------------------------------------------
-- EXPRESSION
----------------------------------------------------------------

-- | The Nodes of the Abstract Syntax Tree
data Expression
    = Operator Evaluator [Expression] -- ^ The middle nodes, which tell it how to collapse and what's underneath
    | Value Number Unit -- ^ The Bottom Node

instance Show Expression where
    show (Value n us) = show n ++ unitsString
      where
        unitDivisions = span (\(_, e) -> e >= 0) $ sortBy (\(_, e1) (_, e2) -> compare e2 e1) us
        upperUnits = fst unitDivisions -- the units in the numerator
        lowerUnits = snd unitDivisions -- the units in the denominator
        showUnit (d, e) = show d ++ if e == 1 then "" else "^" ++ show (abs e) -- abs because we put it under a division later
        upperUnitsString = if upperUnits /= [] then "(" ++ intercalate "*" (map showUnit upperUnits) ++ ")" else "1"
        lowerUnitsString = if lowerUnits /= [] then "/(" ++ intercalate "*" (map showUnit lowerUnits) ++ ")" else "" -- NOTE: includes the "/" (or not)
        unitsString = if unitDivisions /= ([], []) then "(" ++ upperUnitsString ++ lowerUnitsString ++ ")" else ""

-- | The Description and Functionality corresponding to an Operator
--   functions as the Faillable Evaluator for the tree
--   it will either return a Value of the evaluated Expression
--   or the same expression back (indicating it couldn't be evaluated)
type Evaluator = (Identifier, [Expression] -> Expression)

-- | Identifier of an operator
type Identifier = String
