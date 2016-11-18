-- | Equation Solvers for Haskell
----------------------------------------------------------------

module Structure where

----------------------------------------------------------------

import Finite

----------------------------------------------------------------
-- NUMBER
----------------------------------------------------------------

-- | The type that references all number-like values
data Number
    = Real Finite -- ^ "normal" inputted numbers and everything measured
    | Absolute Integer -- ^ numbers as used in math for finding simplest forms
    | Infinity Bool -- ^ Infinity, whrere true is +∞ and false is -∞
    | Unknown String -- ^ the representation of variables
    | Undefined -- ^ the value when a computation cannot give a useful solution

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
    deriving (Eq, Show)

----------------------------------------------------------------
-- EXPRESSION
----------------------------------------------------------------

-- | The Nodes of the Abstract Syntax Tree
data Expression
    = Operator Evaluator [Expression] -- ^ The middle nodes, which tell it how to collapse and what's underneath
    | Value Number Unit -- ^ The Bottom Node

-- | The Description and Functionality corresponding to an Operator
--   functions as the Faillable Evaluator for the tree
--   it will either return a Value of the evaluated Expression
--   or the same expression back (indicating it couldn't be evaluated)
type Evaluator = (Identifier, [Expression] -> Expression)

-- | Identifier of an operator
type Identifier = String
