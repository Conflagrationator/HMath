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
    | Unknown String -- ^ the representation of variables
    | Infinity Bool -- ^ Infinity, whrere true is +∞ and false is -∞
    | Undefined -- ^ the value when a computation cannot give a useful solution

----------------------------------------------------------------
-- UNIT
----------------------------------------------------------------

-- | Compound Unit for a number
--   Consists of all dimensional components
--   * Dimension
--   * Extension (Dimension Multiplier: g = 1, kg = 1000, mg = 1/1000, ng = 1/1000000000, etc)
--   * Exponent (m^2?, m^3?, etc.)
type Unit = [(Dimension, Integer, Integer)]

-- | The SI Units, to be paired with numbers for aid in computation
data Dimension
    = Meter -- ^ Length
    | Gram -- ^ Mass
    | Second -- ^ Time
    | Ampere -- ^ Electric Current
    | Kelvin -- ^ Temperature
    | Mole -- ^ Amount of Substance
    | Candela -- ^ Luminous Intensity
    | Unitless -- ^ No Dimension
    deriving Eq

----------------------------------------------------------------
-- EXPRESSION
----------------------------------------------------------------

-- | The Nodes of the Abstract Syntax Tree
data Expression
    = Operator String Evaluator [Expression] -- ^ The middle nodes, which tell it how to collapse and what's underneath
    | Value Number Unit -- ^ The Bottom Node

-- FIXME: should it be a monad, it must be faillable
type Evaluator = [Expression] -> Expression -- ^ takes the sub expressions and returns an expression that can be evaluated up the tree
    -- does not need to evaluate to Value however, if it cannot evaluate, it will simplify as far as possible
