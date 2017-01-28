-- | The Description of the Structure of Mathematical Expression Data
------------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}

module Structure where

------------------------------------------------------------------------------

import Finite
import Extensions
import Unit

------------------------------------------------------------------------------
-- | Expression

class (Show e, Show r) => Expression e r | e -> r where
    evaluate :: e -> r

------------------------------------------------------------------------------
-- | Structure

class (Expression s s) => Structure s

------------------------------------------------------------------------------

-- TODO: add support for units in structures
-- TODO: fill out more operators
-- TODO: make it work with AsciiMath
-- TODO: make it possible for expressions to be inside structures (like vectors with expressions inside)
