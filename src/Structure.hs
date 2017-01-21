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

class (Show e, Structure s) => Expression e s | e -> s where
    evaluate :: e -> s

------------------------------------------------------------------------------
-- | Structure

class (Show s) => Structure s

------------------------------------------------------------------------------
