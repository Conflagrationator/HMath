-- | The Description of the Structure of Mathematical Expression Data
------------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Structure where

------------------------------------------------------------------------------

import Finite
import Extensions
import Unit

------------------------------------------------------------------------------
-- | Expression

class (Show e, Show r) => Expression e r | e -> r where
    evaluate :: e -> Guard r
    -- Either String r, Left is for failure message

------------------------------------------------------------------------------
-- | Structure

class (Expression s s) => Structure s

------------------------------------------------------------------------------

-- TODO: make it work with AsciiMath
-- TODO: make it possible for expressions to be inside structures (like vectors with expressions inside)

data Guard a = Success a | Failure String -- TODO: put in Extensions

instance (Show r) => Show (Guard r) where
    show (Success a) = show a
    show (Failure s) = s

instance (Structure r) => Expression (Guard r) r where
    evaluate (Success r) = Success r
    evaluate (Failure s) = Failure s

hasFailed :: Guard a -> Bool
hasFailed (Failure _) = True
hasFailed _ = False

hasSucceeded :: Guard a -> Bool
hasSucceeded (Success _) = True
hasSucceeded _ = False
