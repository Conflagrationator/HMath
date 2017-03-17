-- | The Description of the Value of Mathematical Expression Data
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
-- | Value

class (Expression s s) => Value s

------------------------------------------------------------------------------

-- TODO: make it work with AsciiMath
-- TODO: make it possible for expressions to be inside Values (like vectors with expressions inside)

data Guard a = Success a | Failure String -- TODO: put in Extensions

instance (Show r) => Show (Guard r) where
    show (Success a) = "[S:" ++ show a ++ "]"
    show (Failure s) = "[F:" ++ s ++ "]"

instance (Value r) => Expression (Guard r) r where
    evaluate (Success r) = Success r
    evaluate (Failure s) = Failure s

hasFailed :: Guard a -> Bool
hasFailed (Failure _) = True
hasFailed _ = False

hasSucceeded :: Guard a -> Bool
hasSucceeded (Success _) = True
hasSucceeded _ = False

failureMessage :: Guard a -> String
failureMessage (Failure s) = s
failureMessage _ = error "Tried to get a falure message from a success"

fromSuccess :: Guard a -> a
fromSuccess (Success a) = a
fromSuccess _ = error "tried to unwrap a failure as a success"
