-- The internal (non-exported) parts of the Finite module
----------------------------------------------------------------

module Finite.Internal where

----------------------------------------------------------------

import Data.Ratio

----------------------------------------------------------------
-- HELPER FUNCTIONS
----------------------------------------------------------------

-- | Find the number of Digits in a Decimal Number
--   this is useful 
digits :: Integer -> Integer
digits a = until (\n -> abs a `div` 10 ^ n == 0) (+ 1) 0