-- General Extensions to common functionality
-- or additional functionality not supported by default libraries
--------------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Extensions where

--------------------------------------------------------------------------------

import Data.Ratio
import Data.Maybe
import Data.List

--------------------------------------------------------------------------------
-- NUMBER MANIPULATION
--------------------------------------------------------------------------------

-- | Find the number of Digits in a Decimal Number
--   this is useful
digits :: Integer -> Integer
digits 0 = 1 -- helps with power and string manipulation stuff
digits a = until (\n -> abs a `div` 10 ^ n == 0) (+ 1) 0

--------------------------------------------------------------------------------
-- LIST MANIPULATION
--------------------------------------------------------------------------------

-- | Grouping by Association
--   function returns true if these two are associated
groupAssoc :: (a -> a -> Bool) -> [a] -> [[a]]
groupAssoc _ [] = []
groupAssoc f (a:rest) = (a:as) : groupAssoc f bs
  where
    (as, bs) = partition (f a) rest

-- | Combine so that like things are combined
--   the function combines elements if possible
--   the function must also once it is applied still match its own condition for combination
foldUnique :: Eq a => (a -> a -> Maybe a) -> [a] -> [a] -> [a]
foldUnique f as bs = map foldr' $ groupAssoc (\a b -> isJust (f a b)) $ as ++ bs
  where
    foldr' ns = foldr (\a b -> fromJust (f a b)) (head ns) (tail ns) -- modified foldr which has its seed as the head

--------------------------------------------------------------------------------
-- RELATIONSHIPS
--------------------------------------------------------------------------------

-- | Until relationship holds
--   apply the "new" function supplied until a certain relationship between
--   the previous and next iterations is established. Return the newest of those.
untilR :: ((a, a) -> Bool) -> (a -> a) -> a -> a
untilR relationship new seed = snd $ until relationship (\(o, n) -> (n, new n)) (seed, new seed)
