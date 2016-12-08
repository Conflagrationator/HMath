-- Vector Support
----------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Structure.Vector where

----------------------------------------------------------------

import Structure

----------------------------------------------------------------
-- SRUCTURE DEFINITION
----------------------------------------------------------------

class (Structure s) => Vector s where
    add :: s -> s -> s
    smult :: Number -> s -> s
    zero :: s

----------------------------------------------------------------
-- APPLICATIONS
----------------------------------------------------------------

-- NUMBERS

instance Vector Number where
    add (Absolute a) (Absolute b) = Absolute (a + b)
    add (Absolute a) (Measure b) = Measure (fromIntegral a + b)
    add (Measure a) (Absolute b) = add (Absolute b) (Measure a)
    add _ _ = unable
    smult (Absolute a) (Absolute b) = Absolute (a*b)
    smult (Absolute a) (Measure b) = Measure (fromIntegral a * b)
    smult (Measure a) (Absolute b) = add (Absolute b) (Measure a)
    smult _ _ = unable
    zero = Absolute 0

-- EXPRESSION

-- instance Vector Expression where
--     -- TODO: need definition of addition operator and scalar multiple operator
--     -- but this will make the R1 case work
--
-- -- R^1
--
-- data R1 = R1 Expression
--
-- instance Show R1 where
--     show (R1 a) = "[" ++ show a ++ "]"
--
-- instance Eq R1 where
--     (R1 a) == (R1 b) = a == b
--
-- instance Structure R1 where
--     unable = (R1 unable)
--
-- instance Vector R1 where
--     add (R1 a) (R1 b) = R1 (add a b)
--     add _ _ = unable
--     smult a (R1 b) = R1 (smult a b)
--     smult _ _ = unable
--     zero = (R1 zero)


-- TODO: maybe Functions should be linked with their own class
-- to where Addable would be the class for the
-- add function, and so only any addable structures could be
-- put under it (or at least evaluated)
-- similarly
-- only Divisible structures (Structure, Divisible) would be able
-- to be in the second argument of the Divide Function (Operator)
