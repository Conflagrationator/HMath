-- | The Finite Module which exports Finite
----------------------------------------------------------------

module Finite (Finite(..), (*^), (*^-), sigfigs, orderOfMagnitude, showNatural, showFull, showScientific, roundToPrecision, fromIntegerWithPrecision, fromIntegerMatchingPrecision) where

----------------------------------------------------------------

import Extensions

import Data.Ratio

----------------------------------------------------------------
-- FINITE DATA TYPE
----------------------------------------------------------------
-- | Finite Data Type `(b*10^e)`
-- * ensure that the digits comprising b correspond to the significant
--   figures of the number
data Finite = Finite
    {base :: Integer, -- ^ the `b` in `b*10^e`
     expo :: Integer} -- ^ the `e` in `b*10^e`

-- CONVENIENCE CONSTRUCTORS

-- | Constructor for Finite Data Type
(*^) :: Integer -> Integer -> Finite
a*^m = Finite a m

-- | Convenience constructor for Finite where the exponent is negative
--   This is most commonly used for when writing literals
(*^-) :: Integer -> Integer -> Finite
a*^-m = a*^(-m)

-- INFORMATION

-- | Significant figures
sigfigs :: Finite -> Integer
sigfigs (Finite a _) = digits a

-- | Order of Magnitude
--   `n.nnnn*10^?`
orderOfMagnitude :: Finite -> Integer
orderOfMagnitude (Finite a m) = digits a - 1 + m

----------------------------------------------------------------
-- SHOWABILITY
----------------------------------------------------------------

instance Show Finite where
    show = showNatural

-- | Show with component decomposition
showNatural :: Finite -> String
showNatural (Finite a m) = "[" ++ (show a) ++ "*10^" ++ (show m) ++ "]"

-- | Show in full written form
showFull :: Finite -> String
showFull (Finite a m)
    | m < 0 = show intPart ++ "." ++ leadingZeroes ++ show floatPart -- has decimals
    | otherwise = show (a*10^m) -- no decimals
  where
    intPart = a `div` 10^(abs m)
    floatPart = a - a `div` 10^(abs m) * 10^(abs m) -- not including leading 0's
    leadingZeroes = replicate (fromIntegral $ abs m - digits floatPart) '0'

-- | Show in Scientific form
showScientific :: Finite -> String
showScientific (Finite a m) = lead ++ decimals ++ "e" ++ show exponent
  where
    lead = [head (show a)]
    decimals = if tail (show a) /= "" then "." ++ tail (show a) else ""
    exponent = digits a + m - 1

----------------------------------------------------------------
-- NUMBERNESS
----------------------------------------------------------------

instance Num Finite where
    am@(Finite a m) + bn@(Finite b n) = roundToPrecision (u *^ lowestExpo) (digits u - diff) -- FIXME: rounding error on associativity
      where
        lowestExpo = min m n
        highestExpo = max m n
        u = a*10^(m-lowestExpo) + b*10^(n-lowestExpo) -- the unrounded base of the added number
        diff = highestExpo - lowestExpo -- the difference in exponents (always positive)
    
    am - bn = am + (-bn)
    
    am@(Finite a m) * bn@(Finite b n) = roundToPrecision ((a * b) *^ (m + n)) lowestPrecision -- FIXME: rounding error on associativity
      where
        lowestPrecision = min (sigfigs am) (sigfigs bn)
    
    negate (Finite a m) = Finite (-a) m
    
    abs (Finite a m) = abs a *^ m
    
    signum (Finite a _) = Finite (signum a) 0
    
    fromInteger a = (a*10^16)*^-16 -- ^ Assumes IEEE Double precision

-- | takes an integer and sets it to a certain sigfigs precision
--   (instead of the default IEEE Double precision)
fromIntegerWithPrecision :: Integer -> Integer -> Finite
fromIntegerWithPrecision a p
    | digits a > p = (a `div` 10^(digits a - p))*^(digits a - p) -- cut off end of int and give it that exponent
    | otherwise = (a*10^(p - digits a))*^-(p - digits a) -- append zeroes to end and give it that exponent

-- | takes an integer and another finite and turns the integer into
--   a finite of the same precision (sigfigs)
fromIntegerMatchingPrecision :: Integer -> Finite -> Finite
fromIntegerMatchingPrecision a n = fromIntegerWithPrecision a (sigfigs n)

----------------------------------------------------------------
-- EQUATABILITY & ORDERABILITY
----------------------------------------------------------------

-- | tests if two finites have the same value
instance Eq Finite where
    (Finite a m) == (Finite b n)
        | n < m = a*10^(m-n) == b
        | otherwise = a == b*10^(n-m)

-- | tests if two finites are exactly the same
(Finite a m) === (Finite b n) = a == b && m == n

instance Ord Finite where
    compare (Finite a m) (Finite b n) = compare (a*10^(m-lowestExpo)) (b*10^(n-lowestExpo))
      where
        lowestExpo = min m n

----------------------------------------------------------------
-- FRACTIONALNESS
----------------------------------------------------------------

instance Fractional Finite where
    am@(Finite a m) / bn@(Finite b n) = roundToPrecision (((a*10^(db+1)) `div` b) *^ (m-n-db-1)) lowestPrecision
      where
        db = digits b
        lowestPrecision = min (sigfigs am) (sigfigs bn)

    fromRational r = (fromInteger (numerator r)) / (fromInteger (denominator r)) -- ^ Assumes IEEE Double precision

-- | The representation as a ratio
--   `a/10^-m`
instance Real Finite where
    toRational (Finite a m)
        | m > 0 = a % 1
        | otherwise = a % (10^(-m))

-- | The representation as an integer part and float part
instance RealFrac Finite where
    properFraction am@(Finite a m) = (intPart, floatPart)
      where
        intPart = fromIntegral $ if m < 0 then a `div` 10^(abs m) else a * 10^m
        floatPart = am - fromIntegral intPart

----------------------------------------------------------------
-- EXTENDED ARITHMETIC
----------------------------------------------------------------

-- | Powers

-- power :: Finite -> Finite -> Finite
-- power (Finite a m) (Finite b n) =

-- | Roots

-- root :: Finite -> Finite -> Finite
-- root (Finite a m) (Finite b n) =

-- | Logarithms

-- log :: Finite -> Finite -> Finite
-- log (Finite a m) (Finite b n) = 

----------------------------------------------------------------
-- OTHER FUNCTIONS
----------------------------------------------------------------

-- | Round to a certain precision
--   supply with a number and the number of significant figures you want to round to
roundToPrecision :: Finite -> Integer -> Finite
roundToPrecision am@(Finite a m) p
    | d <= p = am -- if equal or worse precision than specified, return original
    | otherwise = (a `div` 10^(d-p) + r) *^ (d-p+m) -- d-p is how many digits to take off the end of a
  where
      d = digits a
      r = if a `div` 10^(d-p-1) `mod` 10 >= 5 then 1 else 0 -- add 1 to the number if the digit after the precision is >= 5

----------------------------------------------------------------
-- MATHEMATICAL CONSTANTS
----------------------------------------------------------------

-- piAtPrecision :: Integer -> Finite


-- eAtPrecision :: Integer -> Finite
