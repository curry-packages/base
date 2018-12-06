{-# LANGUAGE NoImplicitPrelude #-}
module Base.RealFrac (RealFrac (..)) where

import Base.Types
import Base.Num
import Base.Fractional
import Base.Real
import Base.Integral
import Base.Ord
import Base.Function
import Base.Eval
import Base.Error

class (Real a, Fractional a) => RealFrac a where
  properFraction :: Integral b => a -> (b, a)
  truncate :: Integral b => a -> b
  round :: Integral b => a -> b
  ceiling :: Integral b => a -> b
  floor :: Integral b => a -> b

  truncate x = m
   where (m, _) = properFraction x
  round x = let (n, r) = properFraction x
                m      = if r < 0 then n - 1 else n + 1
            in case compare (signum (abs r - 0.5)) 0 of
                 LT -> n
                 EQ -> if even n then n else m
                 GT -> m
  ceiling x = if r > 0 then n + 1 else n
   where (n, r) = properFraction x
  floor x = if r < 0 then n - 1 else n
   where (n, r) = properFraction x

instance RealFrac Float where
  properFraction x = (n, x - fromIntegral n)
   where n = truncate x
  truncate = fromInt . truncateFloat
  round = fromInt . roundFloat

-- Conversion function from floating point numbers to integers.
-- The result is the closest integer between the argument and 0.
truncateFloat :: Float -> Int
truncateFloat x = prim_truncateFloat $# x

prim_truncateFloat :: Float -> Int
prim_truncateFloat external

-- Conversion function from floating point numbers to integers.
-- The result is the nearest integer to the argument.
-- If the argument is equidistant between two integers,
-- it is rounded to the closest even integer value.
roundFloat :: Float -> Int
roundFloat x = prim_roundFloat $# x

prim_roundFloat :: Float -> Int
prim_roundFloat external
