{-# LANGUAGE NoImplicitPrelude #-}
module Base.Integral (Integral (..), even, odd, fromIntegral, realToFrac) where

import Base.Types
import Base.Num
import Base.Fractional
import Base.Real
import Base.Enum

infixl 7 `div`, `mod`, `quot`, `rem`

class (Real a, Enum a) => Integral a where
  div, mod :: a -> a -> a
  quot, rem :: a -> a -> a
  divMod :: a -> a -> (a, a)
  quotRem :: a -> a -> (a, a)
  toInt :: a -> Int

  n `div` d = q
   where (q, _) = divMod n d
  n `mod` d = r
   where (_, r) = divMod n d
  n `quot` d = q
   where (q, _) = quotRem n d
  n `rem` d = r
   where (_, r) = quotRem n d

instance Integral Int where
  divMod n d = (n `divInt` d, n `modInt` d)
  quotRem n d = (n `quotInt` d, n `remInt` d)
  toInt x = x

--- Returns whether an integer is even.
even :: Integral a => a -> Bool
even n = n `rem` 2 == 0

--- Returns whether an integer is odd.
odd :: Integral a => a -> Bool
odd = not . even

--- General coercion from integral types.
fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInt . toInt

--- General coercion to fractional types.
realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromFloat . toFloat

-- Integer division. The value is the integer quotient of its arguments
-- and always truncated towards negative infinity.
divInt :: Int -> Int -> Int
#ifdef __KICS2__
divInt external
#elif defined(__PAKCS__)
x `divInt` y = (prim_divInt $# y) $# x

prim_divInt :: Int -> Int -> Int
prim_divInt external
#endif

-- Integer remainder. The value is the remainder of the integer division
-- and it obeys the rule `mod x y = x - y * (div x y)`.
modInt :: Int -> Int -> Int
#ifdef __KICS2__
modInt external
#elif defined(__PAKCS__)
x `modInt` y = (prim_modInt $# y) $# x

prim_modInt :: Int -> Int -> Int
prim_modInt external
#endif

-- Integer division. The value is the integer quotient of its arguments
-- and always truncated towards zero.
quotInt :: Int -> Int -> Int
#ifdef __KICS2__
quotInt external
#elif defined(__PAKCS__)
x `quotInt` y = (prim_quotInt $# y) $# x

prim_quotInt :: Int -> Int -> Int
prim_quotInt external
#endif

-- Integer remainder. The value is the remainder of the integer division
-- and it obeys the rule `rem x y = x - y * (quot x y)`.
remInt :: Int -> Int -> Int
#ifdef __KICS2__
remInt external
#elif defined(__PAKCS__)
x `remInt` y = (prim_remInt $# y) $# x

prim_remInt :: Int -> Int -> Int
prim_remInt external
#endif
