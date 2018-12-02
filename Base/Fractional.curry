{-# LANGUAGE NoImplicitPrelude #-}
module Base.Fractional (Fractional(..)) where

import Base.Types
import Base.Num

infixl 7 /

class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromFloat :: Float -> a

  recip x = 1.0 / x
  x / y = x * recip y

instance Fractional Float where
  x / y = x `divFloat` y
  fromFloat x = x

-- Division on floating point numbers.
divFloat :: Float -> Float -> Float
x `divFloat` y = (prim_divFloat $# y) $# x

prim_divFloat :: Float -> Float -> Float
prim_divFloat external
