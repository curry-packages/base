{-# LANGUAGE NoImplicitPrelude, CPP #-}
module Base.Num (Num(..)) where

import Base.Types
import Base.Bool
import Base.Eval
import Base.Eq
import Base.Ord
import Base.Error
import Base.Failed

default (Int, Float)

infixl 6 +, -
infixl 7 *

class Num a where
  (+), (-), (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInt :: Int -> a

  x - y = x + negate y
  negate x = 0 - x

instance Num Int where
  x + y = x `plusInt` y
  x - y = x `minusInt` y
  x * y = x `timesInt` y
  negate x = 0 - x
  abs x | x >= 0    = x
        | otherwise = negate x
  signum x | x >  0    = 1
           | x == 0    = 0
           | otherwise = -1
  fromInt x = x

instance Num Float where
  x + y = x `plusFloat` y
  x - y = x `minusFloat` y
  x * y = x `timesFloat` y
  negate x = negateFloat x
  abs x | x >= 0    = x
        | otherwise = negate x
  signum x | x >  0    = 1
           | x == 0    = 0
           | otherwise = -1
  fromInt x = intToFloat x

-- Adds two integers.
plusInt :: Int -> Int -> Int
#ifdef __KICS2__
plusInt external
#elif defined(__PAKCS__)
x `plusInt` y = (prim_plusInt $# y) $# x

prim_plusInt :: Int -> Int -> Int
prim_plusInt external
#endif

-- Subtracts two integers.
minusInt :: Int -> Int -> Int
#ifdef __KICS2__
minusInt external
#elif defined(__PAKCS__)
x `minusInt` y = (prim_minusInt $# y) $# x

prim_minusInt :: Int -> Int -> Int
prim_minusInt external
#endif

-- Multiplies two integers.
timesInt :: Int -> Int -> Int
#ifdef __KICS2__
timesInt external
#elif defined(__PAKCS__)
x `timesInt` y = (prim_timesInt $# y) $# x

prim_timesInt :: Int -> Int -> Int
prim_timesInt external
#endif

-- Adds two floating point numbers.
plusFloat :: Float -> Float -> Float
x `plusFloat` y = (prim_plusFloat $# y) $# x

prim_plusFloat :: Float -> Float -> Float
prim_plusFloat external

-- Subtracts two floating point numbers.
minusFloat :: Float -> Float -> Float
x `minusFloat` y = (prim_minusFloat $# y) $# x

prim_minusFloat :: Float -> Float -> Float
prim_minusFloat external

-- Multiplies two floating point numbers.
timesFloat :: Float -> Float -> Float
x `timesFloat` y = (prim_timesFloat $# y) $# x

prim_timesFloat :: Float -> Float -> Float
prim_timesFloat external

-- Negates a floating point number.
negateFloat :: Float -> Float
#ifdef __KICS2__
negateFloat external
#elif defined(__PAKCS__)
negateFloat x = prim_negateFloat $# x

prim_negateFloat :: Float -> Float
prim_negateFloat external
#endif

-- Converts from integers to floating point numbers.
intToFloat :: Int -> Float
intToFloat x = prim_intToFloat $# x

prim_intToFloat :: Int -> Float
prim_intToFloat external
