{-# LANGUAGE NoImplicitPrelude, CPP #-}
module Base.Ord (Ord(..)) where

import Base.Types
import Base.Eq
import Base.Bool
import Base.Eval
import Base.Failed

infix 4 <, >, <=, >=

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (>), (<=), (>=) :: a -> a -> Bool
  min, max :: a -> a -> a

  compare x y | x == y = EQ
              | x <= y = LT
              | otherwise = GT
  x < y = x <= y && x /= y
  x > y = not (x <= y)
  x <= y = compare x y == EQ || compare x y == LT
  x >= y = y <= x
  min x y | x <= y = x
          | otherwise = y
  max x y | x >= y = x
          | otherwise = y

instance Ord Char where
  c1 <= c2 = c1 `ltEqChar` c2

instance Ord Int where
  i1 <= i2 = i1 `ltEqInt` i2

instance Ord Float where
  f1 <= f2 = f1 `ltEqFloat` f2

instance Ord () where
  () <= () = True

instance (Ord a, Ord b) => Ord (a, b) where
  (a, b) <= (a', b') = a < a' || (a == a' && b <= b')

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
  (a, b, c) <= (a', b', c') = a < a'
    || (a == a' && b < b')
    || (a == a' && b == b' && c <= c')

instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d) where
  (a, b, c, d) <= (a', b', c', d') = a < a'
    || (a == a' && b < b')
    || (a == a' && b == b' && c < c')
    || (a == a' && b == b' && c == c' && d <= d')

instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e) where
  (a, b, c, d, e) <= (a', b', c', d', e') = a < a'
    || (a == a' && b < b')
    || (a == a' && b == b' && c < c')
    || (a == a' && b == b' && c == c' && d < d')
    || (a == a' && b == b' && c == c' && d == d' && e <= e')

instance Ord a => Ord [a] where
  []     <= []     = True
  (_:_)  <= []     = False
  []     <= (_:_)  = True
  (x:xs) <= (y:ys) | x == y    = xs <= ys
                   | otherwise = x < y

instance Ord Bool where
  False <= False = True
  False <= True  = True
  True  <= False = False
  True  <= True  = True

instance Ord Ordering where
  LT <= LT = True
  LT <= EQ = True
  LT <= GT = True
  EQ <= LT = False
  EQ <= EQ = True
  EQ <= GT = True
  GT <= LT = False
  GT <= EQ = False
  GT <= GT = True

-- Compares two characters.
ltEqChar :: Char -> Char -> Bool
#ifdef __KICS2__
ltEqChar external
#elif defined(__PAKCS__)
ltEqChar x y = (prim_ltEqChar $# y) $# x

prim_ltEqChar :: Char -> Char -> Bool
prim_ltEqChar external
#endif

-- Compares two integers.
ltEqInt :: Int -> Int -> Bool
#ifdef __KICS2__
ltEqInt external
#elif defined(__PAKCS__)
ltEqInt x y = (prim_ltEqInt $# y) $# x

prim_ltEqInt :: Int -> Int -> Bool
prim_ltEqInt external
#endif

-- Compares two floating point numbers.
ltEqFloat :: Float -> Float -> Bool
#ifdef __KICS2__
ltEqFloat external
#elif defined(__PAKCS__)
ltEqFloat x y = (prim_ltEqFloat $# y) $# x

prim_ltEqFloat :: Float -> Float -> Bool
prim_ltEqFloat external
#endif
