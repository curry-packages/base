{-# LANGUAGE NoImplicitPrelude, CPP #-}
module Base.Eq (Eq(..)) where

import Base.Types
import Base.Bool
import Base.Eval

infix 4 ==, /=

class Eq a where
  (==), (/=) :: a -> a -> Bool

  x == y = not (x /= y)
  x /= y = not (x == y)

instance Eq Char where
  c == c' = c `eqChar` c'

instance Eq Int where
  i == i' = i `eqInt` i'

instance Eq Float where
  f == f' = f `eqFloat` f'

instance Eq () where
  () == () = True

instance (Eq a, Eq b) => Eq (a, b) where
  (a, b) == (a', b') = a == a' && b == b'

instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  (a, b, c) == (a', b', c') = a == a' && b == b' && c == c'

instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d) where
  (a, b, c, d) == (a', b', c', d') = a == a' && b == b' && c == c' && d == d'

instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e) where
  (a, b, c, d, e) == (a', b', c', d', e') =
    a == a' && b == b' && c == c' && d == d' && e == e'

instance Eq a => Eq [a] where
  []     == []     = True
  []     == (_:_)  = False
  (_:_)  == []     = False
  (x:xs) == (y:ys) = x == y && xs == ys

instance Eq Bool where
  False == False = True
  False == True  = False
  True  == False = False
  True  == True  = True

instance Eq Ordering where
  LT == LT = True
  LT == EQ = False
  LT == GT = False
  EQ == LT = False
  EQ == EQ = True
  EQ == GT = False
  GT == LT = False
  GT == EQ = False
  GT == GT = True

-- Equality on characters.
eqChar :: Char -> Char -> Bool
#ifdef __KICS2__
eqChar external
#elif defined(__PAKCS__)
eqChar x y = (prim_eqChar $# y) $# x

prim_eqChar :: Char -> Char -> Bool
prim_eqChar external
#endif

-- Equality on integers.
eqInt :: Int -> Int -> Bool
#ifdef __KICS2__
eqInt external
#elif defined(__PAKCS__)
eqInt x y = (prim_eqInt $# y) $# x

prim_eqInt :: Int -> Int -> Bool
prim_eqInt external
#endif

-- Equality on floating point numbers.
eqFloat :: Float -> Float -> Bool
#ifdef __KICS2__
eqFloat external
#elif defined(__PAKCS__)
eqFloat x y = (prim_eqFloat $# y) $# x

prim_eqFloat :: Float -> Float -> Bool
prim_eqFloat external
#endif
