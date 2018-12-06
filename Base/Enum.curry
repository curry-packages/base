{-# LANGUAGE NoImplicitPrelude #-}
module Base.Enum (Enum(..)) where

import Base.Types
import Base.Char
import Base.List
import Base.Bounded
import Base.Ord
import Base.Function
import Base.Num
import Base.Failed
import Base.Bool
import Base.Eval
import Base.Error

class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

  succ = toEnum . (+ 1) . fromEnum
  pred = toEnum . (\x -> x - 1) . fromEnum
  enumFrom x = map toEnum [fromEnum x ..]
  enumFromThen x y = map toEnum [fromEnum x, fromEnum y ..]
  enumFromTo x y = map toEnum [fromEnum x .. fromEnum y]
  enumFromThenTo x y z = map toEnum [fromEnum x, fromEnum y .. fromEnum z]

instance Enum Char where
  succ c | c < maxBound = chr $ ord c + 1
  pred c | c > minBound = chr $ ord c - 1
  toEnum = chr
  fromEnum = ord
  enumFrom x = [x .. maxBound]
  enumFromThen x y | y >= x    = [x, y .. maxBound]
                   | otherwise = [x, y .. minBound]

instance Enum Int where
  succ x = x + 1
  pred x = x - 1
  toEnum n = n
  fromEnum n = n
  enumFrom x = x : enumFrom (x + 1)
  enumFromTo x y | x > y     = []
                 | otherwise = x : enumFromTo (x + 1) y
  enumFromThen x y = iterate ((y - x) +) x
  enumFromThenTo x y z = takeWhile p (enumFromThen x y)
   where p x' | y >= x    = x' <= z
              | otherwise = x' >= z

instance Enum () where
  succ _ = failed
  pred _ = failed
  toEnum 0 = ()
  fromEnum () = 0
  enumFrom () = [()]
  enumFromThen () () = let units = () : units in units
  enumFromTo () () = [()]
  enumFromThenTo () () () = let units = () : units in units

instance Enum Bool where
  succ False = True
  pred True  = False
  toEnum 0 = False
  toEnum 1 = True
  fromEnum False = 0
  fromEnum True  = 1
  enumFrom x = enumFromTo x True
  enumFromThen x y = enumFromThenTo x y (x <= y)

instance Enum Ordering where
  succ LT = EQ
  succ EQ = GT
  pred EQ = LT
  pred GT = EQ
  toEnum 0 = LT
  toEnum 1 = EQ
  toEnum 2 = GT
  fromEnum LT = 0
  fromEnum EQ = 1
  fromEnum GT = 2
  enumFrom x = enumFromTo x GT
  enumFromThen x y = enumFromThenTo x y (if x <= y then GT else LT)
