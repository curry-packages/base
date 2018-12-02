{-# LANGUAGE NoImplicitPrelude #-}
module Base.Real (Real(..)) where

import Base.Types
import Base.Num
import Base.Ord
import Base.Error

class (Num a, Ord a) => Real a where
  toFloat :: a -> Float

instance Real Int where
  toFloat x = fromInt x

instance Real Float where
  toFloat x = x
