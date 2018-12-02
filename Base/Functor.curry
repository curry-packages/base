{-# LANGUAGE NoImplicitPrelude #-}
module Base.Functor (Functor(..)) where

import Base.Types
import Base.Function
import Base.List
import Base.Error

infixl 4 <$

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a

  (<$) = fmap . const

instance Functor [] where
  fmap = map

instance Functor ((->) r) where
  fmap = (.)
