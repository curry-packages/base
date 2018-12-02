{-# LANGUAGE NoImplicitPrelude #-}
module Base.Applicative (Applicative(..)) where

import Base.Types
import Base.Functor
import Base.Function
import Base.Error
import Base.List

infixl 4 <*>, <*, *>

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c

  (<*>) = liftA2 id
  a1 *> a2 = (id <$ a1) <*> a2
  (<*) = liftA2 const
  liftA2 f x = (<*>) (fmap f x)

instance Applicative [] where
  pure x    = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]
  xs *> ys  = [y | _ <- xs, y <- ys]
  liftA2 f xs ys = [f x y | x <- xs, y <- ys]

instance Applicative ((->) a) where
  pure = const
  (<*>) f g x = f x (g x)
  liftA2 q f g x = q (f x) (g x)
