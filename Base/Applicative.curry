{-# LANGUAGE NoImplicitPrelude #-}
module Base.Applicative (Applicative(..), when, sequenceA, sequenceA_) where

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

-- | Conditional execution of 'Applicative' expressions.
when :: (Applicative f) => Bool -> f () -> f ()
when p s  = if p then s else pure ()

--- Evaluate each action in the list from left to right, and
--- collect the results. For a version that ignores the results
--- see 'sequenceA_'.
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

--- Evaluate each action in the structure from left to right, and
--- ignore the results. For a version that doesn't ignore the results
--- see 'sequenceA'.
sequenceA_ :: (Applicative f) => [f a] -> f ()
sequenceA_ = foldr (*>) (pure ())
