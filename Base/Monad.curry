{-# LANGUAGE NoImplicitPrelude #-}
module Base.Monad
  (Monad(..), ap, liftM2, sequence, sequence_, mapM, mapM_)
  where

import Base.Types
import Base.Applicative
import Base.Error
import Base.List
import Base.Function

infixl 1 >>, >>=

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a

  return = pure
  m >> k = m >>= \_ -> k
  fail = error

instance Monad [] where
  xs >>= f = [y | x <- xs, y <- f x]
  (>>) = (*>)
  fail _ = []

instance Monad ((->) r) where
  f >>= k = \ r -> k (f r) r

ap :: Monad m => m (a -> b) -> m a -> m b
ap m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (x1 x2)

liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do
  x1 <- m1
  x2 <- m2
  return (f x1 x2)

--- Executes a sequence of monadic actions and collects all results in a list.
sequence :: Monad m => [m a] -> m [a]
sequence []     = return []
sequence (c:cs) = do x <- c
                     xs <- sequence cs
                     return (x : xs)

--- Executes a sequence of monadic actions and ignores the results.
sequence_ :: Monad m => [m _] -> m ()
sequence_ = foldr (>>) (return ())

--- Maps a monadic action function on a list of elements.
--- The results of all monadic actions are collected in a list.
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

--- Maps an monadic action function on a list of elements.
--- The results of all monadic actions are ignored.
mapM_ :: Monad m => (a -> m _) -> [a] -> m ()
mapM_ f = sequence_ . map f
