module Control.Monad.Fix
  ( MonadFix (..)
  ) where

import Data.Function (fix)

--- Monads with a fixed point operator.
class Monad m => MonadFix m where
  --- The fixed point of a monadic computation. The passed function is only
  --- executed once and takes the eventual output of the computation. Note
  --- however that the passed function must be lazy in its argument,
  --- otherwise the result will diverge.
  mfix :: (a -> m a) -> m a

instance MonadFix [] where
  mfix f = case fix (f . head) of
    [] -> []
    (x:_) -> x : mfix (tail . f)

instance MonadFix ((->) r) where
  mfix f = \r -> let a = f a r in a

instance MonadFix Maybe where
  mfix f = let a = f (unJust a) in a
    where
      unJust a = case a of
        Just x  -> x
        Nothing -> error "mfix Maybe: Nothing"

instance MonadFix (Either e) where
  mfix f = let a = f (unRight a) in a
    where
      unRight a = case a of
        Right x -> x
        Left _  -> error "mfix Either: Left"
