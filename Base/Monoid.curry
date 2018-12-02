{-# LANGUAGE NoImplicitPrelude #-}
module Base.Monoid (Monoid(..)) where

import Base.Types
import Base.List
import Base.Error

class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

  mconcat = foldr mappend mempty

instance Monoid () where
  mempty = ()
  _ `mappend` _ = ()
  mconcat _ = ()

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  (a1, b1) `mappend` (a2,b2) = (a1 `mappend` a2, b1 `mappend` b2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c) where
  mempty = (mempty, mempty, mempty)
  (a1, b1, c1) `mappend` (a2, b2, c2) =
    (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a, b, c, d) where
  mempty = (mempty, mempty, mempty, mempty)
  (a1, b1, c1, d1) `mappend` (a2, b2, c2, d2) =
    (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2, d1 `mappend` d2)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
           Monoid (a, b, c, d, e) where
  mempty = (mempty, mempty, mempty, mempty, mempty)
  (a1, b1, c1, d1, e1) `mappend` (a2, b2, c2, d2, e2) =
    (a1 `mappend` a2, b1 `mappend` b2, c1 `mappend` c2, d1 `mappend` d2,
      e1 `mappend` e2)

instance Monoid [a] where
  mempty  = []
  mappend = (++)
  mconcat xss = [x | xs <- xss, x <- xs]

instance Monoid b => Monoid (a -> b) where
  mempty _ = mempty
  mappend f g x = f x `mappend` g x

instance Monoid Ordering where
  mempty = EQ
  LT `mappend` _ = LT
  EQ `mappend` y = y
  GT `mappend` _ = GT
