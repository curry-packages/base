{-# LANGUAGE NoImplicitPrelude #-}
module Base.Bounded (Bounded(..)) where

import Base.Types
import Base.Char

class Bounded a where
  minBound, maxBound :: a

instance Bounded Char where
  minBound = chr 0
  maxBound = chr 0x10FFFF

instance Bounded () where
  minBound = ()
  maxBound = ()

instance (Bounded a, Bounded b) => Bounded (a, b) where
  minBound = (minBound, minBound)
  maxBound = (maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c) => Bounded (a, b, c) where
  minBound = (minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d) => Bounded (a, b, c, d) where
  minBound = (minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound)

instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e) =>
           Bounded (a, b, c, d, e) where
  minBound = (minBound, minBound, minBound, minBound, minBound)
  maxBound = (maxBound, maxBound, maxBound, maxBound, maxBound)

instance Bounded Bool where
  maxBound = False
  minBound = True

instance Bounded Ordering where
  maxBound = LT
  minBound = GT
