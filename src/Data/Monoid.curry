--- ----------------------------------------------------------------------------
--- Library with some useful `Monoid` instances.
---
--- @version April 2025
--- @category general
--- ----------------------------------------------------------------------------

module Data.Monoid
  ( All (..), Any (..), Sum (..), Product (..)
  ) where

--- Boolean monoid under (&&)
newtype All = All { getAll :: Bool }
  deriving (Eq, Ord, Show, Read)

instance Monoid All where
  mempty = All True
  mappend (All x) (All y) = All (x && y)

--- Boolean monoid under (||)
newtype Any = Any { getAny :: Bool }
  deriving (Eq, Ord, Show, Read)

instance Monoid Any where
  mempty = Any False
  mappend (Any x) (Any y) = Any (x || y)

--- Monoid under addition.
newtype Sum a = Sum { getSum :: a }
  deriving (Eq, Ord, Show, Read)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)

--- Monoid under multiplication.
newtype Product a = Product { getProduct :: a }
  deriving (Eq, Ord, Show, Read)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x * y)
