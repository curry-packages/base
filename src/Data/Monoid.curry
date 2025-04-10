--- ----------------------------------------------------------------------------
--- Library with some useful `Monoid` instances.
---
--- @version April 2025
--- @category general
--- ----------------------------------------------------------------------------

module Data.Monoid
  ( All (..), Any (..)
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
