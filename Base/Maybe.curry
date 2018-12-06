{-# LANGUAGE NoImplicitPrelude #-}
module Base.Maybe (Maybe(..), maybe, lookup) where

import Base.Monoid
import Base.Functor
import Base.Applicative
import Base.Monad
import Base.Eq
import Base.Ord
import Base.Read
import Base.Show
import Base.Bool
import Base.Function
import Base.List
import Base.Failed

data Maybe a = Nothing | Just a
 deriving (Eq, Ord, Show, Read)

instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m       = m
  Just m1 `mappend` Nothing = Just m1
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  Just f  <*> m = fmap f m
  Nothing <*> _ = Nothing
  Just _  *> m = m
  Nothing *> _ = Nothing
  liftA2 f (Just x) (Just y) = Just (f x y)
  liftA2 _ (Just _) Nothing  = Nothing
  liftA2 _ Nothing  _        = Nothing

instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x  >>= k = k x
  (>>) = (*>)
  fail _ = Nothing

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x

-- TODO: Not defined in List to avoid cyclic dependency with Monoid -> Maybe -> List
--- Looks up a key in an association list.
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ []          = Nothing
lookup k ((x,y):xys) | k == x    = Just y
                     | otherwise = lookup k xys
