{-# LANGUAGE NoImplicitPrelude #-}
module Base.Either (Either(..), either) where

import Base.Types
import Base.Functor
import Base.Applicative
import Base.Monad
import Base.Eq
import Base.Ord
import Base.Read
import Base.Show
import Base.List
import Base.Function

data  Either a b  =  Left a | Right b
  deriving (Eq, Ord, Read, Show)

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r

instance Monad (Either e) where
    Left  l >>= _ = Left l
    Right r >>= k = k r

either :: (a -> c) -> (b -> c) -> Either a b -> c
either l _ (Left  a) = l a
either _ r (Right b) = r b
