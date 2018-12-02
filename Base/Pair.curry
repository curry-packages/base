{-# LANGUAGE NoImplicitPrelude #-}
module Base.Pair (fst, snd) where

import Base.Types

--- Selects the first component of a pair.
fst :: (a, _) -> a
fst (x, _) = x

--- Selects the second component of a pair.
snd :: (_, b) -> b
snd (_, y) = y
