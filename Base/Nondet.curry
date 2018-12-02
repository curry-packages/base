{-# LANGUAGE NoImplicitPrelude #-}
module Base.Nondet ((?), anyOf, unknown) where

import Base.Types
import Base.List

infixr 0 ?

--- Non-deterministic choice _par excellence_.
--- The value of `x ? y` is either `x` or `y`.
(?) :: a -> a -> a
x ? _ = x
_ ? y = y

--- Returns non-deterministically any element of a list.
anyOf :: [a] -> a
anyOf = foldr1 (?)

--- Evaluates to a fresh free variable.
unknown :: _
unknown = let x free in x
