--------------------------------------------------------------------------------
--- The standard prelude of Curry with type classes.
--- All exported functions, data types, type classes
--- and methods defined in this module are always
--- available in any Curry program.
---
--- @category general
--------------------------------------------------------------------------------

module Prelude
  ( module Base.Types
  , module Base.Eq
  , module Base.Ord
  , module Base.Show
  , module Base.Read
  , module Base.Bounded
  , module Base.Enum
  , module Base.Num
  , module Base.Fractional
  , module Base.Real
  , module Base.Integral
  , module Base.RealFrac
  , module Base.Floating
  , module Base.Monoid
  , module Base.Functor
  , module Base.Applicative
  , module Base.Monad
  , module Base.String
  , module Base.Eval
  , module Base.Function
  , module Base.Bool
  , module Base.Either
  , module Base.List
  , module Base.Maybe
  , module Base.IO
  , module Base.Constraint
  , module Base.Failed
  , module Base.Error
  , module Base.Internal
  , asTypeOf, fst, snd
  , (?), anyOf, unknown
  , maybe, lookup
  ) where

import Base.Types
import Base.Eq
import Base.Ord
import Base.Show
import Base.Read
import Base.Bounded
import Base.Enum
import Base.Num
import Base.Fractional
import Base.Real
import Base.Integral
import Base.RealFrac
import Base.Floating
import Base.Monoid
import Base.Functor
import Base.Applicative
import Base.Monad
import Base.String
import Base.Eval
import Base.Function
import Base.Bool
import Base.Either
import Base.List
import Base.Maybe
import Base.IO
import Base.Constraint
import Base.Failed
import Base.Error
import Base.Internal

default (Int, Float)

--- `asTypeOf` is a type-restricted version of `const`.
--- It is usually used as an infix operator, and its typing forces its first
--- argument (which is usually overloaded) to have the same type as the second.
asTypeOf :: a -> a -> a
asTypeOf = const

--- Selects the first component of a pair.
fst :: (a, _) -> a
fst (x, _) = x

--- Selects the second component of a pair.
snd :: (_, b) -> b
snd (_, y) = y

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

--- Fold on the 'Maybe' Datatype
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x

--- Looks up a key in an association list.
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ []          = Nothing
lookup k ((x,y):xys) | k == x    = Just y
                   | otherwise = lookup k xys
