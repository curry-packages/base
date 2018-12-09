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
  , module Base.Pair
  , module Base.List
  , module Base.Maybe
  , module Base.IO
  , module Base.Constraint
  , module Base.Nondet
  , module Base.Failed
  , module Base.Error
  , module Base.Internal
  , module Base.Annotation
  , asTypeOf
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
import Base.Pair
import Base.List
import Base.Maybe
import Base.IO
import Base.Constraint
import Base.Nondet
import Base.Failed
import Base.Error
import Base.Internal
import Base.Annotation

default (Int, Float)

--- `asTypeOf` is a type-restricted version of `const`.
--- It is usually used as an infix operator, and its typing forces its first
--- argument (which is usually overloaded) to have the same type as the second.
asTypeOf :: a -> a -> a
asTypeOf = const
