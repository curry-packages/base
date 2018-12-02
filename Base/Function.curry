{-# LANGUAGE NoImplicitPrelude #-}
module Base.Function
  ((.), id, const, asTypeOf, curry, uncurry, flip, until)
  where

import Base.Types

infixr 9 .

--- Function composition.
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

--- Identity function.
id :: a -> a
id x = x

--- Constant function.
const :: a -> _ -> a
const x _ = x

--- `asTypeOf` is a type-restricted version of `const`.
--- It is usually used as an infix operator, and its typing forces its first
--- argument (which is usually overloaded) to have the same type as the second.
asTypeOf :: a -> a -> a
asTypeOf = const

--- Converts an uncurried function to a curried function.
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b =  f (a, b)

--- Converts an curried function to a function on pairs.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

--- `flip f` is identical to `f`, but with the order of arguments reversed.
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

--- Repeats application of a function until a predicate holds.
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)
