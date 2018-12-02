{-# LANGUAGE NoImplicitPrelude #-}
module Base.Eval
  ( ($), ($!), ($!!), ($#), ($##), seq
  , ensureNotFree, ensureSpine, normalForm, groundNormalForm
  ) where

import Base.Types
import Base.Function

infixr 0 $, $!, $!!, $#, $##, `seq`

--- Right-associative application.
($) :: (a -> b) -> a -> b
f $ x = f x

--- Right-associative application with strict evaluation of its argument
--- to head normal form.
($!) :: (a -> b) -> a -> b
($!) external

--- Right-associative application with strict evaluation of its argument
--- to normal form.
($!!) :: (a -> b) -> a -> b
($!!) external

--- Right-associative application with strict evaluation of its argument
--- to a non-variable term.
($#) :: (a -> b) -> a -> b
f $# x = f $! (ensureNotFree x)

--- Right-associative application with strict evaluation of its argument
--- to ground normal form.
($##) :: (a -> b) -> a -> b
($##) external

--- Evaluates the first argument to head normal form (which could also
--- be a free variable) and returns the second argument.
seq :: _ -> a -> a
x `seq` y = const y $! x

--- Evaluates the argument to head normal form and returns it.
--- Suspends until the result is bound to a non-variable term.
ensureNotFree :: a -> a
ensureNotFree external

--- Evaluates the argument to spine form and returns it.
--- Suspends until the result is bound to a non-variable spine.
ensureSpine :: [a] -> [a]
ensureSpine l = ensureList (ensureNotFree l)
 where ensureList []     = []
       ensureList (x:xs) = x : ensureSpine xs

--- Evaluates the argument to normal form and returns it.
normalForm :: a -> a
normalForm x = id $!! x

--- Evaluates the argument to ground normal form and returns it.
--- Suspends as long as the normal form of the argument is not ground.
groundNormalForm :: a -> a
groundNormalForm x = id $## x
