{-# LANGUAGE NoImplicitPrelude, CPP #-}
module Base.Constraint (
  Success, success, solve, doSolve, (=:=), (=:<=)
#ifdef __PAKCS__
  , (=:<<=)
#endif
  , (&), (&>)
  ) where

import Base.Types
import Base.IO

infix 4 =:=, =:<=
#ifdef __PAKCS__
infix 4 =:<<=
#endif
infixr 0 &, &>

type Success = Bool

--- The always satisfiable constraint.
success :: Success
success = True

--- Enforce a Boolean condition to be true.
--- The computation fails if the argument evaluates to `False`.
solve :: Bool -> Bool
solve True = True

--- Solves a constraint as an I/O action.
--- Note: The constraint should be always solvable in a deterministic way.
doSolve :: Bool -> IO ()
doSolve b | b = return ()

--- The equational constraint.
--- `(e1 =:= e2)` is satisfiable if both sides `e1` and `e2` can be
--- reduced to a unifiable data term (i.e., a term without defined
--- function symbols).
--TODO: note
(=:=) :: Eq a => a -> a -> Bool
(=:=) external

--- Non-strict equational constraint. Used to implement functional patterns.
(=:<=) :: a -> a -> Bool
(=:<=) external

#ifdef __PAKCS__
--- Non-strict equational constraint for linear functional patterns.
--- Thus, it must be ensured that the first argument is always (after evalutation
--- by narrowing) a linear pattern. Experimental.
(=:<<=) :: a -> a -> Bool
(=:<<=) external

--- internal function to implement =:<=
ifVar :: _ -> a -> a -> a
ifVar external
#endif

--- Concurrent conjunction.
--- An expression like `(c1 & c2)` is evaluated by evaluating
--- the `c1` and `c2` in a concurrent manner.
(&) :: Bool -> Bool -> Bool
(&) external

--- Conditional expression.
--- An expression like `(c &> e)` is evaluated by evaluating the first
--- argument to `True` and then evaluating `e`.
--- The expression has no value if the condition does not evaluate to `True`.
(&>) :: Bool -> a -> a
True &> x = x
