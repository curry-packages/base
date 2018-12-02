{-# LANGUAGE NoImplicitPrelude #-}
module Base.Bool ((&&), (||), not, otherwise, ifThenElse) where

import Base.Types

infixr 3 &&
infixr 2 ||

--- Sequential conjunction on Booleans.
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

--- Sequential disjunction on Booleans.
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

--- Negation on Booleans.
not :: Bool -> Bool
not True  = False
not False = True

--- Useful name for the last condition in a sequence of conditional equations.
otherwise :: Bool
otherwise = True

--- The standard conditional. It suspends if the condition is a free variable.
ifThenElse :: Bool -> a -> a -> a
ifThenElse b t f = case b of True  -> t
                             False -> f
