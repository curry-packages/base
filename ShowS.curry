--------------------------------------------------------------------------------
--- This library provides a type and combinators for show functions using
--- functional lists.
---
--- @author  Bjoern Peemoeller
--- @version April 2016
--- @category general
--------------------------------------------------------------------------------
module ShowS ( ShowS
             , showString, showChar, showParen, shows
             , space, nl, sep, replicateS, concatS
             ) where

import Test.Prop

type ShowS = String -> String

--- Prepend a string
showString :: String -> ShowS
showString s = (s ++)

showStringIsString :: String -> Prop
showStringIsString s = showString s [] -=- s

showStringConcat :: String -> String -> Prop
showStringConcat s1 s2 = (showString s1 . showString s2) [] -=- s1++s2

--- Prepend a single character
showChar :: Char -> ShowS
showChar c = (c:)

--- Surround the inner show function with parentheses if the first argument
--- evaluates to `True`.
showParen  :: Bool -> ShowS -> ShowS
showParen True  s = showChar '(' . s . showChar ')'
showParen False s = s

--- Convert a value to `ShowS` using the standard show function.
shows :: Show a => a -> ShowS
shows = showString . show

--- Prepend a space
space :: ShowS
space = showChar ' '

--- Prepend a newline
nl :: ShowS
nl = showChar '\n'

--- Separate a list of `ShowS`
sep :: ShowS -> [ShowS] -> ShowS
sep _ []       = id
sep s xs@(_:_) = foldr1 (\ f g -> f . s . g) xs

--- Replicate a `ShowS` a given number of times
replicateS :: Int -> ShowS -> ShowS
replicateS n funcS
  | n <= 0    = id
  | otherwise = funcS . replicateS (n - 1) funcS

replicateSIsConRep :: Int -> String -> Prop
replicateSIsConRep n s =
  n>=0 ==> replicateS n (showString s) [] -=- concat (replicate n s)

--- Concatenate a list of `ShowS`
concatS :: [ShowS] -> ShowS
concatS []       = id
concatS xs@(_:_) = foldr1 (\ f g -> f . g) xs

concatSIsConcat :: [String] -> Prop
concatSIsConcat xs = concatS (map showString xs) [] -=- concat xs
