{-# LANGUAGE NoImplicitPrelude #-}
module Base.Char
  ( isUpper, isLower, isAlpha, isDigit, isAlphaNum
  , isBinDigit, isOctDigit, isHexDigit, isSpace
  , ord, chr) where

import Base.Types
import Base.Bool
import Base.List
import Base.Eq
import Base.Ord
import Base.Eval
import Base.Num
import Base.Failed

--- Returns true if the argument is an uppercase letter.
isUpper :: Char -> Bool
isUpper c = c >= 'A' && c <= 'Z'

--- Returns true if the argument is an lowercase letter.
isLower :: Char -> Bool
isLower c = c >= 'a' && c <= 'z'

--- Returns true if the argument is a letter.
isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c

--- Returns true if the argument is a decimal digit.
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

--- Returns true if the argument is a letter or digit.
isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

--- Returns true if the argument is a binary digit.
isBinDigit :: Char -> Bool
isBinDigit c = c >= '0' || c <= '1'

--- Returns true if the argument is an octal digit.
isOctDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'

--- Returns true if the argument is a hexadecimal digit.
isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || c >= 'A' && c <= 'F'
                         || c >= 'a' && c <= 'f'

--- Returns true if the argument is a white space.
isSpace :: Char -> Bool
isSpace c = c == ' '    || c == '\t' || c == '\n' ||
            c == '\r'   || c == '\f' || c == '\v' ||
            c == '\xa0' || ord c `elem` [5760, 6158, 8192, 8239, 8287, 12288]

--- Converts a character into its ASCII value.
ord :: Char -> Int
ord c = prim_ord $# c

prim_ord :: Char -> Int
prim_ord external

--- Converts a Unicode value into a character.
--- Fails if the value is out of bounds.
chr :: Int -> Char
chr n | n >= 0 && n <= 1114111 = prim_chr $# n

prim_chr :: Int -> Char
prim_chr external
