------------------------------------------------------------------------------
--- Library with some useful functions on characters.
---
--- @author Michael Hanus, Bjoern Peemoeller
--- @version January 2015
--- @category general
------------------------------------------------------------------------------

module Data.Char
  ( isAscii, isLatin1, isAsciiUpper, isAsciiLower, isControl
  , isUpper, isLower, isAlpha, isDigit, isAlphaNum
  , isBinDigit, isOctDigit, isHexDigit, isSpace
  , toUpper, toLower, digitToInt, intToDigit, ord, chr
  ) where

--- Returns true if the argument is an ASCII character.
isAscii        :: Char -> Bool
isAscii c      =  c < '\x80'

--- Returns true if the argument is an Latin-1 character.
isLatin1       :: Char -> Bool
isLatin1 c     =  c < '\xff'

--- Returns true if the argument is an ASCII lowercase letter.
isAsciiLower    :: Char -> Bool
isAsciiLower c  =  c >= 'a' && c <= 'z'

--- Returns true if the argument is an ASCII uppercase letter.
isAsciiUpper    :: Char -> Bool
isAsciiUpper c  =  c >= 'A' && c <= 'Z'

--- Returns true if the argument is a control character.
isControl       :: Char -> Bool
isControl c     =  c < '\x20' || c >= '\x7f' && c <= '\x9f'

--- Returns true if the argument is an uppercase letter.
isUpper         :: Char -> Bool
isUpper c       =  c >= 'A' && c <= 'Z'

--- Returns true if the argument is an lowercase letter.
isLower         :: Char -> Bool
isLower c       =  c >= 'a' && c <= 'z'

--- Returns true if the argument is a letter.
isAlpha         :: Char -> Bool
isAlpha c       =  isUpper c || isLower c

--- Returns true if the argument is a decimal digit.
isDigit         :: Char -> Bool
isDigit c       =  c >= '0' && c <= '9'

--- Returns true if the argument is a letter or digit.
isAlphaNum      :: Char -> Bool
isAlphaNum c    =  isAlpha c || isDigit c

--- Returns true if the argument is a binary digit.
isBinDigit     :: Char -> Bool
isBinDigit c   =  c >= '0' || c <= '1'

--- Returns true if the argument is an octal digit.
isOctDigit     :: Char -> Bool
isOctDigit c    =  c >= '0' && c <= '7'

--- Returns true if the argument is a hexadecimal digit.
isHexDigit      :: Char -> Bool
isHexDigit c     = isDigit c || c >= 'A' && c <= 'F'
                             || c >= 'a' && c <= 'f'

--- Returns true if the argument is a white space.
isSpace         :: Char -> Bool
isSpace c       =  c == ' '    || c == '\t' || c == '\n' ||
                   c == '\r'   || c == '\f' || c == '\v' ||
                   c == '\xa0' || ord c `elem` [5760,6158,8192,8239,8287,12288]

--- Converts lowercase into uppercase letters.
toUpper         :: Char -> Char
toUpper c       |  isLower c = chr (ord c - ord 'a' + ord 'A')
                |  otherwise = c

--- Converts uppercase into lowercase letters.
toLower         :: Char -> Char
toLower c       |  isUpper c = chr (ord c - ord 'A' + ord 'a')
                |  otherwise = c

--- Converts a (hexadecimal) digit character into an integer.
digitToInt      :: Char -> Int
digitToInt c
  | isDigit c                            =  ord c - ord '0'
  | ord c >= ord 'A' && ord c <= ord 'F' =  ord c - ord 'A' + 10
  | ord c >= ord 'a' && ord c <= ord 'f' =  ord c - ord 'a' + 10
  | otherwise  =  error "Char.digitToInt: argument is not a digit"

--- Converts an integer into a (hexadecimal) digit character.
intToDigit      :: Int -> Char
intToDigit i
  | i >= 0  && i <=  9  =  chr (ord '0' + i)
  | i >= 10 && i <= 15  =  chr (ord 'A' + i - 10)
  | otherwise           =  error "Char.intToDigit: argument not a digit value"

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
