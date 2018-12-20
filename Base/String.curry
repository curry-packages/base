{-# LANGUAGE NoImplicitPrelude #-}
module Base.String (String, lines, unlines, words, unwords) where

import Base.Types
import Base.Char
import Base.Eq
import Base.List
import Base.Function
import Base.Failed
import Base.Internal

type String = [Char]

--- Breaks a string into a list of lines where a line is terminated at a
--- newline character. The resulting lines do not contain newline characters.
lines :: String -> [String]
lines []       = []
lines as@(_:_) = let (l, bs) = splitLine as in l : lines bs
 where splitLine []     = ([], [])
       splitLine (c:cs) = if c == '\n' then ([], cs)
                                       else let (ds, es) = splitLine cs
                                            in (c : ds, es)

--- Concatenates a list of strings with terminating newlines.
unlines :: [String] -> String
unlines = concatMap (++ "\n")

--- Breaks a string into a list of words where the words are delimited by
--- white spaces.
words :: String -> [String]
words s = let s1 = dropWhile isSpace s
          in if s1 == "" then []
                         else let (w, s2) = break isSpace s1
                              in w : words s2

--- Concatenates a list of strings with a blank between two strings.
unwords :: [String] -> String
unwords ws = if ws == [] then []
                         else foldr1 (\w s -> w ++ ' ' : s) ws
