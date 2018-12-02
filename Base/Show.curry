{-# LANGUAGE NoImplicitPrelude #-}
module Base.Show
  (ShowS, Show (..), shows, showChar, showString, showParen)
  where

import Base.Types
import Base.String

type ShowS = String -> String

class Show a where
  show :: a -> String
  showsPrec :: Int -> a -> ShowS
  showList :: [a] -> ShowS

  show x = shows x ""
  showsPrec _ x s = show x ++ s
  showList = showListDefault

instance Show Char where
  showsPrec _ c = showString (showCharLiteral c)
  showList cs | null cs   = showString "\"\""
              | otherwise = showString (showStringLiteral cs)

instance Show Int where
  showsPrec = showSigned (showString . showIntLiteral)

instance Show Float where
  showsPrec = showSigned (showString . showFloatLiteral)

instance Show () where
  showsPrec _ () = showString "()"

instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (a, b) = showTuple [shows a, shows b]

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (a, b, c) = showTuple [shows a, shows b, shows c]

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (a, b, c, d) = showTuple [shows a, shows b, shows c, shows d]

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
  showsPrec _ (a, b, c, d, e) =
    showTuple [shows a, shows b, shows c, shows d, shows e]

instance Show a => Show [a] where
  showsPrec _ = showList

instance Show Bool where
  showsPrec _ False = showString "False"
  showsPrec _ True  = showString "True"

instance Show Ordering where
  showsPrec _ LT = showString "LT"
  showsPrec _ EQ = showString "EQ"
  showsPrec _ GT = showString "GT"

shows :: Show a => a -> ShowS
shows = showsPrec 0

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString str s = foldr showChar s str

showListDefault :: Show a => [a] -> ShowS
showListDefault []     s = "[]" ++ s
showListDefault (x:xs) s = '[' : shows x (showl xs)
 where showl []     = ']' : s
       showl (y:ys) = ',' : shows y (showl ys)

showParen :: Bool -> ShowS -> ShowS
showParen b s = if b then showChar '(' . s . showChar ')' else s

showSigned :: Real a => (a -> ShowS) -> Int -> a -> ShowS
showSigned showPos p x
  | x < 0     = showParen (p > 6) (showChar '-' . showPos (-x))
  | otherwise = showPos x

showTuple :: [ShowS] -> ShowS
showTuple ss = showChar '('
             . foldr1 (\s r -> s . showChar ',' . r) ss
             . showChar ')'

-- Returns the string representation of a character.
showCharLiteral :: Char -> String
showCharLiteral x = prim_show $## x

-- Returns the string representation of a string.
showStringLiteral :: String -> String
showStringLiteral x = prim_show $## x

-- Returns the string representation of an integer.
showIntLiteral :: Int -> String
showIntLiteral x = prim_show $## x

-- Returns the string representation of a floating point number.
showFloatLiteral :: Float -> String
showFloatLiteral x = prim_show $## x

prim_show :: _ -> String
prim_show external
