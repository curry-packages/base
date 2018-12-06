{-# LANGUAGE NoImplicitPrelude #-}
module Base.Read (ReadS, Read (..), reads, readParen, read, lex) where

import Base.Types
import Base.List
import Base.Char
import Base.String
import Base.Real
import Base.Num
import Base.Bool
import Base.Eq
import Base.Ord
import Base.Eval
import Base.Error
import Base.Failed

type ReadS a = String -> [(a, String)]

class Read a where
  readsPrec :: Int -> ReadS a
  readList :: ReadS [a]

  readList = readListDefault

instance Read Char where
  readsPrec _ = readParen False
                  (\s -> [ (c, t) | (x, t) <- lex s, not (null x)
                                  , head x == '\''
                                  , (c, []) <- readCharLiteral x ])
  readList xs = readParen False
                  (\s -> [ (cs, t) | (x, t) <- lex s, not (null x)
                                   , head x == '"'
                                   , (cs, []) <- readStringLiteral x ]) xs
                ++ readListDefault xs

instance Read Int where
  readsPrec _ = readSigned (\s -> [ (i, t) | (x, t) <- lexDigits s
                                           , (i, []) <- readNatLiteral x ])

instance Read Float where
  readsPrec _ = readSigned
                  (\s -> [ (f, t) | (x, t) <- lex s, not (null x)
                                  , isDigit (head x), (f, []) <- readFloat x ])
   where
    readFloat x = if all isDigit x
                    then [(fromInt i, t) | (i, t) <- readNatLiteral x]
                    else readFloatLiteral x

instance Read () where
  readsPrec _ = readParen False (\r -> [ ((), t) | ("(", s) <- lex r
                                                 , (")", t) <- lex s ])

instance (Read a, Read b) => Read (a, b) where
  readsPrec _ = readParen False (\r -> [ ((a, b), w) | ("(", s) <- lex r
                                                     , (a, t) <- reads s
                                                     , (",", u) <- lex t
                                                     , (b, v) <- reads u
                                                     , (")", w) <- lex v ])

instance (Read a, Read b, Read c) => Read (a, b, c) where
  readsPrec _ = readParen False (\r -> [ ((a, b, c), y) | ("(", s) <- lex r
                                                        , (a, t) <- reads s
                                                        , (",", u) <- lex t
                                                        , (b, v) <- reads u
                                                        , (",", w) <- lex v
                                                        , (c, x) <- reads w
                                                        , (")", y) <- lex x ])

instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
  readsPrec _ = readParen False
                  (\q -> [ ((a, b, c, d), z) | ("(", r) <- lex q
                                             , (a, s) <- reads r
                                             , (",", t) <- lex s
                                             , (b, u) <- reads t
                                             , (",", v) <- lex u
                                             , (c, w) <- reads v
                                             , (",", x) <- lex w
                                             , (d, y) <- reads x
                                             , (")", z) <- lex y ])

instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
  readsPrec _ = readParen False
                  (\o -> [ ((a, b, c, d, e), z) | ("(", p) <- lex o
                                                , (a, q) <- reads p
                                                , (",", r) <- lex q
                                                , (b, s) <- reads r
                                                , (",", t) <- lex s
                                                , (c, u) <- reads t
                                                , (",", v) <- lex u
                                                , (d, w) <- reads v
                                                , (",", x) <- lex w
                                                , (e, y) <- reads x
                                                , (")", z) <- lex y ])

instance Read a => Read [a] where
  readsPrec _ = readList

instance Read Bool where
  readsPrec _ r =
    readParen False (\s -> [(False, t) | ("False", t) <- lex s]) r ++
      readParen False (\s -> [(True, t) | ("True", t) <- lex s]) r

instance Read Ordering where
  readsPrec _ r =
    readParen False (\s -> [(LT, t) | ("LT", t) <- lex s]) r ++
      readParen False (\s -> [(EQ, t) | ("EQ", t) <- lex s]) r ++
      readParen False (\s -> [(GT, t) | ("GT", t) <- lex s]) r

reads :: Read a => ReadS a
reads = readsPrec 0

readListDefault :: Read a => ReadS [a]
readListDefault = readParen False (\r -> [pr | ("[",s) <- lex r, pr <- readl s])
 where readl s = [([], t) | ("]", t) <- lex s] ++
                   [(x : xs, u) | (x, t) <- reads s, (xs, u) <- readl' t]
       readl' s = [([], t) | ("]", t) <- lex s] ++
                   [ (x : xs, v) | (",", t)  <- lex s, (x, u) <- reads t
                                 , (xs,v) <- readl' u ]

readParen :: Bool -> ReadS a -> ReadS a
readParen b g = if b then mandatory else optional
 where optional r = g r ++ mandatory r
       mandatory r =
         [(x, u) | ("(", s) <- lex r, (x, t) <- optional s, (")", u) <- lex t]

readSigned :: Real a => ReadS a -> ReadS a
readSigned p = readParen False read'
 where read' r = read'' r ++ [(-x, t) | ("-", s) <- lex r, (x, t) <- read'' s]
       read'' r = [(n, s) | (str, s) <- lex r, (n, "") <- p str]

read :: Read a => String -> a
read s =  case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> x

lex :: ReadS String
lex xs = case xs of
  ""                  -> [("", "")]
  (c:cs) | isSpace c  -> lex $ dropWhile isSpace cs
  ('\'':s)            ->
    [('\'' : ch ++ "'", t) | (ch, '\'' : t)  <- lexCharLiteral s, ch /= "'"]
  ('"':s)             -> [('"' : str, t) | (str, t) <- lexString s]
  (c:cs) | isSingle c -> [([c], cs)]
         | isSymbol c -> [(c : sym, t) | (sym, t) <- [span isSymbol cs]]
         | isAlpha c  -> [(c : nam, t) | (nam, t) <- [span isIdChar cs]]
         | isDigit c  -> [ (c : ds ++ fe, t) | (ds, s) <- [span isDigit cs]
                                             , (fe, t) <- lexFracExp s ]
         | otherwise  -> []
 where
  isSingle c = c `elem` ",;()[]{}_`"
  isSymbol c = c `elem` "!@#$%&⋆+./<=>?\\^|:-~"
  isIdChar c = isAlphaNum c || c `elem` "_'"
  lexFracExp s = case s of
    ('.':c:cs) | isDigit c ->
      [('.' : ds ++ e, u) | (ds, t) <- lexDigits (c : cs), (e, u) <- lexExp t]
    _                      -> lexExp s
  lexExp s = case s of
    (e:cs) | e `elem` "eE" ->
      [ (e : c : ds, u) | (c:t) <- [cs], c `elem` "+-"
                        , (ds, u) <- lexDigits t ] ++
        [(e : ds, t) | (ds, t) <- lexDigits cs]
    _                      -> [("", s)]
  lexString s = case s of
    ('"':cs) -> [("\"", cs)]
    _        -> [ (ch ++ str, u) | (ch, t) <- lexStringItem s
                                  , (str, u) <- lexString t ]
  lexStringItem s = case s of
    ('\\':'&':cs)           -> [("\\&", cs)]
    ('\\':c:cs) | isSpace c -> [("\\&", t) | '\\':t <- [dropWhile isSpace cs]]
    _                       -> lexCharLiteral s

lexCharLiteral :: ReadS String
lexCharLiteral xs = case xs of
  ""        -> []
  ('\\':cs) -> map (prefix '\\') (lexEsc cs)
  (c:cs)    -> [([c], cs)]
 where
  lexEsc s = case s of
    (c:cs) | c `elem` "abfnrtv\\\"'"  -> [([c], cs)]
    ('^':c:cs) | c >= '@' && c <= '_' -> [(['^', c], cs)]
    ('b':cs)                          -> [prefix 'b' (span isBinDigit cs)]
    ('o':cs)                          -> [prefix 'o' (span isOctDigit cs)]
    ('x':cs)                          -> [prefix 'x' (span isHexDigit cs)]
    cs@(d:_) | isDigit d              -> [span isDigit cs]
    cs@(c:_) | isUpper c              -> [span isCharName cs]
    _                                 -> []
  isCharName c = isUpper c || isDigit c
  prefix c (t, cs) = (c : t, cs)

lexDigits :: ReadS String
lexDigits s = [(cs, t) | (cs@(_:_), t) <- [span isDigit s]]

readCharLiteral :: ReadS Char
readCharLiteral s = prim_readCharLiteral $## s

prim_readCharLiteral :: String -> [(Char, String)]
prim_readCharLiteral external

readStringLiteral :: ReadS String
readStringLiteral s = prim_readStringLiteral $## s

prim_readStringLiteral :: String -> [(String, String)]
prim_readStringLiteral external

readNatLiteral :: ReadS Int
readNatLiteral s = prim_readNatLiteral $## s

prim_readNatLiteral :: String -> [(Int, String)]
prim_readNatLiteral external

readFloatLiteral :: ReadS Float
readFloatLiteral s = prim_readFloatLiteral $## s

prim_readFloatLiteral :: String -> [(Float, String)]
prim_readFloatLiteral external
