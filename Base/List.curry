{-# LANGUAGE NoImplicitPrelude #-}
module Base.List
  ( head, tail, null, (++), length, (!!), map, foldl, foldl1, foldr, foldr1
  , filter, zip, zip3, zipWith, zipWith3, unzip, unzip3, concat, concatMap
  , iterate, repeat, replicate, take, drop, splitAt, takeWhile, dropWhile
  , span, break, reverse, and, or, any, all, elem, notElem) where

import Base.Types
import Base.Bool
import Base.Eq
import Base.Ord
import Base.Num
import Base.Function
import Base.Eval
import Base.Failed

infixr 5 ++
infixl 9 !!
infix 4 `elem`, `notElem`

--- Computes the first element of a list.
head :: [a] -> a
head (x:_) = x

--- Computes the remaining elements of a list.
tail :: [a] -> [a]
tail (_:xs) = xs

--- Is a list empty?
null :: [_] -> Bool
null []    = True
null (_:_) = False

--- Concatenates two lists.
--- Since it is flexible, it could be also used to split a list
--- into two sublists etc.
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

--- Computes the length of a list.
length :: [_] -> Int
length xs = len xs 0
 where len []     n = n
       len (_:ys) n = let np1 = n + 1 in len ys $!! np1

--- List index (subscript) operator, head has index 0.
(!!) :: [a] -> Int -> a
(x:xs) !! n | n == 0 = x
            | n > 0  = xs !! (n - 1)

--- Maps a function on all elements of a list.
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

--- Accumulates all list elements by applying a binary operator from
--- left to right.
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

--- Accumulates a non-empty list from left to right.
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs

--- Accumulates all list elements by applying a binary operator from
--- right to left.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

--- Accumulates a non-empty list from right to left:
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]          = x
foldr1 f (x:xs@(_:_)) = f x (foldr1 f xs)

--- Filters all elements satisfying a given predicate in a list.
filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) = if p x then x : filter p xs
                         else filter p xs

--- Joins two lists into one list of pairs. If one input list is shorter than
--- the other, the additional elements of the longer list are discarded.
zip :: [a] -> [b] -> [(a, b)]
zip []     _      = []
zip (_:_)  []     = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

--- Joins three lists into one list of triples. If one input list is shorter
--- than the other, the additional elements of the longer lists are discarded.
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 []     _      _      = []
zip3 (_:_)  []     _      = []
zip3 (_:_)  (_:_)  []     = []
zip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3 xs ys zs

--- Joins two lists into one list by applying a combination function to
--- corresponding pairs of elements. Thus `zip = zipWith (,)`
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ []     _      = []
zipWith _ (_:_)  []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

--- Joins three lists into one list by applying a combination function to
--- corresponding triples of elements. Thus `zip3 = zipWith3 (,,)`
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 _ []     _      _      = []
zipWith3 _ (_:_)  []     _      = []
zipWith3 _ (_:_)  (_:_)  []     = []
zipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 f xs ys zs

--- Transforms a list of pairs into a pair of lists.
unzip :: [(a, b)] -> ([a], [b])
unzip []          = ([], [])
unzip ((x, y):ps) = (x : xs, y : ys)
 where (xs, ys) = unzip ps

--- Transforms a list of triples into a triple of lists.
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 []             = ([], [], [])
unzip3 ((x, y, z):ts) = (x : xs, y : ys, z : zs)
 where (xs, ys, zs) = unzip3 ts

--- Concatenates a list of lists into one list.
concat :: [[a]] -> [a]
concat = foldr (++) []

--- Maps a function from elements to lists and merges the result into one list.
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

--- Infinite list of repeated applications of a function f to an element x.
--- Thus, `iterate f x = [x, f x, f (f x), ...]`.
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

--- Infinite list where all elements have the same value.
--- Thus, `repeat x = [x, x, x, ...]`.
repeat :: a -> [a]
repeat x = x : repeat x

--- List of length n where all elements have the same value.
replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

--- Returns prefix of length n.
take :: Int -> [a] -> [a]
take n l = if n <= 0 then [] else takep n l
 where takep _ []     = []
       takep m (x:xs) = x : take (m - 1) xs

--- Returns suffix without first n elements.
drop :: Int -> [a] -> [a]
drop n xs = if n <= 0 then xs
                      else case xs of []     -> []
                                      (_:ys) -> drop (n - 1) ys

--- `splitAt n xs` is equivalent to `(take n xs, drop n xs)`
splitAt :: Int -> [a] -> ([a], [a])
splitAt n l = if n <= 0 then ([], l) else splitAtp n l
  where splitAtp _ []     = ([], [])
        splitAtp m (x:xs) = let (ys, zs) = splitAt (m - 1) xs in (x : ys, zs)

--- Returns longest prefix with elements satisfying a predicate.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

--- Returns suffix without takeWhile prefix.
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x : xs

--- `span p xs` is equivalent to `(takeWhile p xs, dropWhile p xs)`
span :: (a -> Bool) -> [a] -> ([a], [a])
span _ []     = ([], [])
span p (x:xs) | p x       = let (ys, zs) = span p xs in (x : ys, zs)
              | otherwise = ([], x : xs)

--- `break p xs` is equivalent to
--- `(takeWhile (not . p) xs, dropWhile (not . p) xs)`.
--- Thus, it breaks a list at the first occurrence of an element satisfying p.
break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

--- Reverses the order of all elements in a list.
reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

--- Computes the conjunction of a Boolean list.
and :: [Bool] -> Bool
and = foldr (&&) True

--- Computes the disjunction of a Boolean list.
or :: [Bool] -> Bool
or = foldr (||) False

--- Is there an element in a list satisfying a given predicate?
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

--- Is a given predicate satisfied by all elements in a list?
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

--- Element of a list?
elem :: Eq a => a -> [a] -> Bool
elem x = any (x ==)

--- Not element of a list?
notElem :: Eq a => a -> [a] -> Bool
notElem x = all (x /=)
