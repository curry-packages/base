------------------------------------------------------------------------------
--- A collection of useful functions for sorting and comparing
--- characters, strings, and lists.
---
--- @author Michael Hanus
--- @version April 2016
--- @category algorithm
------------------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-overlapping #-}

module Sort( sort, sortBy, sorted, sortedBy
           , permSort, permSortBy, insertionSort, insertionSortBy
           , quickSort, quickSortBy, mergeSort, mergeSortBy
           , cmpChar, cmpList, cmpString
           , leqChar, leqCharIgnoreCase, leqList
           , leqString, leqStringIgnoreCase, leqLexGerman
           ) where

import Char
import Test.Prop

--- The default sorting operation, mergeSort, with standard ordering `<=`.
sort :: [a] -> [a]
sort = sortBy (<=)

-- Postcondition: input and output lists have same length and output is sorted.
sort'post :: [a] -> [a] -> Bool
sort'post xs ys = length xs == length ys && sorted ys

-- Specification via permutation sort:
sort'spec :: [a] -> [a]
sort'spec xs = permSort xs


--- The default sorting operation: mergeSort
sortBy :: (a -> a -> Bool) -> [a] -> [a]
sortBy = mergeSortBy

--- `sorted xs` is satisfied if the elements `xs` are in ascending order.
sorted :: [a] -> Bool
sorted = sortedBy (<=)

--- `sortedBy leq xs` is satisfied if all adjacent elements of the list `xs`
--- satisfy the ordering predicate `leq`.
sortedBy :: (a -> a -> Bool) -> [a] -> Bool
sortedBy _   []       = True
sortedBy _   [_]      = True
sortedBy leq (x:y:ys) = leq x y && sortedBy leq (y:ys)


------------------------------------------------------------------------------
--- Permutation sort with standard ordering `<=`.
--- Sorts a list by finding a sorted permutation
--- of the input. This is not a usable way to sort a list but it can be used
--- as a specification of other sorting algorithms.
permSort :: [a] -> [a]
permSort = permSortBy (<=)

--- Permutation sort with ordering as first parameter.
--- Sorts a list by finding a sorted permutation
--- of the input. This is not a usable way to sort a list but it can be used
--- as a specification of other sorting algorithms.
permSortBy :: (a -> a -> Bool) -> [a] -> [a]
permSortBy leq xs | sortedBy leq ys = ys
 where ys = perm xs

--- Computes a permutation of a list.
perm :: [a] -> [a]
perm []     = []
perm (x:xs) = insert (perm xs)
 where insert ys     = x : ys
       insert (y:ys) = y : insert ys

------------------------------------------------------------------------------
--- Insertion sort with standard ordering `<=`.
--- The list is sorted by repeated sorted insertion of the elements
--- into the already sorted part of the list.
insertionSort  :: [a] -> [a]
insertionSort = insertionSortBy (<=)

-- Postcondition: input and output lists have same length and output is sorted.
insertionSort'post :: [a] -> [a] -> Bool
insertionSort'post xs ys = length xs == length ys && sorted ys

-- Specification via permutation sort:
insertionSort'spec :: [a] -> [a]
insertionSort'spec = permSort


--- Insertion sort with ordering as first parameter.
--- The list is sorted by repeated sorted insertion of the elements
--- into the already sorted part of the list.
insertionSortBy  :: (a -> a -> Bool) -> [a] -> [a]
insertionSortBy _ [] = []
insertionSortBy leq (x:xs) = insert (insertionSortBy leq xs)
 where
  insert [] = [x]
  insert zs@(y:ys) | leq x y   = x : zs
                   | otherwise = y : insert ys


------------------------------------------------------------------------------
--- Quicksort with standard ordering `<=`.
--- The classical quicksort algorithm on lists.
quickSort :: [a] -> [a]
quickSort = quickSortBy (<=)

-- Postcondition: input and output lists have same length and output is sorted.
quickSort'post :: [a] -> [a] -> Bool
quickSort'post xs ys = length xs == length ys && sorted ys

-- Specification via permutation sort:
quickSort'spec :: [a] -> [a]
quickSort'spec = permSort


--- Quicksort with ordering as first parameter.
--- The classical quicksort algorithm on lists.
quickSortBy :: (a -> a -> Bool) -> [a] -> [a]
quickSortBy _   []     = []
quickSortBy leq (x:xs) = let (l,r) = split x xs
                          in quickSortBy leq l ++ (x : quickSortBy leq r)
 where
  split _ [] = ([],[])
  split e (y:ys) | leq y e   = (y:l,r)
                 | otherwise = (l,y:r)
              where (l,r) = split e ys


------------------------------------------------------------------------------
--- Bottom-up mergesort with standard ordering `<=`.
mergeSort :: [a] -> [a]
mergeSort = mergeSortBy (<=)

-- Postcondition: input and output lists have same length and output is sorted.
mergeSort'post :: [a] -> [a] -> Bool
mergeSort'post xs ys = length xs == length ys && sorted ys

-- Specification via permutation sort:
mergeSort'spec :: [a] -> [a]
mergeSort'spec = permSort


--- Bottom-up mergesort with ordering as first parameter.
mergeSortBy :: (a -> a -> Bool) -> [a] -> [a]
mergeSortBy leq zs =  mergeLists (genRuns zs)
 where
  -- generate runs of length 2:
  genRuns []               =  []
  genRuns [x]              =  [[x]]
  genRuns (x1:x2:xs) | leq x1 x2 =  [x1,x2] : genRuns xs
                     | otherwise =  [x2,x1] : genRuns xs

  -- merge the runs:
  mergeLists []         =  []
  mergeLists [x]        =  x
  mergeLists (x1:x2:xs) =  mergeLists (merge leq x1 x2 : mergePairs xs)

  mergePairs []         =  []
  mergePairs [x]        =  [x]
  mergePairs (x1:x2:xs) =  merge leq x1 x2 : mergePairs xs


--- Merges two lists with respect to an ordering predicate.

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _   [] ys     = ys
merge _   (x:xs) [] = x : xs
merge leq (x:xs) (y:ys) | leq x y   = x : merge leq xs (y:ys)
                        | otherwise = y : merge leq (x:xs) ys


------------------------------------------------------------------------------
-- Comparing lists, characters and strings

--- Less-or-equal on lists.
leqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
leqList _   []     _      = True
leqList _   (_:_)  []     = False
leqList leq (x:xs) (y:ys) | x == y    = leqList leq xs ys
                          | otherwise = leq x y

--- Comparison of lists.
cmpList :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
cmpList _   []     []     = EQ
cmpList _   []     (_:_)  = LT
cmpList _   (_:_)  []     = GT
cmpList cmp (x:xs) (y:ys) | cmp x y == EQ = cmpList cmp xs ys
                          | otherwise     = cmp x y

--- Less-or-equal on characters (deprecated, use 'Prelude.<=').
leqChar :: Char -> Char -> Bool
leqChar = (<=)

--- Comparison of characters (deprecated, use 'Prelude.compare').
cmpChar :: Char -> Char -> Ordering
cmpChar = compare

--- Less-or-equal on characters ignoring case considerations.
leqCharIgnoreCase :: Char -> Char -> Bool
leqCharIgnoreCase c1 c2 = (toUpper c1) <= (toUpper c2)

--- Less-or-equal on strings (deprecated, use 'Prelude.<=').
leqString :: String -> String -> Bool
leqString = (<=)

--- Comparison of strings (deprecated, use 'Prelude.compare').
cmpString :: String -> String -> Ordering
cmpString = compare

--- Less-or-equal on strings ignoring case considerations.
leqStringIgnoreCase :: String -> String -> Bool
leqStringIgnoreCase = leqList leqCharIgnoreCase

--- Lexicographical ordering on German strings.
--- Thus, upper/lowercase are not distinguished and Umlauts are sorted
--- as vocals.
leqLexGerman :: String -> String -> Bool
leqLexGerman []    _  = True
leqLexGerman (_:_) [] = False
leqLexGerman (x:xs) (y:ys) | x' == y'  = leqLexGerman xs ys
                           | otherwise = x' < y'
  where
    x' = glex (ord x)
    y' = glex (ord y)
    -- map umlauts to vocals and make everything lowercase:
    glex o | o >= ord 'A' && o <= ord 'Z'  =  o + (ord 'a' - ord 'A')
           | o == 228  = ord 'a'
           | o == 246  = ord 'o'
           | o == 252  = ord 'u'
           | o == 196  = ord 'a'
           | o == 214  = ord 'o'
           | o == 220  = ord 'u'
           | o == 223  = ord 's'
           | otherwise = o

-- end module Sort
