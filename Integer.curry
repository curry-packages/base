------------------------------------------------------------------------------
--- A collection of common operations on integer numbers.
--- Most operations make no assumption on the precision of integers.
--- Operation `bitNot` is necessarily an exception.
---
--- @author Sergio Antoy
--- @version October 2016
--- @category general
------------------------------------------------------------------------------
{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module Integer
  ( (^), pow, ilog, isqrt, factorial, binomial
  , max3, min3, maxlist, minlist
  , bitTrunc, bitAnd, bitOr, bitNot, bitXor
  , even, odd ) where

infixr 8 ^

------------------------------------------------------------------
--                       Public Operations
------------------------------------------------------------------

--- The value of `a ^ b` is `a` raised to the power of `b`.
--- Fails if `b &lt; 0`.
--- Executes in `O(log b)` steps.
---
--- @param a - The base.
--- @param b - The exponent.
--- @return `a` raised to the power of `b`.

(^) :: Int -> Int -> Int
a ^ b = pow a b

--- The value of `pow a b` is `a`
--- raised to the power of `b`.
--- Fails if `b &lt; 0`.
--- Executes in `O(log b)` steps.
---
--- @param a - The base.
--- @param b - The exponent.
--- @return `a` raised to the power of `b`.

pow :: Int -> Int -> Int
pow a b | b>= 0 = powaux 1 a b
 where
  powaux n x y = if y == 0
                   then n
                   else powaux (n * if (y `mod` 2 == 1) then x else 1)
                               (x * x)
                               (y `div` 2)

--- The value of `ilog n` is the floor of the logarithm
--- in the base 10 of `n`.
--- Fails if `n &lt;= 0`.
--- For positive integers, the returned value is
--- 1 less the number of digits in the decimal representation of `n`.
---
--- @param n - The argument.
--- @return the floor of the logarithm in the base 10 of `n`.

ilog :: Int -> Int
ilog n | n>0 = if n<10 then 0 else 1 + ilog (n `div` 10)

--- The value of `isqrt n` is the floor
--- of the square root of `n`.
--- Fails if `n &lt; 0`.
--- Executes in `O(log n)` steps, but there must be a better way.
---
--- @param n - The argument.
--- @return the floor of the square root of `n`.

isqrt :: Int -> Int
isqrt n | n >= 0 = if n == 0 then 0
                             else if n <  4 then 1
                                            else aux 2 n
 where
  aux low past = -- invariant low <= result < past
    if past == low+1
      then low
      else let cand = (past + low) `div` 2
           in if cand*cand > n then aux low cand else aux cand past

--- The value of `factorial n` is the factorial of `n`.
--- Fails if `n &lt; 0`.
---
--- @param n - The argument.
--- @return the factorial of `n`.

factorial :: Int -> Int
factorial n | n >= 0 = if n == 0 then 1 else n * factorial (n-1)

--- The value of `binomial n m` is `n*(n-1)*...*(n-m+1)/m*(m-1)*...1`.
--- Fails if `m &lt;= 0` or `n &lt; m`.
---
--- @param n - Argument.
--- @param m - Argument.
--- @return the binomial coefficient of `n` over `m`.

binomial :: Int -> Int -> Int
binomial n m | m > 0 && n >= m = aux m n `div` factorial m
  where aux x y = if x == 0 then 1 else y * aux (x-1) (y-1)

--- Returns the maximum of the three arguments.
---
--- @param n - Argument.
--- @param m - Argument.
--- @param p - Argument.
--- @return the maximum among `n`, `m` and `p`.

max3 :: Ord a => a -> a -> a -> a
max3 n m p = max n (max m p)

--- Returns the minimum of the three arguments.
---
--- @param n - Argument.
--- @param m - Argument.
--- @param p - Argument.
--- @return the minimum among `n`, `m` and `p`.

min3 :: Ord a => a -> a -> a -> a
min3 n m p = min n (min m p)

--- Returns the maximum of a list of integer values.
--- Fails if the list is empty.
---
--- @param l - The list of values.
--- @return the maximum element of `l`.

maxlist :: Ord a => [a] -> a
maxlist [n]      = n
maxlist (n:m:ns) = max n (maxlist (m:ns))

--- Returns the minimum of a list of integer values.
--- Fails if the list is empty.
---
--- @param l - The list of values.
--- @return the minimum element of `l`.

minlist :: Ord a => [a] -> a
minlist [n]      = n
minlist (n:m:ns) = min n  (minlist (m:ns))

--- The value of `bitTrunc n m` is the value of the `n`
--- least significant bits of `m`.
---
--- @param n - Argument.
--- @param m - Argument.
--- @return `m` truncated to the `n` least significant bits.

bitTrunc :: Int -> Int -> Int
bitTrunc n m = bitAnd (pow 2 n - 1) m

--- Returns the bitwise AND of the two arguments.
---
--- @param n - Argument.
--- @param m - Argument.
--- @return the bitwise and of `n` and `m`.

bitAnd :: Int -> Int -> Int
bitAnd n m = if m == 0
               then 0
               else let p = 2 * bitAnd (n `div` 2) (m `div` 2)
                        q = if m `mod` 2 == 0 then 0 else n `mod` 2
                    in p + q

--- Returns the bitwise inclusive OR of the two arguments.
---
--- @param n - Argument.
--- @param m - Argument.
--- @return the bitwise inclusive or of `n` and `m`.

bitOr :: Int -> Int -> Int
bitOr n m = if m == 0
              then n
              else let p = 2 * bitOr (n `div` 2) (m `div` 2)
                       q = if m `mod` 2 == 1 then 1 else n `mod` 2
                   in p + q

--- Returns the bitwise NOT of the argument.
--- Since integers have unlimited precision,
--- only the 32 least significant bits are computed.
---
--- @param n - Argument.
--- @return the bitwise negation of `n` truncated to 32 bits.

bitNot :: Int -> Int
bitNot n = aux 32 n
 where
  aux c m = if c==0
              then 0
              else let p = 2 * aux (c-1) (m `div` 2)
                       q = 1 - m `mod` 2
                   in p + q

--- Returns the bitwise exclusive OR of the two arguments.
---
--- @param n - Argument.
--- @param m - Argument.
--- @return the bitwise exclusive of `n` and `m`.

bitXor :: Int -> Int -> Int
bitXor n m = if m == 0
               then n
               else let p = 2 * bitXor (n `div` 2) (m `div` 2)
                        q = if m `mod` 2 == n `mod` 2 then 0 else 1
                    in p + q

--- Returns whether an integer is even
---
--- @param n - Argument.
--- @return whether `n` is even.

even :: Int -> Bool
even n = n `mod` 2 == 0

--- Returns whether an integer is odd
---
--- @param n - Argument.
--- @return whether `n` is odd.

odd :: Int -> Bool
odd n = n `mod` 2 /= 0
