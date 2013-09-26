------------------------------------------------------------------------------
--- Library for finite domain constraint solving.
--- <p>
--- The general structure of a specification of an FD problem is as follows:
--- 
--- <code>fd variable bindings & domain_constraint & fd_constraint0 & ... & fd_constraintN & labeling</code>
--- 
--- where:
--- <code>fd variable bindings</code>
--- introduces possible bindings for FD variables using (=:=),
--- this part of the specification of a FD problem is optional
---
--- <code>domain_constraint</code>
--- specifies the possible range of the FD variables (see constraint <code>domain</code>)
--- 
--- <code>fd_constraint</code>
--- specifies the constraint to be satisfied by a valid solution
--- (see constraints #+, #-, allDifferent, etc below)
--- 
--- <code>labeling</code>
--- is a labeling function to search for a solution by enumerating possible variable bindings.
---

------------------------------------------------------------------------------

module CLPFD(domain,(+#),(-#),(*#),(=#), (/=#), (<#), (<=#), (>#), (>=#), allDifferent, sum, labeling, genVars) where

-- The operator declarations are similar to the standard arithmetic
-- and relational operators.

infixl 7 *#
infixl 6 +#, -#
infix  4 =#, /=#, <#, <=#, >#, >=#

-- Constraint to specify the domain of all finite domain variables
-- @param vs - list of FD variables
-- @param l - lower boundary for all variables in vs
-- @param u - upper boundary for all variables in vs
domain :: [Int] -> Int -> Int -> Success
domain vs l u = ((prim_domain $!! (ensureSpine vs)) $!! l) $!! u

prim_domain :: [Int] -> Int -> Int -> Success
prim_domain external

-- Addition of FD variables.
-- @result - free variable to which the result of x+#y is bound 
(+#)   :: Int -> Int -> Int
x +# y = ((prim_FD_plus $!! x) $!! y) result
 where result free

prim_FD_plus :: Int -> Int -> Int -> Int
prim_FD_plus external

-- Subtraction of FD variables.
-- @result - free variable to which the result of x-#y is bound
(-#)   :: Int -> Int -> Int
x -# y = ((prim_FD_minus $!! x) $!! y) result
 where result free

prim_FD_minus :: Int -> Int -> Int -> Int
prim_FD_minus external

-- Multiplication of FD variables.
-- @result - free variable to which the result of x*#y is bound
(*#)   :: Int -> Int -> Int
x *# y = ((prim_FD_times $!! x) $!! y) result
 where result free

prim_FD_times :: Int -> Int -> Int -> Int
prim_FD_times external

-- Equality of FD variables.
(=#)   :: Int -> Int -> Success
x =# y = (prim_FD_equal $!! x) $!! y

prim_FD_equal :: Int -> Int -> Success
prim_FD_equal external

-- Disequality of FD variables.
(/=#)  :: Int -> Int -> Success
x /=# y = (prim_FD_notequal $!! x) $!! y

prim_FD_notequal :: Int -> Int -> Success
prim_FD_notequal external

-- "Less than" constraint on FD variables.
(<#)   :: Int -> Int -> Success
x <# y = (prim_FD_le $!! x) $!! y

prim_FD_le :: Int -> Int -> Success
prim_FD_le external

-- "Less than or equal" constraint on FD variables.
(<=#)  :: Int -> Int -> Success
x <=# y = (prim_FD_leq $!! x) $!! y

prim_FD_leq :: Int -> Int -> Success
prim_FD_leq external

-- "Greater than" constraint on FD variables.
(>#)   :: Int -> Int -> Success
x ># y = (prim_FD_ge $!! x) $!! y

prim_FD_ge :: Int -> Int -> Success
prim_FD_ge external

-- "Greater than or equal" constraint on FD variables.
(>=#)  :: Int -> Int -> Success
x >=# y = (prim_FD_geq $!! x) $!! y

prim_FD_geq :: Int -> Int -> Success
prim_FD_geq external

-- "All different" constraint on FD variables.
-- @param vs - list of FD variables
-- @return satisfied if the FD variables in the argument list xs
--         have pairwise different values.
allDifferent :: [Int] -> Success
allDifferent vs = prim_allDifferent $!! (ensureSpine vs)

prim_allDifferent :: [Int] -> Success
prim_allDifferent external

-- "Sum" constraint on FD variables.
-- @param vs - list of FD variables
-- @return   - sum of given variables
-- @result   - free variable to which the result of sum vs is bound 
sum :: [Int] -> Int
sum vs = (prim_sum $!! (ensureSpine vs)) result
 where result free

prim_sum :: [Int] -> Int -> Int
prim_sum external

-- label FD variables in order
-- @param vs - list of FD variables (labeling variables)
-- @labelVar - KiCS2-internal the ID of this variable is used for binding solutions to labeling variables
labeling :: [Int] -> Success
labeling vs = (prim_labeling $!! (ensureSpine vs)) labelVar
 where labelVar free

prim_labeling :: [Int] -> [Int] -> Success
prim_labeling external

-- generate list of n free variables
genVars :: Int -> [Int]
genVars n = if n==0 then [] else var : genVars (n-1)
 where var free

-- end of library CLPFD
