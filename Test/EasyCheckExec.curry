-------------------------------------------------------------------------
--- EasyCheck is a library for automated, property-based testing of
--- Curry programs. The ideas behind EasyCheck are described in
--- [this paper](http://www-ps.informatik.uni-kiel.de/~sebf/pub/flops08.html)
--- This module implements the operations to actually execute
--- the tests.
---
--- @author Sebastian Fischer (with extensions by Michael Hanus)
--- @version April 2016
--- @category general
-------------------------------------------------------------------------

module Test.EasyCheckExec (

  -- test functions
  checkWithValues0, checkWithValues1, checkWithValues2,
  checkWithValues3, checkWithValues4, checkWithValues5,
  check0, check1, check2, check3, check4, check5,
  easyCheck0, easyCheck1, easyCheck2, easyCheck3, easyCheck4, easyCheck5,
  verboseCheck0, verboseCheck1, verboseCheck2, verboseCheck3, verboseCheck4,
  verboseCheck5,

  --easyCheck', easyCheck1', easyCheck2', easyCheck3', easyCheck4', easyCheck5',

  -- operations used by the CurryCheck tool
  checkPropWithMsg, checkPropIOWithMsg
  ) where

import AllSolutions ( getAllValues )
import Distribution ( curryCompiler )
import List         ( group, intersperse, nub )
import Sort         ( leqList, leqString, mergeSort )
import Test.EasyCheck

-------------------------------------------------------------------------
-- Test Functions

-- Note that this does not work with PAKCS! However, if CurryCheck is used,
-- this operation is not replaced by explicit generator operations.
suc :: (a -> Prop) -> (b -> a) -> Prop
suc n = forAllValues n (valuesOf unknown)

--- Checks a unit test with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check0 :: Config -> String -> Prop -> IO Bool
check0 = check

--- The property `forValues xs p` is satisfied if all values of `xs`
--- satisfy property `p x`.
forValues :: [a] -> (a -> Prop) -> Prop
forValues xs p = forAllValues id xs p

--- Checks a unit test with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
checkWithValues0 :: Config -> String -> Prop -> IO Bool
checkWithValues0 = check

--- Checks a property parameterized over a single argument
--- with a given configuration (first argument),
--- a name for the test (second argument),
--- and all values given in the third argument.
--- Returns a flag whether the test was successful.
checkWithValues1 :: Config -> String -> [a] -> (a -> Prop) -> IO Bool
checkWithValues1 config msg xs p = check config msg (forValues xs p)

--- Checks a property parameterized over two arguments
--- with a given configuration (first argument)
--- a name for the test (second argument),
--- and all values given in the third and fourth argument.
--- Returns a flag whether the test was successful.
checkWithValues2 :: Config -> String -> [a] -> [b]
                -> (a -> b -> Prop) -> IO Bool
checkWithValues2 config msg xs ys p =
  check config msg (forValues xs (\x -> forValues ys (p x)))

--- Checks a property parameterized over three arguments
--- with a given configuration (first argument)
--- a name for the test (second argument),
--- and all values given in the third, fourth and fifth argument.
--- Returns a flag whether the test was successful.
checkWithValues3 :: Config -> String -> [a] -> [b] -> [c]
                -> (a -> b -> c -> Prop) -> IO Bool
checkWithValues3 config msg xs ys zs p =
  check config msg
        (forValues xs (\x -> forValues ys (\y -> forValues zs (p x y))))

--- Checks a property parameterized over four arguments
--- with a given configuration (first argument)
--- a name for the test (second argument),
--- and all values given in the further arguments.
--- Returns a flag whether the test was successful.
checkWithValues4 :: Config -> String -> [a] -> [b] -> [c] -> [d]
                -> (a -> b -> c -> d -> Prop) -> IO Bool
checkWithValues4 config msg xs ys zs1 zs2 p =
  check config msg
        (forValues xs (\x -> forValues ys
                      (\y -> forValues zs1
                      (\z1 -> forValues zs2 (p x y z1)))))

--- Checks a property parameterized over five arguments
--- with a given configuration (first argument)
--- a name for the test (second argument),
--- and all values given in the further arguments.
--- Returns a flag whether the test was successful.
checkWithValues5 :: Config -> String -> [a] -> [b] -> [c] -> [d] -> [e]
                -> (a -> b -> c -> d -> e -> Prop) -> IO Bool
checkWithValues5 config msg xs ys zs1 zs2 zs3 p =
  check config msg
        (forValues xs (\x -> forValues ys
                      (\y -> forValues zs1
                      (\z1 -> forValues zs2
                      (\z2 -> forValues zs3 (p x y z1 z2))))))

--- Checks a property parameterized over a single argument
--- with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check1 :: Config -> String -> (_ -> Prop) -> IO Bool
check1 config msg = check config msg . suc id

--- Checks a property parameterized over two arguments
--- with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check2 :: Config -> String -> (_ -> _ -> Prop) -> IO Bool
check2 config msg = check config msg . suc (suc id)

--- Checks a property parameterized over three arguments
--- with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check3 :: Config -> String -> (_ -> _ -> _ -> Prop) -> IO Bool
check3 config msg = check config msg . suc (suc (suc id))

--- Checks a property parameterized over four arguments
--- with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check4 :: Config -> String -> (_ -> _ -> _ -> _ -> Prop) -> IO Bool
check4 config msg = check config msg . suc (suc (suc (suc id)))

--- Checks a property parameterized over five arguments
--- with a given configuration (first argument)
--- and a name for the test (second argument).
--- Returns a flag whether the test was successful.
check5 :: Config -> String -> (_ -> _ -> _ -> _ -> _ -> Prop) -> IO Bool
check5 config msg = check config msg . suc (suc (suc (suc (suc id))))


--- Checks a unit test according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck0 :: String -> Prop -> IO Bool
easyCheck0 = check0 easyConfig

--- Checks a property parameterized over a single argument
--- according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck1 :: String -> (_ -> Prop) -> IO Bool
easyCheck1 = check1 easyConfig

--- Checks a property parameterized over two arguments
--- according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck2 :: String -> (_ -> _ -> Prop) -> IO Bool
easyCheck2 = check2 easyConfig

--- Checks a property parameterized over three arguments
--- according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck3 :: String -> (_ -> _ -> _ -> Prop) -> IO Bool
easyCheck3 = check3 easyConfig

--- Checks a property parameterized over four arguments
--- according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck4 :: String -> (_ -> _ -> _ -> _ -> Prop) -> IO Bool
easyCheck4 = check4 easyConfig

--- Checks a property parameterized over five arguments
--- according to the default configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
easyCheck5 :: String -> (_ -> _ -> _ -> _ -> _ -> Prop) -> IO Bool
easyCheck5 = check5 easyConfig

--- Checks a unit test according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck0 :: String -> Prop -> IO Bool
verboseCheck0 = check0 verboseConfig

--- Checks a property parameterized over a single argument
--- according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck1 :: String -> (_ -> Prop) -> IO Bool
verboseCheck1 = check1 verboseConfig

--- Checks a property parameterized over two arguments
--- according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck2 :: String -> (_ -> _ -> Prop) -> IO Bool
verboseCheck2 = check2 verboseConfig

--- Checks a property parameterized over three arguments
--- according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck3 :: String -> (_ -> _ -> _ -> Prop) -> IO Bool
verboseCheck3 = check3 verboseConfig

--- Checks a property parameterized over four arguments
--- according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck4 :: String -> (_ -> _ -> _ -> _ -> Prop) -> IO Bool
verboseCheck4 = check4 verboseConfig

--- Checks a property parameterized over five arguments
--- according to the verbose configuration
--- and a name for the test (first argument).
--- Returns a flag whether the test was successful.
verboseCheck5 :: String -> (_ -> _ -> _ -> _ -> _ -> Prop) -> IO Bool
verboseCheck5 = check5 verboseConfig


check :: Config -> String -> Prop -> IO Bool
check config msg ts = tests config msg (testsOf ts) 0 0 []

tests :: Config -> String -> [Test] -> Int -> Int -> [[String]] -> IO Bool
tests config msg [] ntest _ stamps =
  done config (msg ++ ":\n Passed") ntest stamps True
tests config msg (t:ts) ntest nfail stamps
  | ntest == maxTest config
  = done config (msg ++ ":\n OK, passed") ntest stamps True
  | nfail == maxFail config
  = done config (msg ++ ":\n Arguments exhausted after") ntest stamps False
  | otherwise = do
      putStr (every config ntest (args t))
      case result t of
        Undef -> tests config msg ts ntest (nfail+1) stamps
        Ok    -> tests config msg ts (ntest+1) nfail (stamp t:stamps)
        Falsified results -> do
          putStr $
            msg ++ " failed\n" ++
            "Falsified by " ++ nth (ntest+1) ++ " test" ++
            (if null (args t) then "." else ".\nArguments:") ++ "\n" ++
            unlines (args t) ++
            if null results then "no result\n"
              else "Results:\n" ++ unlines (nub results)
          return False
        Ambigious bs results -> do
          putStr $
            "Ambigious property yields " ++ show bs ++ " for " ++
            nth (ntest+1) ++ " test" ++
            (if null (args t) then "." else ".\nArguments:") ++ "\n" ++
            unlines (args t) ++
            if null results then "no result\n"
              else "Results:\n" ++ unlines (nub results)
          return False

check' :: Config -> Prop -> Result
check' config ts = tests' config (testsOf ts) 0 0 []

tests' :: Config -> [Test] -> Int -> Int -> [[String]] -> Result
tests' config (t:ts) ntest nfail stamps
  | ntest == maxTest config = Ok
  | nfail == maxFail config = Falsified ["Arguments exhausted after " ++ show ntest ++ " test"]
  | otherwise = case result t of
                     Undef     -> tests' config ts ntest (nfail+1) stamps
                     Ok        -> tests' config ts (ntest+1) nfail stamps
                     res       -> res

easyCheck' :: Prop -> Result
easyCheck' = check' easyConfig

easyCheck1' :: (_ -> Prop) -> Result
easyCheck1' = easyCheck' . suc id

easyCheck2' :: (_ -> _ -> Prop) -> Result
easyCheck2' = easyCheck' . suc (suc id)

easyCheck3' :: (_ -> _ -> _ -> Prop) -> Result
easyCheck3' = easyCheck' . suc (suc (suc id))

easyCheck4' :: (_ -> _ -> _ -> _ -> Prop) -> Result
easyCheck4' = easyCheck' . suc (suc (suc (suc id)))

easyCheck5' :: (_ -> _ -> _ -> _ -> _ -> Prop) -> Result
easyCheck5' = easyCheck' . suc (suc (suc (suc (suc id))))

nth :: Int -> String
nth n = case n of 1 -> "first"; 2 -> "second"; 3 -> "third"; _ -> show n++ "th"

done :: Config -> String -> Int -> [[String]] -> Bool -> IO Bool
done config mesg ntest stamps status = do
  unless (isQuiet config) $
    putStr (mesg ++ " " ++ show ntest ++ " test"
            ++ (if ntest >= 2 then "s" else "") ++ table)
  return status
 where
  table = display
        . map entry
        . reverse
        . mergeSort (leqPair (<=) (leqList leqString))
        . map pairLength
        . group
        . mergeSort (leqList leqString)
        . filter (not . null)
        $ stamps

  display []         = ".\n"
  display [x]        = " - " ++ x ++ ".\n"
  display xs@(_:_:_) = ".\n" ++ unlines (map (++".") xs)

  pairLength xss@(xs:_) = (length xss,xs)

  entry (n,xs) = percentage n ntest ++ " " ++ concat (intersperse ", " xs)

  percentage n _ = let s = show n -- ((100*n)`div`m)
                    in replicate (5-length s) ' ' ++ s -- ++ "%"

-- Auxiliary Functions

leqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> ((a,b) -> (a,b) -> Bool)
leqPair leqa leqb (x1,y1) (x2,y2)
  | x1 == x2  = leqb y1 y2
  | otherwise = leqa x1 x2

-------------------------------------------------------------------------
--- Safely checks a property, i.e., catch all exceptions that might occur
--- and return appropriate error message in case of a failed test.
checkPropWithMsg :: String -> IO Bool -> IO (Maybe String)
checkPropWithMsg msg execprop = catchNDIO msg $ do
  b <- catch execprop
             (\e -> putStrLn (msg ++ ": EXECUTION FAILURE:\n" ++ showError e)
                    >> return False)
  return (if b then Nothing else Just msg)

--- Safely checks an IO property, i.e., catch all exceptions that might occur
--- and return appropriate error message in case of a failed test.
--- This operation is used by the currycheck tool.
checkPropIOWithMsg :: Config -> String -> PropIO -> IO (Maybe String)
checkPropIOWithMsg config msg p = catchNDIO msg $ (ioTestOf p) config msg

-- Execute I/O action for assertion checking and report any failure
-- or non-determinism.
catchNDIO :: String -> IO (Maybe String) -> IO (Maybe String)
catchNDIO msg testact =
  if curryCompiler == "kics2"
  then -- specific handling for KiCS2 since it might report non-det errors
       -- even if there is only one result value, e.g., in functional patterns
       getAllValues testact >>= checkIOActions
  else -- For PAKCS we need a different code since it is more strict
       -- in encapsulating search
       catch testact
             (\e -> putStrLn (msg++": EXECUTION FAILURE: "++showError e) >>
                    return (Just msg))
 where
  checkIOActions results
    | null results
     = putStrLn (msg++": FAILURE: computation failed") >> return (Just msg)
    | not (null (tail results))
     = putStrLn (msg++": FAILURE: computation is non-deterministic") >>
       return (Just msg)
    | otherwise = head results

-------------------------------------------------------------------------
