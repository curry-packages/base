{-# LANGUAGE NoImplicitPrelude, CPP #-}
module Base.IO
  ( IO, getChar, getLine, putChar, putStr, putStrLn, print
  , IOError (..), userError, ioError, catch) where

import Base.Types
import Base.Monoid
import Base.Functor
import Base.Applicative
import Base.Monad
import Base.Show
import Base.String
import Base.Error

external data IO _

instance Monoid a => Monoid (IO a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance  Functor IO where
  fmap f x = x >>= (pure . f)

instance Applicative IO where
  pure = returnIO
#ifdef __PAKCS__
  (*>) = seqIO
#else
  m *> k = m >>= \_ -> k
#endif
  (<*>) = ap
  liftA2 = liftM2

instance Monad IO where
  (>>=) = bindIO
  (>>) = (*>)
  fail s = ioError (userError s)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO external

seqIO :: IO a -> IO b -> IO b
seqIO external

returnIO :: a -> IO a
returnIO external

--- An action that reads a character from standard output and returns it.
getChar :: IO Char
getChar external

--- An action that reads a line from standard input and returns it.
getLine :: IO String
getLine = do c <- getChar
             case c of
               '\n' -> return []
               _ -> do cs <- getLine
                       return (c : cs)

--- An action that puts its character argument on standard output.
putChar :: Char -> IO ()
putChar c = prim_putChar $# c

prim_putChar :: Char -> IO ()
prim_putChar external

--- Action to print a string on standard output.
putStr :: String -> IO ()
putStr []     = return ()
putStr (c:cs) = putChar c >> putStr cs

--- Action to print a string with a newline on standard output.
putStrLn :: String -> IO ()
putStrLn cs = putStr cs >> putChar '\n'

--- Converts a term into a string and prints it.
print :: Show a => a -> IO ()
print = putStrLn . show

type FilePath = String

--- An action that (lazily) reads a file and returns its contents.
readFile :: FilePath -> IO String
readFile f = prim_readFile $## f

prim_readFile :: FilePath -> IO String
prim_readFile external

#ifdef __PAKCS__
-- Needed for internal implementation of readFile.
prim_readFileContents :: FilePath -> String
prim_readFileContents external
#endif

--- An action that writes a file.
writeFile :: FilePath -> String -> IO ()
writeFile f s = (prim_writeFile $## f) s

prim_writeFile :: FilePath -> String -> IO ()
prim_writeFile external

--- An action that appends a string to a file.
--- It behaves like `writeFile` if the file does not exist.
appendFile :: FilePath -> String -> IO ()
appendFile f s = (prim_appendFile $## f) s

prim_appendFile :: FilePath -> String -> IO ()
prim_appendFile external

--- The (abstract) type of error values.
--- Currently, it distinguishes between general I/O errors,
--- user-generated errors (see 'userError'), failures and non-determinism
--- errors during I/O computations. These errors can be caught by 'catch'.
--- Each error contains a string shortly explaining the error.
--- This type might be extended in the future to distinguish
--- further error situations.
data IOError
  = IOError String     -- normal IO error
  | UserError String   -- user-specified error
  | FailError String   -- failing computation
  | NondetError String -- non-deterministic computation
 deriving Eq

instance Show IOError where
  show (IOError     s) = "i/o error: " ++ s
  show (UserError   s) = "user error: " ++ s
  show (FailError   s) = "fail error: " ++ s
  show (NondetError s) = "nondet error: " ++ s

--- A user error value is created by providing a description of the
--- error situation as a string.
userError :: String -> IOError
userError = UserError

--- Raises an I/O exception with a given error value.
ioError :: IOError -> IO _
#ifdef __PAKCS__
ioError err = error (show err)
#else
ioError err = prim_ioError $## err

prim_ioError :: IOError -> IO _
prim_ioError external
#endif

--- Catches a possible error or failure during the execution of an
--- I/O action. `catch act errfun` executes the I/O action `act`.
--- If an exception or failure occurs during this I/O action, the
--- function `errfun` is applied to the error value.
catch :: IO a -> (IOError -> IO a) -> IO a
catch external
