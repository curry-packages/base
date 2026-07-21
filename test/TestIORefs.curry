-- Testing operations from library Data.IORef:

import Data.IORef
import Data.List  ( replace )
import Test.Prop

-- Testing IORefs with basic types:
iorefBasic = do
  r <- newIORef (1,True)
  (n,_) <- readIORef r
  writeIORef r (n,False)
  readIORef r

testIORefBasic = iorefBasic `returns` (1,False)

-- Testing IORefs containing infinite structures:
iorefInf = do
  r <- newIORef [1,2..]
  l1 <- readIORef r
  writeIORef r (replace 42 2 l1)
  l2 <- readIORef r
  return (take 5 l2)

testIORefInfinite = iorefInf `returns` [1,2,42,4,5]

-- Testing IORefs containing functional values:
iorefFunc = do
  r <- newIORef (not,False)
  (f1,b1) <- readIORef r
  writeIORef r (id,b1)
  (f2,b2) <- readIORef r
  return (f1 b1, f2 b2)

testIORefFunc = iorefFunc `returns` (True,False)

-- Testing IORefs containing free variables:
iorefFree = do
  r <- let x free in newIORef (x,x)
  (_,y) <- readIORef r
  doSolve (y=:=1)
  (z,_) <- readIORef r
  return z

testIORefFree = iorefFree `returns` 1

