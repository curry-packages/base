{-# LANGUAGE NoImplicitPrelude, CPP #-}
module Base.Internal
  ( apply, cond
#ifdef __PAKCS__
  , letrec, failure
#endif
  ) where

import Base.Types

-- Representation of higher-order applications in FlatCurry.
apply :: (a -> b) -> a -> b
apply external

-- Representation of conditional rules in FlatCurry.
cond :: Bool -> a -> a
cond external

#ifdef __PAKCS__
-- `letrec ones (1 : ones)` binds `ones` to `1 : ones`.
letrec :: a -> a -> Bool
letrec external

-- Internal operation to implement failure reporting.
failure :: _ -> _ -> _
failure external
#endif
