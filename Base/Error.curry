{-# LANGUAGE NoImplicitPrelude #-}
module Base.Error (error) where

import Base.Eval

--- Aborts the execution with an error message.
error :: String -> _
error x = prim_error $## x

prim_error :: String -> _
prim_error external
