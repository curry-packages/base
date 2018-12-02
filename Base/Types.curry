{-# LANGUAGE NoImplicitPrelude #-}
module Base.Types
  ( Char (..), Int (..), Float (..)
  , Bool (..), Ordering (..)
  ) where

external data Char

external data Int

external data Float

data Bool = False | True

data Ordering = LT | EQ | GT

--- A non-reducible polymorphic function.
--- It is useful to express a failure in a search branch of the execution.
failed :: _
failed external
