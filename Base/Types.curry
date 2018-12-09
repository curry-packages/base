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
