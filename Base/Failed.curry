{-# LANGUAGE NoImplicitPrelude #-}
module Base.Failed (failed) where

import Base.Types

--- A non-reducible polymorphic function.
--- It is useful to express a failure in a search branch of the execution.
failed :: _
failed external
