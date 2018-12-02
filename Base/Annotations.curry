{-# LANGUAGE NoImplicitPrelude #-}
module Base.Annotation (DET, PEVAL) where

--- Identity type synonym used to mark deterministic operations.
type DET a = a

--- Identity function used by the partial evaluator
--- to mark expressions to be partially evaluated.
PEVAL :: a -> a
PEVAL x = x
