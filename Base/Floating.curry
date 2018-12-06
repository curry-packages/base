{-# LANGUAGE NoImplicitPrelude #-}
module Base.Floating (Floating(..)) where

import Base.Types
import Base.Fractional
import Base.Eval
import Base.Num
import Base.Error

class Fractional a => Floating a where
  pi :: a
  exp, log, sqrt :: a -> a
  (**), logBase :: a -> a -> a
  sin, cos, tan :: a -> a
  asin, acos, atan :: a -> a
  sinh, cosh, tanh :: a -> a
  asinh, acosh, atanh :: a -> a

  sqrt x = x ** 0.5
  x ** y = exp (log x * y)
  logBase x y = log y / log x
  tan x = sin x / cos x
  tanh x = sinh x / cosh x

instance Floating Float where
  pi = 3.141592653589793238
  exp = expFloat
  log = logFloat
  sqrt = sqrtFloat
  sin = sinFloat
  cos = cosFloat
  tan = tanFloat
  asin = asinFloat
  acos = acosFloat
  atan = atanFloat
  sinh = sinhFloat
  cosh = coshFloat
  tanh = tanhFloat
  asinh = asinhFloat
  acosh = acoshFloat
  atanh = atanhFloat

-- Natural logarithm.
logFloat :: Float -> Float
logFloat x = prim_logFloat $# x

prim_logFloat :: Float -> Float
prim_logFloat external

-- Natural exponent.
expFloat :: Float -> Float
expFloat x = prim_expFloat $# x

prim_expFloat :: Float -> Float
prim_expFloat external

-- Square root.
sqrtFloat :: Float -> Float
sqrtFloat x = prim_sqrtFloat $# x

prim_sqrtFloat :: Float -> Float
prim_sqrtFloat external

-- Sine.
sinFloat :: Float -> Float
sinFloat x = prim_sinFloat $# x

prim_sinFloat :: Float -> Float
prim_sinFloat external

-- Cosine.
cosFloat :: Float -> Float
cosFloat x = prim_cosFloat $# x

prim_cosFloat :: Float -> Float
prim_cosFloat external

-- Tangent.
tanFloat :: Float -> Float
tanFloat x = prim_tanFloat $# x

prim_tanFloat :: Float -> Float
prim_tanFloat external

-- Arcus sine.
asinFloat :: Float -> Float
asinFloat x = prim_asinFloat $# x

prim_asinFloat :: Float -> Float
prim_asinFloat external

-- Arcus cosine.
acosFloat :: Float -> Float
acosFloat x = prim_acosFloat $# x

prim_acosFloat :: Float -> Float
prim_acosFloat external

-- Arcus tangent.
atanFloat :: Float -> Float
atanFloat x = prim_atanFloat $# x

prim_atanFloat :: Float -> Float
prim_atanFloat external

-- Hyperbolic sine.
sinhFloat :: Float -> Float
sinhFloat x = prim_sinhFloat $# x

prim_sinhFloat :: Float -> Float
prim_sinhFloat external

-- Hyperbolic cosine.
coshFloat :: Float -> Float
coshFloat x = prim_coshFloat $# x

prim_coshFloat :: Float -> Float
prim_coshFloat external

-- Hyperbolic tangent.
tanhFloat :: Float -> Float
tanhFloat x = prim_tanhFloat $# x

prim_tanhFloat :: Float -> Float
prim_tanhFloat external

-- Hyperbolic arcus sine.
asinhFloat :: Float -> Float
asinhFloat x = prim_asinhFloat $# x

prim_asinhFloat :: Float -> Float
prim_asinhFloat external

-- Hyperbolic arcus cosine.
acoshFloat :: Float -> Float
acoshFloat x = prim_acoshFloat $# x

prim_acoshFloat :: Float -> Float
prim_acoshFloat external

-- Hyperbolic arcus tangent.
atanhFloat :: Float -> Float
atanhFloat x = prim_atanhFloat $# x

prim_atanhFloat :: Float -> Float
prim_atanhFloat external
