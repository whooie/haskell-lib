{-# LANGUAGE
  TypeFamilies
, TypeFamilyDependencies
, AllowAmbiguousTypes
, FlexibleContexts #-}

-- | Provides an implementation of fourth-order Runge-Kutta with optional
-- adaptive step size to solve 1D ordinary differential equations.
module Whooie.Math.Solve
  ( Integrable (..)
  , rk4Step
  , rk4
  , Error (..)
  , Result
  , AdaptiveStep (..)
  , AdaptiveOpts (..)
  , rkaStep
  , rka
  ) where

import Data.Function ((&))
import Whooie.Utils.Misc ((<&>))

-- | Provides basic functions for an integration scheme on some ODE of a
-- function, described by two types: @XCoord a@ as the function's domain and @a@
-- as its codomain.
class Integrable a where
  -- | Data type representing the function domain.
  type XCoord a = b | b -> a -- this variant is needed to prove the invertibility of this type
  -- | Add two elements of the domain.
  addxx :: XCoord a -> XCoord a -> XCoord a
  -- | Add two elements of the codomain.
  addyy :: a -> a -> a
  -- | Subtract two elements of the domain.
  subxx :: XCoord a -> XCoord a -> XCoord a
  -- | Subtract two elements of the codomain.
  subyy :: a -> a -> a
  -- | Multiply an element of the domain by an ordinary `Double`.
  mulfx :: Double -> XCoord a -> XCoord a
  -- | Multiply an element of the codomain by an ordinary `Double`.
  mulfy :: Double -> a -> a
  -- | Multiply an element of the codomain by one of the domain.
  mulxy :: XCoord a -> a -> a

-- | @rk4Step rhs dx x y@: Take a single RK4 step of a certain size @dx@, given
-- the right-hand side of the governing equation.
rk4Step
  :: Integrable a
  => (XCoord a -> a -> a)
  -> XCoord a
  -> XCoord a
  -> a
  -> a
rk4Step rhs dx x y =
  let xHalf = x `addxx` (0.5 `mulfx` dx)
      xFull = x `addxx` dx
      k1 = rhs x      y
      k2 = rhs xHalf (y `addyy` (0.5 `mulfy` (dx `mulxy` k1)))
      k3 = rhs xHalf (y `addyy` (0.5 `mulfy` (dx `mulxy` k2)))
      k4 = rhs xFull (y `addyy` (dx `mulxy` k3))
      k = k1 `addyy` k2 `addyy` k3 `addyy` k4
   in y `addyy` ((1.0 / 6.0) `mulfy` (dx `mulxy` k))

-- | @rk4 rhs x y0@: Solve an ordinary differential equation over a fixed
-- coordinate grid @x@ via RK4, given an initial condition @y0@.
--
-- The governing equation is modeled by a single function that should return the
-- first derivative of the dependent variable (this is the entire right-hand
-- side of the equation, opposite only a first-order derivative).
rk4 :: Integrable a => (XCoord a -> a -> a) -> [XCoord a] -> a -> [a]
rk4 rhs x y0 =
  let scanFn yCur (xCur, dxCur) = rk4Step rhs dxCur xCur yCur
      nx = length x
   in
    zip (drop 1 x) (take (nx - 1) x)
    & map (\(xn, xp) -> (xn `subxx` xp, xp))
    & scanl scanFn y0

-- | Error type returned if the adaptive step size routine fails to converge.
data Error = NoConverge String

-- | Result wrapper around `Error`.
type Result a = Either Error a

-- | Extends `Integrable` to include other necessary functions and values for an
-- adaptive step size routine.
class Integrable a => AdaptiveStep a where
  -- | The real-valued norm of an @a@.
  norm :: a -> Double
  -- | Compare two @XCoord a@ values.
  cmpxx :: XCoord a -> XCoord a -> Ordering

-- | Simple structure to hold options for the adaptive step size routine.
-- @epsilon@ is the desired precision bound, and @maxiters@ is the maixmum
-- number of iterations allowed before `NoConverge` is returned.
data AdaptiveOpts = AdaptiveOpts { epsilon :: Double, maxiters :: Int }

safe1 :: Double
safe1 = 0.9

safe2 :: Double
safe2 = 4.0

safe2Inv :: Double
safe2Inv = 0.25

errorRatio :: AdaptiveStep a => a -> a -> Double -> Double
errorRatio y0 y1 err0 =
  let scale = (err0 / 2.0) * ((norm y0) + (norm y1))
      diff = norm (y0 `subyy` y1)
   in diff / (scale + 1.0e-16)

rkaStepInner
  :: AdaptiveStep a
  => Double
  -> Int
  -> (XCoord a -> a -> a)
  -> XCoord a
  -> XCoord a
  -> a
  -> Int
  -> Result (XCoord a, a, XCoord a)
rkaStepInner eps maxit rhs dx x y iterCount =
  if iterCount >= maxit then
    Left (NoConverge "failed to converge to the desired error bound")
  else
    let dxHalf = 0.5 `mulfx` dx
        xHalf = x `addxx` dxHalf
        yHalf = rk4Step rhs dxHalf x y
        yHalf2 = rk4Step rhs dxHalf xHalf yHalf
        xFull = x `addxx` dx
        yFull = rk4Step rhs dx x y
        e = errorRatio yHalf2 yFull eps
        dx' = (safe1 * (e ** (-0.2))) `mulfx` dx
        dxCond1 = safe2Inv `mulfx` dx
        dxCond2 = safe2 `mulfx` dx
        dx'' = if cmpxx dxCond1 dx' == GT then dxCond1 else dx'
        dx''' = if cmpxx dxCond2 dx'' == LT then dxCond2 else dx''
     in
      if e < 1.0 then
        Right (xFull, yHalf2, dx''')
      else 
        rkaStepInner eps maxit rhs dx''' x y (iterCount + 1)

-- | @rkaStep opts rhs dx x y@: Take a single adaptive RK4 step based on an
-- initial, test step size. This function iteratively converges to a new step
-- size by repeatedly comparing the estimated local truncation error for
-- subdivided steps until the desired error bound is met.
--
-- Returns `NoConverge` if the error bound cannot be met within `AdaptiveStep`'s
-- `maxiters` number of iterations. If successful, the returned tuple is of the
-- form @(newX, newY, newDx)@.
rkaStep
  :: AdaptiveStep a
  => AdaptiveOpts
  -> (XCoord a -> a -> a)
  -> XCoord a
  -> XCoord a
  -> a
  -> Result (XCoord a, a, XCoord a)
rkaStep opts rhs dx x y = rkaStepInner eps maxit rhs dx x y 0
    where AdaptiveOpts { epsilon = eps, maxiters = maxit } = opts

rkaInner
  :: AdaptiveStep a
  => AdaptiveOpts
  -> (XCoord a -> a -> a)
  -> XCoord a
  -> XCoord a
  -> XCoord a
  -> a
  -> Result [(XCoord a, a)]
rkaInner opts rhs xStop dx x y =
  if cmpxx x xStop == GT then
    Right []
  else
    let xRem = xStop `subxx` x
        dx' = if cmpxx dx xRem == LT then dx else xRem
     in
      case rkaStep opts rhs dx' x y of
        Left err -> Left err
        Right (xNext, yNext, dxNext) ->
          case rkaInner opts rhs xStop dxNext xNext yNext of
            Left err' -> Left err'
            Right points -> Right ((xNext, yNext) : points)

-- | @rka opts rhs dxInit xBounds y0@: Solve an ordinary differential equation
-- over some coordinate bounds via RK4 with adaptive step sizes, given an
-- initial condition.
--
-- The governing equation is modeled by a single function that should return the
-- first dierivative of the dependent variable (this is the entire right-hand
-- side of the equation, opposite only a first-order derivative).
rka
  :: AdaptiveStep a
  => AdaptiveOpts
  -> (XCoord a -> a -> a)
  -> XCoord a
  -> (XCoord a, XCoord a)
  -> a
  -> Result ([XCoord a], [a])
rka opts rhs dxInit (xStart, xStop) y0 =
  rkaInner opts rhs xStop dxInit xStart y0
  <&> (\points -> unzip points)

