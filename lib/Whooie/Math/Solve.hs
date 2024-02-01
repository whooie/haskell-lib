{-# LANGUAGE
  TypeFamilies
, TypeFamilyDependencies
, AllowAmbiguousTypes
, FlexibleContexts #-}

-- | Provides an implementation of fourth-order Runge-Kutta with optional
-- adaptive step size to solve 1D ordinary differential equations.
module Whooie.Math.Solve
  (
    Integrable, addxx, addyy, subxx, subyy, mulfx, mulfy, mulxy,
    rk4_step,
    rk4,
    Error(..),
    Result,
    AdaptiveStep, norm, cmpxx,
    AdaptiveOpts(..),
    rka_step,
    rka,
  ) where

import Data.Function ((&))
import Whooie.Utils.Misc ((>$<))

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
  -- | Multiply an element of the domain by an ordinary `Float`.
  mulfx :: Float -> XCoord a -> XCoord a
  -- | Multiply an element of the codomain by an ordinary `Float`.
  mulfy :: Float -> a -> a
  -- | Multiply an element of the codomain by one of the domain.
  mulxy :: XCoord a -> a -> a

-- | @rk4_step rhs dx x y@: Take a single RK4 step of a certain size @dx@, given
-- the right-hand side of the governing equation.
rk4_step
  :: Integrable a
  => (XCoord a -> a -> a)
  -> XCoord a
  -> XCoord a
  -> a
  -> a
rk4_step rhs dx x y =
  let x_half = x `addxx` (0.5 `mulfx` dx)
      x_full = x `addxx` dx
      k1 = rhs x      y
      k2 = rhs x_half (y `addyy` (0.5 `mulfy` (dx `mulxy` k1)))
      k3 = rhs x_half (y `addyy` (0.5 `mulfy` (dx `mulxy` k2)))
      k4 = rhs x_full (y `addyy` (dx `mulxy` k3))
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
  let scan_fn y_cur (x_cur, dx_cur) = rk4_step rhs dx_cur x_cur y_cur
      nx = length x
   in
    zip (drop 1 x) (take (nx - 1) x)
    & map (\(xn, xp) -> (xn `subxx` xp, xp))
    & scanl scan_fn y0

-- | Error type returned if the adaptive step size routine fails to converge.
data Error = NoConverge String

-- | Result wrapper around `Error`.
type Result a = Either Error a

-- | Extends `Integrable` to include other necessary functions and values for an
-- adaptive step size routine.
class Integrable a => AdaptiveStep a where
  -- | The real-valued norm of an @a@.
  norm :: a -> Float
  -- | Compare two @XCoord a@ values.
  cmpxx :: XCoord a -> XCoord a -> Ordering

-- | Simple structure to hold options for the adaptive step size routine.
-- @epsilon@ is the desired precision bound, and @maxiters@ is the maixmum
-- number of iterations allowed before `NoConverge` is returned.
data AdaptiveOpts = AdaptiveOpts { epsilon :: Float, maxiters :: Int }

safe1 :: Float
safe1 = 0.9

safe2 :: Float
safe2 = 4.0

safe2_inv :: Float
safe2_inv = 0.25

error_ratio :: AdaptiveStep a => a -> a -> Float -> Float
error_ratio y0 y1 err0 =
  let scale = (err0 / 2.0) * ((norm y0) + (norm y1))
      diff = norm (y0 `subyy` y1)
   in diff / (scale + 1.0e-16)

rka_step_inner
  :: AdaptiveStep a
  => Float
  -> Int
  -> (XCoord a -> a -> a)
  -> XCoord a
  -> XCoord a
  -> a
  -> Int
  -> Result (XCoord a, a, XCoord a)
rka_step_inner eps maxit rhs dx x y iter_count =
  if iter_count >= maxit then
    Left (NoConverge "failed to converge to the desired error bound")
  else
    let dx_half = 0.5 `mulfx` dx
        x_half = x `addxx` dx_half
        y_half = rk4_step rhs dx_half x y
        y_half2 = rk4_step rhs dx_half x_half y_half
        x_full = x `addxx` dx
        y_full = rk4_step rhs dx x y
        e = error_ratio y_half2 y_full eps
        dx' = (safe1 * (e ** (-0.2))) `mulfx` dx
        dx_cond1 = safe2_inv `mulfx` dx
        dx_cond2 = safe2 `mulfx` dx
        dx'' = if cmpxx dx_cond1 dx' == GT then dx_cond1 else dx'
        dx''' = if cmpxx dx_cond2 dx'' == LT then dx_cond2 else dx''
     in
      if e < 1.0 then
        Right (x_full, y_half2, dx''')
      else 
        rka_step_inner eps maxit rhs dx''' x y (iter_count + 1)

-- | @rka_step opts rhs dx x y@: Take a single adaptive RK4 step based on an
-- initial, test step size. This function iteratively converges to a new step
-- size by repeatedly comparing the estimated local truncation error for
-- subdivided steps until the desired error bound is met.
--
-- Returns `NoConverge` if the error bound cannot be met within `AdaptiveStep`'s
-- `maxiters` number of iterations. If successful, the returned tuple is of the
-- form @(new_x, new_y, new_dx)@.
rka_step
  :: AdaptiveStep a
  => AdaptiveOpts
  -> (XCoord a -> a -> a)
  -> XCoord a
  -> XCoord a
  -> a
  -> Result (XCoord a, a, XCoord a)
rka_step opts rhs dx x y = rka_step_inner eps maxit rhs dx x y 0
    where AdaptiveOpts { epsilon = eps, maxiters = maxit } = opts

rka_inner
  :: AdaptiveStep a
  => AdaptiveOpts
  -> (XCoord a -> a -> a)
  -> XCoord a
  -> XCoord a
  -> XCoord a
  -> a
  -> Result [(XCoord a, a)]
rka_inner opts rhs x_stop dx x y =
  if cmpxx x x_stop == GT then
    Right []
  else
    let x_rem = x_stop `subxx` x
        dx' = if cmpxx dx x_rem == LT then dx else x_rem
     in
      case rka_step opts rhs dx' x y of
        Left err -> Left err
        Right (x_next, y_next, dx_next) ->
          case rka_inner opts rhs x_stop dx_next x_next y_next of
            Left err' -> Left err'
            Right points -> Right ((x_next, y_next) : points)

-- | @rka opts rhs dx_init x_bounds y0@: Solve an ordinary differential equation
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
rka opts rhs dx_init (x_start, x_stop) y0 =
  rka_inner opts rhs x_stop dx_init x_start y0
  >$< (\points -> unzip points)

