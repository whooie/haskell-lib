{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Provides implementations of Newton-Raphson root-finding and golden-section
-- extremum finding.
module Whooie.Math.Search
  (
    NRDomain, clamp, step_lt_eps,
    GSDomain, gen_point, bracket_lt_eps,
    NoConverge(..),
    Result,
    SearchOpts(..),
    Cmp(..),
    Point,
    Bracket,
    nr_find_root,
    gs_find_extremum,
  ) where

-- | Error type returned when a search fails to converge. Contains the last
-- search point.
data NoConverge a = NoConverge a

-- | Result type returned by searching functions.
type Result e a = Either (NoConverge e) a

-- | Options for searching routines. @epsilon@ is the desired precision bound
-- and @maxiters@ is the maximum number of steps that can be taken before a
-- `NoConverge` is returned.
data SearchOpts = SearchOpts { epsilon :: Float, maxiters :: Int }

-- | Defines functions for a Newton-Raphson root search.
class Fractional a => NRDomain a where
  -- | Ensure that a given point is returned to another within a certain desired
  -- sub-region of the domain. Defaults to `id`.
  clamp :: a -> a
  clamp = id
  -- | Return @True@ if a certain step size meets a desired precision bound.
  step_lt_eps :: a -> Float -> Bool

nr_find_root_inner
  :: NRDomain a
  => Float
  -> Int
  -> (a -> a)
  -> (a -> a)
  -> a
  -> Int
  -> Result a a
nr_find_root_inner eps maxit f df cur step_count =
  if step_count >= maxit then
    Left (NoConverge cur)
  else
    let step = (f cur) / (df cur)
        is_converged = step_lt_eps step eps
        next = clamp (cur - step)
     in
      if is_converged then
        Right cur
      else
        nr_find_root_inner eps maxit f df next (step_count + 1)

-- | Newton-Raphson root-finding routine.
--
-- @nr_find_root opts f df x0@ is the result of a Newton-Raphson search
-- beginning at @x0@ for function @f@ with derivative @df@, with options @opts@.
nr_find_root
  :: NRDomain a
  => SearchOpts
  -> (a -> a)
  -> (a -> a)
  -> a
  -> Result a a
nr_find_root opts f df x0 =
  nr_find_root_inner eps maxit f df x0 0
    where SearchOpts { epsilon = eps, maxiters = maxit } = opts

-- | Defines functions for a golden-section extremum search.
class Fractional a => GSDomain a where
  -- | Generate a point within a given sub-domain according to this
  -- parameterization, which must map @0.0@ and @1.0@ to the ends of the
  -- sub-domain.
  gen_point :: (a, a) -> Float -> a
  -- | Return @True@ if a sub-domain meets a desired precision bound.
  bracket_lt_eps :: (a, a) -> Float -> Bool

-- | Represents a preference between two elements of the codomain of the
-- searched space.
data Cmp = L | R

invphi :: Float
invphi = 0.6180339887498949

invphi2 :: Float
invphi2 = 0.3819660112501051

-- | An @(x, y)@ pair.
type Point a = (a, a)

-- | A bracketing pair of points. Returned by `gs_find_extremum` if the search
-- fails to converge.
type Bracket a = (Point a, Point a)

gs_find_extremum_inner
  :: GSDomain a
  => Float
  -> Int
  -> (a -> a)
  -> (a -> a -> Cmp)
  -> ((a, a), (a, a), (a, a), (a, a))
  -> Int
  -> Result ((a, a), (a, a)) (a, a)
gs_find_extremum_inner eps maxit f cmp points step_count =
  let ((x0, f0), (x1, f1), (x2, f2), (x3, f3)) = points
   in
    if step_count >= maxit then
      Left (NoConverge ((x0, f0), (x3, f3)))
    else if bracket_lt_eps (x0, x3) eps then
      case cmp f1 f2 of
        L -> Right (x1, f1)
        R -> Right (x2, f2)
    else
      case cmp f1 f2 of
        L ->
          let x1' = gen_point (x0, x2) invphi2
              f1' = f x1'
              points' = ((x0, f0), (x1', f1'), (x1, f1), (x2, f2))
           in gs_find_extremum_inner eps maxit f cmp points' (step_count + 1)
        R ->
          let x2' = gen_point (x1, x3) invphi
              f2' = f x2'
              points' = ((x1, f1), (x2, f2), (x2', f2'), (x3, f3))
           in gs_find_extremum_inner eps maxit f cmp points' (step_count + 1)
      

-- | Main golden-section search routine.
--
-- @gs_find_extremum opts f cmp init_bracket@ is the result of a golden-section
-- search in an initial sub-domain @init_bracket@ with codomain given through
-- application of @f@, and preference between codomain elements given through
-- @cmp@, with options @opts@.
gs_find_extremum
  :: GSDomain a
  => SearchOpts
  -> (a -> a)
  -> (a -> a -> Cmp)
  -> (a, a)
  -> Result (Bracket a) (Point a)
gs_find_extremum opts f cmp (x0, x3) =
  gs_find_extremum_inner eps maxit f cmp points 0
    where SearchOpts { epsilon = eps, maxiters = maxit } = opts
          x1 = gen_point (x0, x3) invphi2
          x2 = gen_point (x0, x3) invphi
          f0 = f x0
          f1 = f x1
          f2 = f x2
          f3 = f x3
          points = ((x0, f0), (x1, f1), (x2, f2), (x3, f3))

