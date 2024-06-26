-- | Provides implementations of Newton-Raphson root-finding and golden-section
-- extremum finding.
module Whooie.Math.Search
  ( NRDomain (..)
  , GSDomain (..)
  , NoConverge (..)
  , Result
  , SearchOpts (..)
  , Cmp (..)
  , Point
  , Bracket
  , nrFindRoot
  , gsFindExtremum
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
  stepLtEps :: a -> Float -> Bool

nrFindRootInner
  :: NRDomain a
  => Float
  -> Int
  -> (a -> a)
  -> (a -> a)
  -> a
  -> Int
  -> Result a a
nrFindRootInner eps maxit f df cur stepCount =
  if stepCount >= maxit then
    Left (NoConverge cur)
  else
    let step = (f cur) / (df cur)
        isConverged = stepLtEps step eps
        next = clamp (cur - step)
     in
      if isConverged then
        Right cur
      else
        nrFindRootInner eps maxit f df next (stepCount + 1)

-- | Newton-Raphson root-finding routine.
--
-- @nrFindRoot opts f df x0@ is the result of a Newton-Raphson search
-- beginning at @x0@ for function @f@ with derivative @df@, with options @opts@.
nrFindRoot
  :: NRDomain a
  => SearchOpts
  -> (a -> a)
  -> (a -> a)
  -> a
  -> Result a a
nrFindRoot opts f df x0 =
  nrFindRootInner eps maxit f df x0 0
    where SearchOpts { epsilon = eps, maxiters = maxit } = opts

-- | Defines functions for a golden-section extremum search.
class Fractional a => GSDomain a where
  -- | Generate a point within a given sub-domain according to this
  -- parameterization, which must map @0.0@ and @1.0@ to the ends of the
  -- sub-domain.
  genPoint :: (a, a) -> Float -> a
  -- | Return @True@ if a sub-domain meets a desired precision bound.
  bracketLtEps :: (a, a) -> Float -> Bool

-- | Represents a preference between two elements of the codomain of the
-- searched space.
data Cmp = L | R

invphi :: Float
invphi = 0.6180339887498949

invphi2 :: Float
invphi2 = 0.3819660112501051

-- | An @(x, y)@ pair.
type Point a = (a, a)

-- | A bracketing pair of points. Returned by `gsFindExtremum` if the search
-- fails to converge.
type Bracket a = (Point a, Point a)

gsFindExtremumInner
  :: GSDomain a
  => Float
  -> Int
  -> (a -> a)
  -> (a -> a -> Cmp)
  -> ((a, a), (a, a), (a, a), (a, a))
  -> Int
  -> Result ((a, a), (a, a)) (a, a)
gsFindExtremumInner eps maxit f cmp points stepCount =
  let ((x0, f0), (x1, f1), (x2, f2), (x3, f3)) = points
   in
    if stepCount >= maxit then
      Left (NoConverge ((x0, f0), (x3, f3)))
    else if bracketLtEps (x0, x3) eps then
      case cmp f1 f2 of
        L -> Right (x1, f1)
        R -> Right (x2, f2)
    else
      case cmp f1 f2 of
        L ->
          let x1' = genPoint (x0, x2) invphi2
              f1' = f x1'
              points' = ((x0, f0), (x1', f1'), (x1, f1), (x2, f2))
           in gsFindExtremumInner eps maxit f cmp points' (stepCount + 1)
        R ->
          let x2' = genPoint (x1, x3) invphi
              f2' = f x2'
              points' = ((x1, f1), (x2, f2), (x2', f2'), (x3, f3))
           in gsFindExtremumInner eps maxit f cmp points' (stepCount + 1)
      

-- | Main golden-section search routine.
--
-- @gsFindExtremum opts f cmp initBracket@ is the result of a golden-section
-- search in an initial sub-domain @initBracket@ with codomain given through
-- application of @f@, and preference between codomain elements given through
-- @cmp@, with options @opts@.
gsFindExtremum
  :: GSDomain a
  => SearchOpts
  -> (a -> a)
  -> (a -> a -> Cmp)
  -> (a, a)
  -> Result (Bracket a) (Point a)
gsFindExtremum opts f cmp (x0, x3) =
  gsFindExtremumInner eps maxit f cmp points 0
    where SearchOpts { epsilon = eps, maxiters = maxit } = opts
          x1 = genPoint (x0, x3) invphi2
          x2 = genPoint (x0, x3) invphi
          f0 = f x0
          f1 = f x1
          f2 = f x2
          f3 = f x3
          points = ((x0, f0), (x1, f1), (x2, f2), (x3, f3))

