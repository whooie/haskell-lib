{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Provides implementations of various numerical techniques to compute
-- integrals.
module Whooie.Math.Integrate
  (
    Error(..),
    Result,
    trapz,
    simpson,
    boole,
    integrate,
  ) where

import Data.Function ((&))
import Text.Printf (printf)
import Whooie.Utils.List (unpack_fml)
import Whooie.Utils.Either (just_right_or_else)

-- | Main error type.
data Error =
    TooShort Int Int
  | UnequalLengths Int Int
  deriving Eq

instance Show Error where
  show err =
    case err of
      TooShort n m ->
        printf "expected at least %d points but got %d" n m
      UnequalLengths n m ->
        printf "data must be of equal length; got %d and %d" n m

-- | Result type returned by integration functions.
type Result a = Either Error a

(>$<) :: Functor f => f a -> (a -> b) -> f b
(>$<) = flip (<$>)

-- | @trapz dx y@: Compute the integral by the trapezoidal rule for fixed
-- sampling interval.
trapz :: Fractional a => a -> [a] -> Result a
trapz dx y =
  unpack_fml y
  >$< (\(f, m, l) -> (f, sum m, l))
  >$< (\(f, m, l) -> (half * dx * f, dx * m, half * dx * l))
  >$< (\(xf, xm, xl) -> xf + xm + xl)
  & just_right_or_else (\() -> TooShort 2 (length y))
    where half = recip $ fromInteger 2

modidx_filter :: Int -> Int -> [(Int, a)] -> [a]
modidx_filter n m items =
  filter (\(k, _ak) -> k `mod` m == n) items
  & map (\(_k, ak) -> ak)

-- | @simpson dx y@: Compute the integral by Simpson's rule for fixed sampling
-- interval. This rule performs optimally for an odd number of points.
simpson :: Fractional a => a -> [a] -> Result a
simpson dx y =
  unpack_fml (zip [0..] y)
  >$< (\((_, f), m, (_, l)) -> (f, modidx_filter 1 2 m, modidx_filter 0 2 m, l))
  >$< (\(f, m1, m0, l) -> (f, sum m1, sum m0, l))
  >$< (\(f, m1, m0, l) ->
    (third * dx * f, simps_43 * dx * m1, simps_23 * dx * m0, third * dx * l))
  >$< (\(xf, xm1, xm0, xl) -> xf + xm1 + xm0 + xl)
  & just_right_or_else (\() -> TooShort 2 (length y))
    where third = recip $ fromInteger 3
          simps_43 = (fromInteger 4) / (fromInteger 3)
          simps_23 = (fromInteger 2) / (fromInteger 3)

-- | @boole dx y@: Compute the integral by Boole's rule for fixed sampling
-- interval. This rule performs optimally for a number of points that is one
-- more than a multiple of four.
boole :: Fractional a => a -> [a] -> Result a
boole dx y =
  unpack_fml (zip [0..] y)
  >$< (\((_, f), m, (_, l)) ->
    (f, map (\n -> modidx_filter n 4 m) [1, 2, 3, 0], l))
  >$< (\(f, mm, l) -> (f, map sum mm, l))
  >$< (\(f, mm, l) ->
    ( boole_14_45 * dx * f
    , map (\(boolek, mk) -> boolek * dx * mk) (zip factors mm)
    , boole_14_45 * dx * l))
  >$< (\(xf, xmm, xl) -> xf + (sum xmm) + xl)
  & just_right_or_else (\() -> TooShort 2 (length y))
    where boole_14_45 = (fromInteger 14) / (fromInteger 45)
          boole_64_45 = (fromInteger 64) / (fromInteger 45)
          boole_24_45 = (fromInteger  8) / (fromInteger 15)
          boole_28_45 = (fromInteger 28) / (fromInteger 45)
          factors = [boole_64_45, boole_24_45, boole_64_45, boole_28_45]

-- | @integrate dx y@: Compute the integral for fixed sampling interval by the
-- trapezoidal rule, Simpson's rule, or Boole's rule depending on the number of
-- points.
integrate :: Fractional a => a -> [a] -> Result a
integrate dx y =
  let n = length y
   in
    if n `mod` 4 == 1 then
      boole dx y
    else if n `mod` 2 == 1 then
      simpson dx y
    else
      trapz dx y

