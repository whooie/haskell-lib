{-# LANGUAGE TypeFamilies #-}

-- | Provides implementations of various numerical techniques to compute
-- integrals.
module Whooie.Math.Integrate
  ( Integrable (..)
  , Error (..)
  , Result
  , trapz
  , simpson
  , boole
  , integrate
  ) where

import Data.Function ((&))
import Data.Kind (Type)
import Text.Printf (printf)
import Whooie.Utils.List (unpackFml)
import Whooie.Utils.Either (justRightOrElse)
import Whooie.Utils.Misc ((<&>))

-- | Provides basic functions for an integration scheme on some function,
-- described by two types: @XCoord a@ as the function's domain and @a@ as its
-- codomain.
class Integrable a where
  -- | Data type representing the function domain.
  type XCoord a :: Type
  -- | Zero element of the codomain. Defaults to @fromInteger 0@.
  zeroy :: a
  -- | Add two elements of the codomain. Defaults to `Num`'s `+`.
  addyy :: a -> a -> a
  -- | Multiply an element of the codomain by one of the domain.
  mulxy :: XCoord a -> a -> a
  -- | Multiply an element of the codomain by an ordinary `Double`.
  mulfy :: Double -> a -> a

-- | Main error type.
data Error = TooShort Int Int deriving Eq

instance Show Error where
  show (TooShort n m) = printf "expected at least %d points but got %d" n m

-- | Result type returned by integration functions.
type Result a = Either Error a

-- | @trapz dx y@: Compute the integral by the trapezoidal rule for fixed
-- sampling interval.
trapz :: Integrable a => XCoord a -> [a] -> Result a
trapz dx y =
  unpackFml y
  <&> (\(f, m, l) -> (f, foldl addyy zeroy m, l))
  <&> (\(f, m, l) -> (half `mulfy` f, m, half `mulfy` l))
  <&> (\(f, m, l) -> dx `mulxy` (f `addyy` m `addyy` l))
  & justRightOrElse (\() -> TooShort 2 (length y))
    where half = 0.5

modidxFilter :: Int -> Int -> [(Int, a)] -> [a]
modidxFilter n m items =
  filter (\(k, _ak) -> k `mod` m == n) items
  & map (\(_k, ak) -> ak)

-- | @simpson dx y@: Compute the integral by Simpsons rule for fixed sampling
-- interval. This rule performs optimally for an odd number of points.
simpson :: Integrable a => XCoord a -> [a] -> Result a
simpson dx y =
  unpackFml (zip [0..] y)
  <&> (\((_, f), m, (_, l)) -> (f, modidxFilter 1 2 m, modidxFilter 0 2 m, l))
  <&> (\(f, m1, m0, l) -> (f, foldl addyy zeroy m1, foldl addyy zeroy m0, l))
  <&> (\(f, m1, m0, l) ->
    ( third `mulfy` f
    , simps43 `mulfy` m1
    , simps23 `mulfy` m0
    , third `mulfy` l ))
  <&> (\(f, m1, m0, l) -> dx `mulxy` (f `addyy` m1 `addyy` m0 `addyy` l))
  & justRightOrElse (\() -> TooShort 2 (length y))
    where third = 1.0 / 3.0
          simps43 = 4.0 / 3.0
          simps23 = 2.0 / 3.0

-- | @boole dx y@: Compute the integral by Booles rule for fixed sampling
-- interval. This rule performs optimally for a number of points that is one
-- more than a multiple of four.
boole :: Integrable a => XCoord a -> [a] -> Result a
boole dx y =
  unpackFml (zip [0..] y)
  <&> (\((_, f), m, (_, l)) ->
    (f, map (\n -> modidxFilter n 4 m) [1, 2, 3, 0], l))
  <&> (\(f, mm, l) -> (f, map (foldl addyy zeroy) mm, l))
  <&> (\(f, mm, l) ->
    ( boole1445 `mulfy` f
    , map (\(boolek, mk) -> boolek `mulfy` mk) (zip factors mm)
    , boole1445 `mulfy` l ))
  <&> (\(f, mm, l) -> dx `mulxy` (f `addyy` (foldl addyy zeroy mm) `addyy` l))
  & justRightOrElse (\() -> TooShort 2 (length y))
    where boole1445 = 14.0 / 45.0
          boole6445 = 64.0 / 45.0
          boole2445 =  8.0 / 15.0
          boole2845 = 28.0 / 45.0
          factors = [boole6445, boole2445, boole6445, boole2845]

-- | @integrate dx y@: Compute the integral for fixed sampling interval by the
-- trapezoidal rule, Simpsons rule, or Booles rule depending on the number of
-- points.
integrate :: Integrable a => XCoord a -> [a] -> Result a
integrate dx y =
  let n = length y
   in
    if n `mod` 4 == 1 then
      boole dx y
    else if n `mod` 2 == 1 then
      simpson dx y
    else
      trapz dx y

