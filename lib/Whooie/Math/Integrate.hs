{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Provides implementations of various numerical techniques to compute
-- integrals.
module Whooie.Math.Integrate
  (
  ) where

import Text.Printf (printf)

class Integrable x y where
  zero :: y
  add :: y -> y -> y
  subx :: x -> x -> x
  mulx :: x -> y -> y
  mulf :: Float -> y -> y

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

type Result a = Either Error a

-- trapz :: Integrable x y => x -> [y] -> Result y
-- trapz dx y =


