-- | Definitions for manipulating a single total-spin number.
module Whooie.Phys.SpinTotal
  ( SpinTotal
  , new
  , halves
  , toDouble
  ) where

import Text.Printf (printf)

-- | Main type representing a total-spin quantum number.
data SpinTotal = SpinTotal Int deriving (Eq, Ord)

instance Show SpinTotal where
  show (SpinTotal j) = printf fmt n
    where fmt = if j `mod` 2 == 0 then "tot:%d" else "tot:%d/2"
          n = if j `mod` 2 == 0 then j `quot` 2 else j

-- | Create a new @SpinTotal@ from a number of half-spin quanta.
--
-- Throws an `error` if the value is negative.
new :: Int -> SpinTotal
new j =
  if j < 0 then error "total spin number must be non-negative" else SpinTotal j

-- | Return the quantum number as a bare number of half-quanta.
halves :: SpinTotal -> Int
halves (SpinTotal j) = j

-- | Return the quantum number as an ordinary floating-point value.
toDouble :: SpinTotal -> Double
toDouble (SpinTotal j) = (fromInteger $ toInteger j) / 2.0

