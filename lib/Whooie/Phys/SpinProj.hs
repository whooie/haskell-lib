-- | Definitions for manipulating a single spin-projection number.
module Whooie.Phys.SpinProj
  ( SpinProj
  , new
  , refl
  , raise
  , lower
  , halves
  , toDouble
  ) where

import Text.Printf (printf)

-- | Main type representing a spin-projection quantum number.
data SpinProj = SpinProj Int deriving (Eq, Ord)

instance Show SpinProj where
  show (SpinProj m) = printf fmt n
    where fmt = if m `mod` 2 == 0 then "proj:%d" else "proj:%d/2"
          n = if m `mod` 2 == 0 then m `quot` 2 else m

-- | Create a new @SpinProj@ from a number of half-spin quanta.
new :: Int -> SpinProj
new = SpinProj

-- | Flip the sign of a projection number.
refl :: SpinProj -> SpinProj
refl (SpinProj m) = new (-m)

-- | Increase the projection number by 1.
raise :: SpinProj -> SpinProj
raise (SpinProj m) = new (m + 2)

-- | Decrease the projection number by 1.
lower :: SpinProj -> SpinProj
lower (SpinProj m) = new (m - 2)

-- | Return the quantum number as a bare number of half-quanta.
halves :: SpinProj -> Int
halves (SpinProj m) = m

-- | Return the quantum number as an ordinary floating-point value.
toDouble :: SpinProj -> Double
toDouble (SpinProj m) = (fromInteger $ toInteger m) / 2.0

