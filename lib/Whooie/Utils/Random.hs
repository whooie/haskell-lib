-- | Definitions for simple RNG.
module Whooie.Utils.Random
  ( R
  , runR
  , rand
  , unif
  , unifRange
  ) where

import Control.Monad.State
import System.Random.Stateful

-- | Denotes a randomized "action" outputting a type @a@ to be realized by
-- `runR`.
type R a = State StdGen a

-- | Runner for a randomized action (@R a@), given a seed.
--
-- > -- this program defines a lazy, infinite stream of random Floats
-- >
-- > import System.Random.Stateful
-- >
-- > randFloat :: R Float
-- > randFloat = do
-- >   gen <- get
-- >   let (f, gen') = random gen
-- >   put gen'
-- >   return f
-- >
-- > floatStream :: R [Float]
-- > floatStream = mapM (\_ -> randFloat) $ repeat ()
-- >
-- > getFloats :: Int -> [Float]
-- > getFloats seed = runR seed floatStream
runR
  -- | RNG seed
  :: Int
  -- | Randomized action
  -> R a
  -- | Action output
  -> a
runR seed action = evalState action $ mkStdGen seed

genSingle :: (StdGen -> (a, StdGen)) -> R a
genSingle genf = do
  gen <- get
  let (r, gen') = genf gen
  put gen'
  return r

-- | A general-purpose generator action for a single value.
rand :: Random a => R a
rand = genSingle random

-- | A general-purpose generator action for a single value drawn uniformly from
-- all possible values.
unif :: Uniform a => R a
unif = genSingle uniform

-- | A general-purpose generator action for a single value drawn uniformly from
-- a range of values.
unifRange :: UniformRange a => (a, a) -> R a
unifRange range = genSingle (uniformR range)

