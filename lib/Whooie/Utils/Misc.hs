-- | Miscellaneous other stuff.
module Whooie.Utils.Misc
  ( (<&>)
  ) where

-- | A left-associative operator version of `fmap`. Alternatively, the
-- functorial version of @Data.Function.(&)@.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

