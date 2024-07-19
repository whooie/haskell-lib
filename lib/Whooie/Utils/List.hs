-- | Miscellaneous functions on lists types.
module Whooie.Utils.List
  ( unsnoc
  , unpackFml
  ) where

import Data.Function ((&))
import Data.List (uncons)

-- | Use this until the actual @unsnoc@ is added to the Stack upstream.
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

-- | Unpack a list into @(first, mid, last)@, where @first@ and @last@ are the
-- first and last elements of the list and @mid@ is a list of everything in
-- between. @mid@ may be empty. Returns @Nothing@ if the original list has fewer
-- than two elements.
unpackFml :: [a] -> Maybe (a, [a], a)
unpackFml items =
  uncons items
  >>= (\(first, rest) ->
    unsnoc rest & fmap (\(mid, last) -> (first, mid, last)))

