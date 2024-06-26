-- | Miscellaneous functions on lists types.
module Whooie.Utils.List
  ( collectMaybe
  , collectRight
  , collectLeft
  , unsnoc
  , unpackFml
  ) where

import Data.Function ((&))
import Data.List (uncons)

-- | Collect a list of `Maybe`s into a `Maybe` of a list, returning @Just@ if
-- all items are @Just@, otherwise @Nothing@.
collectMaybe :: [Maybe a] -> Maybe [a]
collectMaybe items =
  case items of
    [] -> Just []
    Nothing : _ -> Nothing
    (Just item) : rest ->
      case collectMaybe rest of
        Nothing -> Nothing
        Just recRes -> Just (item : recRes)

-- | Collect a list of `Either`s into an `Either` of a list, returning @Right@
-- if all items are @Right@, otherwise the first @Left@ encountered.
collectRight :: [Either l r] -> Either l [r]
collectRight items =
  case items of
    [] -> Right []
    (Left l) : _ -> Left l
    (Right item) : rest ->
      case collectRight rest of
        Left l -> Left l
        Right recRes -> Right (item : recRes)

-- | Collect a list of `Either`s into an `Either` of a list, returning @Left@ if
-- all items are @Left@, otherwise the first @Right@ encountered.
collectLeft :: [Either l r] -> Either [l] r
collectLeft items =
  case items of
    [] -> Left []
    (Right r) : _ -> Right r
    (Left item) : rest ->
      case collectLeft rest of
        Right r -> Right r
        Left recRes -> Left (item : recRes)

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

