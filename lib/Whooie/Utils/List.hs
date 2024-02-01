-- | Miscellaneous functions on lists types.
module Whooie.Utils.List
  (
    collect_maybe,
    collect_right,
    collect_left,
    unsnoc,
    unpack_fml,
  ) where

import Data.Function ((&))
import Data.List (uncons)

-- | Collect a list of `Maybe`s into a `Maybe` of a list, returning @Just@ if
-- all items are @Just@, otherwise @Nothing@.
collect_maybe :: [Maybe a] -> Maybe [a]
collect_maybe items =
  case items of
    [] -> Just []
    Nothing : _ -> Nothing
    (Just item) : rest ->
      case collect_maybe rest of
        Nothing -> Nothing
        Just rec_res -> Just (item : rec_res)

-- | Collect a list of `Either`s into an `Either` of a list, returning @Right@
-- if all items are @Right@, otherwise the first @Left@ encountered.
collect_right :: [Either l r] -> Either l [r]
collect_right items =
  case items of
    [] -> Right []
    (Left l) : _ -> Left l
    (Right item) : rest ->
      case collect_right rest of
        Left l -> Left l
        Right rec_res -> Right (item : rec_res)

-- | Collect a list of `Either`s into an `Either` of a list, returning @Left@ if
-- all items are @Left@, otherwise the first @Right@ encountered.
collect_left :: [Either l r] -> Either [l] r
collect_left items =
  case items of
    [] -> Left []
    (Right r) : _ -> Right r
    (Left item) : rest ->
      case collect_left rest of
        Right r -> Right r
        Left rec_res -> Left (item : rec_res)

-- | Use this until the actual @unsnoc@ is added to the Stack upstream.
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

-- | Unpack a list into @(first, mid, last)@, where @first@ and @last@ are the
-- first and last elements of the list and @mid@ is a list of everything in
-- between. @mid@ may be empty. Returns @Nothing@ if the original list has fewer
-- than two elements.
unpack_fml :: [a] -> Maybe (a, [a], a)
unpack_fml items =
  uncons items
  >>= (\(first, rest) ->
    unsnoc rest & fmap (\(mid, last) -> (first, mid, last)))

