-- | Miscellaneous functions on lists types.
module Whooie.Utils.List
  (
    get,
    collect_maybe,
    collect_right,
    collect_left,
  ) where

get_inner :: Int -> Maybe a -> (Int, a) -> Maybe a
get_inner nth acc (k, it) =
  case acc of
    Just a -> Just a
    Nothing -> if k == nth then Just it else Nothing

-- | Like `!!`, but returning the result in a `Maybe`.
get :: Int -> [a] -> Maybe a
get nth items = foldl (get_inner nth) Nothing (zip [0..] items)

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

