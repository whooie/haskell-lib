-- | Emulates Rust's @Either@ type, but on `Either`s.
module Whooie.Utils.Either
  (
    Either(..),
    is_right,
    is_right_and,
    is_left,
    is_left_and,
    get_right,
    get_left,
    right_and,
    right_and_then,
    right_then,
    right_then_else,
    right_or,
    right_or_else,
    map,
    map_or,
    map_or_else,
    map_left,
    just_right_or,
    just_right_or_else,
    unwrap_or,
    unwrap_or_else,
    unwrap,
    expect,
    expect_with,
    expect_left,
    expect_left_with,
    collect_list,
    collect_list_left,
  ) where

import Prelude
  (
    Show, show,
    Bool(..),
    Either(..),
    Maybe(..),
    String,
    IO,
    error,
    ($),
    (++),
    return,
  )

-- | @is_right eith@ returns @True@ if @eith@ is @Right@.
is_right :: Either e a -> Bool
is_right eith =
  case eith of
    Right _ -> True
    Left _ -> False

-- | @is_right_and f eith@ returns @f a@ if @eith@ is @Right a@, otherwise
-- @False@.
is_right_and :: (a -> Bool) -> Either e a -> Bool
is_right_and f eith =
  case eith of
    Right a -> f a
    Left _ -> False

-- | @is_left eith@ returns @True@ if @eith@ is @Left@.
is_left :: Either e a -> Bool
is_left eith =
  case eith of
    Right _ -> False
    Left _ -> True

-- | @is_left_and f eith@ returns @f e@ if @eith@ is @Left e@, otherwise
-- @False@.
is_left_and :: (e -> Bool) -> Either e a -> Bool
is_left_and f eith =
  case eith of
    Right _ -> False
    Left e -> f e

-- | @get_right eith@ returns @Just a@ if @eith@ is @Right a@, otherwise
-- @Nothing@.
get_right :: Either e a -> Maybe a
get_right eith =
  case eith of
    Right a -> Just a
    Left _ -> Nothing

-- | @get_left eith@ returns @Just e@ if @eith@ is @Left e@, otherwise
-- @Nothing@.
get_left :: Either e a -> Maybe e
get_left eith =
  case eith of
    Right _ -> Nothing
    Left e -> Just e

-- | @right_and eith_a eith_b@ returns @eith_b@ if @eith_a@ is @Right@,
-- otherwise @eith_a@.
right_and :: Either e a -> Either e b -> Either e b
right_and eith_a eith_b =
  case eith_a of
    Right _ -> eith_b
    Left e -> Left e

-- | @right_and_then f eith@ returns @f a@ if @eith@ is @Right a@, otherwise
-- @eith@.
right_and_then :: (a -> Either e b) -> Either e a -> Either e b
right_and_then f eith =
  case eith of
    Right a -> f a
    Left e -> Left e

-- | @right_then f eith@ calls @f@ on the wrapped @Right@ value, otherwise
-- @return@s @()@.
right_then :: (a -> IO ()) -> Either e a -> IO ()
right_then f eith =
  case eith of
    Right a -> f a
    Left _ -> return ()

-- | @right_then_else f_left f_right eith@ calls @f_right@ on the wrapped
-- @Right@ value or @f_left@ on the wrapped @Left@ value.
right_then_else :: (e -> IO ()) -> (a -> IO ()) -> Either e a -> IO ()
right_then_else f_left f_right eith =
  case eith of
    Right a -> f_right a
    Left e -> f_left e

-- | @right_or eith_a eith_b@ returns @eith_b@ if @eith_a@ is @Left@, otherwise
-- @eith_a@.
right_or :: Either e a -> Either f a -> Either f a
right_or eith_a eith_b =
  case eith_a of
    Right a -> Right a
    Left _ -> eith_b

-- | @right_or_else f eith@ returns @f e@ if @eith@ is @Left e@, otherwise
-- @eith@.
right_or_else :: (e -> Either f a) -> Either e a -> Either f a
right_or_else f eith =
  case eith of
    Right a -> Right a
    Left e -> f e

-- | @map f eith@ returns @Right (f a)@ if @eith@ is @Right a@, otherwise @eith@.
map :: (a -> b) -> Either e a -> Either e b
map f eith =
  case eith of
    Right a -> Right (f a)
    Left e -> Left e

-- | @map_or f def eith@ returns @f a@ if @eith@ is @Right a@, otherwise @def@.
map_or :: (a -> b) -> b -> Either e a -> b
map_or f def eith =
  case eith of
    Right a -> f a
    Left _ -> def

-- | @map_or_else f_left f_right eith@ returns @f_right a@ when @eith@ is @Right
-- a@, otherwise @f_left e@ when @eith@ is @Left e@.
map_or_else :: (e -> b) -> (a -> b) -> Either e a -> b
map_or_else f_left f_right eith =
  case eith of
    Right a -> f_right a
    Left e -> f_left e

-- | @map_left f eith@ returns @Leftor (f e)@ if @eith@ is @Leftor e@, otherwise
-- @eith@.
map_left :: (e -> f) -> Either e a -> Either f a
map_left f eith =
  case eith of
    Right a -> Right a
    Left e -> Left (f e)

-- | @just_right_or err opt@ returns @Right a@ if @opt@ is @Just a@, otherwise
-- @Left err@. See `Either` for more info.
just_right_or :: e -> Maybe a -> Either e a
just_right_or err opt =
  case opt of
    Just a -> Right a
    Nothing -> Left err

-- | @just_right_or_else f opt@ returns @Right a@ if @opt@ is @Just a@,
-- otherwise @Left (f ())@. See `Either` for more info.
just_right_or_else :: (() -> e) -> Maybe a -> Either e a
just_right_or_else f opt =
  case opt of
    Just a -> Right a
    Nothing -> Left (f ())

-- | @unwrap_or def eith@ returns @a@ if @eith@ is @Right a@, otherwise @def@.
unwrap_or :: a -> Either e a -> a
unwrap_or def eith =
  case eith of
    Right a -> a
    Left _ -> def

-- | @unwrap_or_else f eith@ returns @a@ if @eith@ is @Right a@, otherwise @f e@
-- when @eith@ is @Left e@.
unwrap_or_else :: (e -> a) -> Either e a -> a
unwrap_or_else f eith =
  case eith of
    Right a -> a
    Left e -> f e

-- | @unwrap eith@ returns @a@ if @eith@ is @Right a@, otherwise an `error` is
-- thrown.
unwrap :: Show e => Either e a -> a
unwrap eith =
  case eith of
    Right a -> a
    Left e ->
      error $ "called `unwrap` on an `Left` variant with error: " ++ (show e)

-- | @expect msg eith@ returns @a@ if @eith@ is @Right a@, otherwise an `error`
-- is thrown with message @msg@.
expect :: String -> Either e a -> a
expect msg eith =
  case eith of
    Right a -> a
    Left _ -> error msg

-- | @expect_with f eith@ returns @a@ if @eith@ is @Right a@, otherwise an
-- `error` is thrown with message @f ()@.
expect_with :: (e -> String) -> Either e a -> a
expect_with f eith =
  case eith of
    Right a -> a
    Left e -> error $ f e

-- | @expect_left msg eith@ returns @e@ if @eith@ is @Left e@, otherwise an
-- `error` is thrown with message @msg@.
expect_left :: String -> Either e a -> e
expect_left msg eith =
  case eith of
    Right _ -> error msg
    Left e -> e

-- | @expect_left_with f eith@ returns @e@ if @eith@ is @Left e@, otherwise an
-- `error` is thrown with message @f ()@.
expect_left_with :: (a -> String) -> Either e a -> e
expect_left_with f eith =
  case eith of
    Right a -> error $ f a
    Left e -> e

-- | @collect_list items@ returns @Right [its]@ if all elements of @items@ are
-- @Right _@, otherwise the leftmost @Left@.
collect_list :: [Either e a] -> Either e [a]
collect_list items =
  case items of
    [] -> Right []
    (Left e) : _ -> Left e
    (Right item) : rest ->
      case collect_list rest of
        Left e -> Left e
        Right rec_res -> Right (item : rec_res)

-- | @collect_list items@ returns @Left [its]@ if all elements of @items@ are
-- @Left _@, otherwise the leftmost @Right@.
collect_list_left :: [Either e a] -> Either [e] a
collect_list_left items =
  case items of
    [] -> Left []
    (Right a) : _ -> Right a
    (Left item) : rest ->
      case collect_list_left rest of
        Right a -> Right a
        Left rec_res -> Left (item : rec_res)

