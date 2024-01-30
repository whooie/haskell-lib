-- | Emulates the behavior of Rust's @Maybe@ type, but on `Maybe`s.
module Whooie.Utils.Maybe
  (
    is_just,
    is_just_and,
    is_nothing,
    just_and,
    just_and_then,
    just_then,
    just_or,
    just_or_else,
    just_xor,
    zip,
    unzip,
    flatten,
    map,
    map_or,
    map_or_else,
    unwrap_or,
    unwrap_or_else,
    unwrap,
    expect,
    expect_with,
    collect_list,
  ) where

import Prelude
  (
    Bool(..),
    Maybe(..),
    String,
    IO,
    error,
    return,
    ($),
  )

-- | @is_just opt@ returns @True@ if @opt@ is @Just@.
is_just :: Maybe a -> Bool
is_just opt =
  case opt of
    Just _ -> True
    Nothing -> False

-- | @is_just_and f opt@ returns @f a@ if @opt@ is @Just a@, otherwise @False@.
is_just_and :: (a -> Bool) -> Maybe a -> Bool
is_just_and f opt =
  case opt of
    Just a -> f a
    Nothing -> False

-- | @is_nothing opt@ returns @True@ if @opt@ is @Nothing@.
is_nothing :: Maybe a -> Bool
is_nothing opt =
  case opt of
    Just _ -> False
    Nothing -> True

-- | @just_and opt_a opt_b@ returns @opt_b@ if @opt_a@ is @Just@, otherwise
-- `Nothing`.
just_and :: Maybe a -> Maybe b -> Maybe b
just_and opt_a opt_b =
  case opt_a of
    Just _ -> opt_b
    Nothing -> Nothing

-- | @just_and_then f opt@ returns @f a@ if @opt@ is @Just a@, otherwise
-- @Nothing@.
just_and_then :: (a -> Maybe b) -> Maybe a -> Maybe b
just_and_then f opt =
  case opt of
    Just a -> f a
    Nothing -> Nothing

-- | @just_then f opt@ returns @f a@ if @opt@ is @Just a@, otherwise @return@s
-- @()@.
just_then :: (a -> IO ()) -> Maybe a -> IO ()
just_then f opt =
  case opt of
    Just a -> f a
    Nothing -> return ()

-- | @just_or opt_a opt_b@ returns @opt_b@ if @opt_a@ is @Nothing@, otherwise
-- @Nothing@.
just_or :: Maybe a -> Maybe a -> Maybe a
just_or opt_a opt_b =
  case opt_a of
    Just a -> Just a
    Nothing -> opt_b

-- | @just_or_else f opt@ returns @f ()@ if @opt@ is @Nothing@, otherwise @opt@.
just_or_else :: (() -> Maybe a) -> Maybe a -> Maybe a
just_or_else f opt =
  case opt of
    Just a -> Just a
    Nothing -> f ()

-- | @just_xor opt_a opt_b@ returns whichever of the two is @Just@ only if
-- exactly one of them is @Just@, otherwise @Nothing@.
just_xor :: Maybe a -> Maybe a -> Maybe a
just_xor opt_a opt_b =
  case (opt_a, opt_b) of
    (Just a, Nothing) -> Just a
    (Nothing, Just b) -> Just b
    _ -> Nothing

-- | @zip opt_a opt_b@ is @Just (a, b)@ if @opt_a@ is @Just a@ and @opt_b@ is
-- @Just b@, otherwise @Nothing@.
zip :: Maybe a -> Maybe b -> Maybe (a, b)
zip opt_a opt_b =
  case (opt_a, opt_b) of
    (Just a, Just b) -> Just (a, b)
    _ -> Nothing

-- | @unzip opt@ returns @(Just a, Just b)@ if @opt@ is @Just (a, b)@, otherwise
-- @(Nothing, Nothing)@.
unzip :: Maybe (a, b) -> (Maybe a, Maybe b)
unzip opt =
  case opt of
    Just (a, b) -> (Just a, Just b)
    Nothing -> (Nothing, Nothing)

-- | @flatten opt@ returns @Just a@ if @opt@ is @Just (Just a)@, otherwise
-- @Nothing@.
flatten :: Maybe (Maybe a) -> Maybe a
flatten opt =
  case opt of
    Just (Just a) -> Just a
    _ -> Nothing

-- | @map f opt@ returns @Just (f a)@ if @opt@ is @Just a@, otherwise @Nothing@.
map :: (a -> b) -> Maybe a -> Maybe b
map f opt =
  case opt of
    Just a -> Just (f a)
    Nothing -> Nothing

-- | @map_or f def opt@ returns @f a@ if @opt@ is @Just a@, otherwise @def@.
map_or :: (a -> b) -> b -> Maybe a -> b
map_or f def opt =
  case opt of
    Just a -> f a
    Nothing -> def

-- | @map_or_else f_just f_nothing@ returns @f_just a@ if @opt@ is @Just a@,
-- otherwise @f_nothing ()@.
map_or_else :: (a -> b) -> (() -> b) -> Maybe a -> b
map_or_else f_just f_nothing opt =
  case opt of
    Just a -> f_just a
    Nothing -> f_nothing ()

-- | @unwrap_or def opt@ returns @a@ if @opt@ is @Just a@, otherwise @def@.
unwrap_or :: a -> Maybe a -> a
unwrap_or def opt =
  case opt of
    Just a -> a
    Nothing -> def

-- | @unwrap_or_else f opt@ returns @a@ if @opt@ is @Just a@, otherwise @f ()@.
unwrap_or_else :: (() -> a) -> Maybe a -> a
unwrap_or_else f opt =
  case opt of
    Just a -> a
    Nothing -> f ()

-- | @unwrap opt@ returns @a@ if @opt@ is @Just a@, otherwise an `error` is
-- thrown.
unwrap :: Maybe a -> a
unwrap opt =
  case opt of
    Just a -> a
    Nothing -> error "`unwrap` called on a `Nothing` variant"

-- | @expect msg opt@ returns @a@ if @opt@ is @Just a@, otherwise an `error` is
-- thrown with message @msg@.
expect :: String -> Maybe a -> a
expect msg opt =
  case opt of
    Just a -> a
    Nothing -> error msg

-- | @expect_with f opt@ returns @a@ if @opt@ is @Just a@, otherwise an `error`
-- is thrown with message @f ()@.
expect_with :: (() -> String) -> Maybe a -> a
expect_with f opt =
  case opt of
    Just a -> a
    Nothing -> error $ f ()

-- | @collect_list items@ returns @Just [its]@ if all elements of @items@ are
-- @Just _@, otherwise @Nothing@.
collect_list :: [Maybe a] -> Maybe [a]
collect_list items =
  case items of
    [] -> Just []
    Nothing : _ -> Nothing
    (Just item) : rest ->
      case collect_list rest of
        Nothing -> Nothing
        Just rec_res -> Just (item : rec_res)

