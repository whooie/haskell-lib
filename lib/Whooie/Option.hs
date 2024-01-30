-- | Emulates Rust's @Option@ type. Really just for practice.
module Whooie.Option
  (
    Option(..),
    from_maybe,
    to_maybe,
    is_some,
    is_some_and,
    is_none,
    some_and,
    some_and_then,
    some_then,
    some_or,
    some_or_else,
    some_xor,
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
    Eq,
    Ord,
    Show,
    Functor, fmap,
    Applicative, pure, (<*>),
    Monad, (>>=),
    Bool(..),
    Maybe(..),
    String,
    IO,
    error,
    return,
    ($),
  )

-- | Rust-like subtitution for the `Prelude.Maybe` type.
data Option a =
    None
  | Some a
  deriving (Eq, Ord, Show)

-- | Convert from a `Maybe`, mapping `Just` to `Some` and `Nothing` to `None`.
from_maybe :: Maybe a -> Option a
from_maybe mbe =
  case mbe of
    Just a -> Some a
    Nothing -> None

-- | Convert to a `Maybe`, mapping `Some` to `Just` and `None` to `Nothing`.
to_maybe :: Option a -> Maybe a
to_maybe opt =
  case opt of
    Some a -> Just a
    None -> Nothing

-- | @is_some opt@ returns @True@ if @opt@ is @Some@.
is_some :: Option a -> Bool
is_some opt =
  case opt of
    Some _ -> True
    None -> False

-- | @is_some_and f opt@ returns @f a@ if @opt@ is @Some a@, otherwise @False@.
is_some_and :: (a -> Bool) -> Option a -> Bool
is_some_and f opt =
  case opt of
    Some a -> f a
    None -> False

-- | @is_none opt@ returns @True@ if @opt@ is @None@.
is_none :: Option a -> Bool
is_none opt =
  case opt of
    Some _ -> False
    None -> True

-- | @some_and opt_a opt_b@ returns @opt_b@ if @opt_a@ is @Some@, otherwise
-- `None`.
some_and :: Option a -> Option b -> Option b
some_and opt_a opt_b =
  case opt_a of
    Some _ -> opt_b
    None -> None

-- | @some_and_then f opt@ returns @f a@ if @opt@ is @Some a@, otherwise @None@.
some_and_then :: (a -> Option b) -> Option a -> Option b
some_and_then f opt =
  case opt of
    Some a -> f a
    None -> None

-- | @some_then f opt@ returns @f a@ if @opt@ is @Some a@, otherwise @return@s
-- @()@.
some_then :: (a -> IO ()) -> Option a -> IO ()
some_then f opt =
  case opt of
    Some a -> f a
    None -> return ()

-- | @some_or opt_a opt_b@ returns @opt_b@ if @opt_a@ is @None@, otherwise
-- @None@.
some_or :: Option a -> Option a -> Option a
some_or opt_a opt_b =
  case opt_a of
    Some a -> Some a
    None -> opt_b

-- | @some_or_else f opt@ returns @f ()@ if @opt@ is @None@, otherwise @opt@.
some_or_else :: (() -> Option a) -> Option a -> Option a
some_or_else f opt =
  case opt of
    Some a -> Some a
    None -> f ()

-- | @some_xor opt_a opt_b@ returns whichever of the two is @Some@ only if
-- exactly one of them is @Some@, otherwise @None@.
some_xor :: Option a -> Option a -> Option a
some_xor opt_a opt_b =
  case (opt_a, opt_b) of
    (Some a, None) -> Some a
    (None, Some b) -> Some b
    _ -> None

-- | @zip opt_a opt_b@ is @Some (a, b)@ if @opt_a@ is @Some a@ and @opt_b@ is
-- @Some b@, otherwise @None@.
zip :: Option a -> Option b -> Option (a, b)
zip opt_a opt_b =
  case (opt_a, opt_b) of
    (Some a, Some b) -> Some (a, b)
    _ -> None

-- | @unzip opt@ returns @(Some a, Some b)@ if @opt@ is @Some (a, b)@, otherwise
-- @(None, None)@.
unzip :: Option (a, b) -> (Option a, Option b)
unzip opt =
  case opt of
    Some (a, b) -> (Some a, Some b)
    None -> (None, None)

-- | @flatten opt@ returns @Some a@ if @opt@ is @Some (Some a)@, otherwise
-- @None@.
flatten :: Option (Option a) -> Option a
flatten opt =
  case opt of
    Some (Some a) -> Some a
    _ -> None

-- | @map f opt@ returns @Some (f a)@ if @opt@ is @Some a@, otherwise @None@.
map :: (a -> b) -> Option a -> Option b
map f opt =
  case opt of
    Some a -> Some (f a)
    None -> None

-- | @map_or f def opt@ returns @f a@ if @opt@ is @Some a@, otherwise @def@.
map_or :: (a -> b) -> b -> Option a -> b
map_or f def opt =
  case opt of
    Some a -> f a
    None -> def

-- | @map_or_else f_some f_none@ returns @f_some a@ if @opt@ is @Some a@,
-- otherwise @f_none ()@.
map_or_else :: (a -> b) -> (() -> b) -> Option a -> b
map_or_else f_some f_none opt =
  case opt of
    Some a -> f_some a
    None -> f_none ()

-- | @unwrap_or def opt@ returns @a@ if @opt@ is @Some a@, otherwise @def@.
unwrap_or :: a -> Option a -> a
unwrap_or def opt =
  case opt of
    Some a -> a
    None -> def

-- | @unwrap_or_else f opt@ returns @a@ if @opt@ is @Some a@, otherwise @f ()@.
unwrap_or_else :: (() -> a) -> Option a -> a
unwrap_or_else f opt =
  case opt of
    Some a -> a
    None -> f ()

-- | @unwrap opt@ returns @a@ if @opt@ is @Some a@, otherwise an `error` is
-- thrown.
unwrap :: Option a -> a
unwrap opt =
  case opt of
    Some a -> a
    None -> error "`unwrap` called on a `None` variant"

-- | @expect msg opt@ returns @a@ if @opt@ is @Some a@, otherwise an `error` is
-- thrown with message @msg@.
expect :: String -> Option a -> a
expect msg opt =
  case opt of
    Some a -> a
    None -> error msg

-- | @expect_with f opt@ returns @a@ if @opt@ is @Some a@, otherwise an `error`
-- is thrown with message @f ()@.
expect_with :: (() -> String) -> Option a -> a
expect_with f opt =
  case opt of
    Some a -> a
    None -> error $ f ()

-- | @collect_list items@ returns @Some [its]@ if all elements of @items@ are
-- @Some _@, otherwise @None@.
collect_list :: [Option a] -> Option [a]
collect_list items =
  case items of
    [] -> Some []
    None : _ -> None
    (Some item) : rest ->
      case collect_list rest of
        None -> None
        Some rec_res -> Some (item : rec_res)

instance Functor Option where
  fmap = map

instance Applicative Option where
  pure = Some
  opt_f <*> m =
    case opt_f of
      Some f -> fmap f m
      None -> None

instance Monad Option where
  opt >>= f = some_and_then f opt

