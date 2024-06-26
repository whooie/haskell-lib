-- | Emulates Rust's @Option@ type. Really just for practice.
module Whooie.Utils.Option
  ( Option (..)
  , fromMaybe
  , toMaybe
  , isSome
  , isSomeAnd
  , isNone
  , someAnd
  , someAndThen
  , someThen
  , someOr
  , someOrElse
  , someXor
  , zip
  , unzip
  , flatten
  , map
  , mapOr
  , mapOrElse
  , unwrapOr
  , unwrapOrElse
  , unwrap
  , expect
  , expectWith
  , collectList
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
fromMaybe :: Maybe a -> Option a
fromMaybe mbe =
  case mbe of
    Just a -> Some a
    Nothing -> None

-- | Convert to a `Maybe`, mapping `Some` to `Just` and `None` to `Nothing`.
toMaybe :: Option a -> Maybe a
toMaybe opt =
  case opt of
    Some a -> Just a
    None -> Nothing

-- | @isSome opt@ returns @True@ if @opt@ is @Some@.
isSome :: Option a -> Bool
isSome opt =
  case opt of
    Some _ -> True
    None -> False

-- | @isSomeAnd f opt@ returns @f a@ if @opt@ is @Some a@, otherwise @False@.
isSomeAnd :: (a -> Bool) -> Option a -> Bool
isSomeAnd f opt =
  case opt of
    Some a -> f a
    None -> False

-- | @isNone opt@ returns @True@ if @opt@ is @None@.
isNone :: Option a -> Bool
isNone opt =
  case opt of
    Some _ -> False
    None -> True

-- | @someAnd optA optB@ returns @optB@ if @optA@ is @Some@, otherwise
-- `None`.
someAnd :: Option a -> Option b -> Option b
someAnd optA optB =
  case optA of
    Some _ -> optB
    None -> None

-- | @someAndThen f opt@ returns @f a@ if @opt@ is @Some a@, otherwise @None@.
someAndThen :: (a -> Option b) -> Option a -> Option b
someAndThen f opt =
  case opt of
    Some a -> f a
    None -> None

-- | @someThen f opt@ returns @f a@ if @opt@ is @Some a@, otherwise @return@s
-- @()@.
someThen :: (a -> IO ()) -> Option a -> IO ()
someThen f opt =
  case opt of
    Some a -> f a
    None -> return ()

-- | @someOr optA optB@ returns @optB@ if @optA@ is @None@, otherwise
-- @None@.
someOr :: Option a -> Option a -> Option a
someOr optA optB =
  case optA of
    Some a -> Some a
    None -> optB

-- | @someOrElse f opt@ returns @f ()@ if @opt@ is @None@, otherwise @opt@.
someOrElse :: (() -> Option a) -> Option a -> Option a
someOrElse f opt =
  case opt of
    Some a -> Some a
    None -> f ()

-- | @someXor optA optB@ returns whichever of the two is @Some@ only if
-- exactly one of them is @Some@, otherwise @None@.
someXor :: Option a -> Option a -> Option a
someXor optA optB =
  case (optA, optB) of
    (Some a, None) -> Some a
    (None, Some b) -> Some b
    _ -> None

-- | @zip optA optB@ is @Some (a, b)@ if @optA@ is @Some a@ and @optB@ is
-- @Some b@, otherwise @None@.
zip :: Option a -> Option b -> Option (a, b)
zip optA optB =
  case (optA, optB) of
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

-- | @mapOr f def opt@ returns @f a@ if @opt@ is @Some a@, otherwise @def@.
mapOr :: (a -> b) -> b -> Option a -> b
mapOr f def opt =
  case opt of
    Some a -> f a
    None -> def

-- | @mapOrElse fSome fNone@ returns @fSome a@ if @opt@ is @Some a@,
-- otherwise @fNone ()@.
mapOrElse :: (a -> b) -> (() -> b) -> Option a -> b
mapOrElse fSome fNone opt =
  case opt of
    Some a -> fSome a
    None -> fNone ()

-- | @unwrapOr def opt@ returns @a@ if @opt@ is @Some a@, otherwise @def@.
unwrapOr :: a -> Option a -> a
unwrapOr def opt =
  case opt of
    Some a -> a
    None -> def

-- | @unwrapOrElse f opt@ returns @a@ if @opt@ is @Some a@, otherwise @f ()@.
unwrapOrElse :: (() -> a) -> Option a -> a
unwrapOrElse f opt =
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

-- | @expectWith f opt@ returns @a@ if @opt@ is @Some a@, otherwise an `error`
-- is thrown with message @f ()@.
expectWith :: (() -> String) -> Option a -> a
expectWith f opt =
  case opt of
    Some a -> a
    None -> error $ f ()

-- | @collectList items@ returns @Some [its]@ if all elements of @items@ are
-- @Some @, otherwise @None@.
collectList :: [Option a] -> Option [a]
collectList items =
  case items of
    [] -> Some []
    None : _ -> None
    (Some item) : rest ->
      case collectList rest of
        None -> None
        Some recRes -> Some (item : recRes)

instance Functor Option where
  fmap = map

instance Applicative Option where
  pure = Some
  optF <*> m =
    case optF of
      Some f -> fmap f m
      None -> None

instance Monad Option where
  opt >>= f = someAndThen f opt

