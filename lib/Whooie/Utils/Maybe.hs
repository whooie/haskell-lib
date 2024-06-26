-- | Emulates the behavior of Rust's @Maybe@ type, but on `Maybe`s.
module Whooie.Utils.Maybe
  ( isJust
  , isJustAnd
  , isNothing
  , justAnd
  , justAndThen
  , justThen
  , justOr
  , justOrElse
  , justXor
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
    Bool(..),
    Maybe(..),
    String,
    IO,
    error,
    return,
    ($),
  )

-- | @isJust opt@ returns @True@ if @opt@ is @Just@.
isJust :: Maybe a -> Bool
isJust opt =
  case opt of
    Just _ -> True
    Nothing -> False

-- | @isJustAnd f opt@ returns @f a@ if @opt@ is @Just a@, otherwise @False@.
isJustAnd :: (a -> Bool) -> Maybe a -> Bool
isJustAnd f opt =
  case opt of
    Just a -> f a
    Nothing -> False

-- | @isNothing opt@ returns @True@ if @opt@ is @Nothing@.
isNothing :: Maybe a -> Bool
isNothing opt =
  case opt of
    Just _ -> False
    Nothing -> True

-- | @justAnd optA optB@ returns @optB@ if @optA@ is @Just@, otherwise
-- `Nothing`.
justAnd :: Maybe a -> Maybe b -> Maybe b
justAnd optA optB =
  case optA of
    Just _ -> optB
    Nothing -> Nothing

-- | @justAndThen f opt@ returns @f a@ if @opt@ is @Just a@, otherwise
-- @Nothing@.
justAndThen :: (a -> Maybe b) -> Maybe a -> Maybe b
justAndThen f opt =
  case opt of
    Just a -> f a
    Nothing -> Nothing

-- | @justThen f opt@ returns @f a@ if @opt@ is @Just a@, otherwise @return@s
-- @()@.
justThen :: (a -> IO ()) -> Maybe a -> IO ()
justThen f opt =
  case opt of
    Just a -> f a
    Nothing -> return ()

-- | @justOr optA optB@ returns @optB@ if @optA@ is @Nothing@, otherwise
-- @Nothing@.
justOr :: Maybe a -> Maybe a -> Maybe a
justOr optA optB =
  case optA of
    Just a -> Just a
    Nothing -> optB

-- | @justOrElse f opt@ returns @f ()@ if @opt@ is @Nothing@, otherwise @opt@.
justOrElse :: (() -> Maybe a) -> Maybe a -> Maybe a
justOrElse f opt =
  case opt of
    Just a -> Just a
    Nothing -> f ()

-- | @justXor optA optB@ returns whichever of the two is @Just@ only if
-- exactly one of them is @Just@, otherwise @Nothing@.
justXor :: Maybe a -> Maybe a -> Maybe a
justXor optA optB =
  case (optA, optB) of
    (Just a, Nothing) -> Just a
    (Nothing, Just b) -> Just b
    _ -> Nothing

-- | @zip optA optB@ is @Just (a, b)@ if @optA@ is @Just a@ and @optB@ is
-- @Just b@, otherwise @Nothing@.
zip :: Maybe a -> Maybe b -> Maybe (a, b)
zip optA optB =
  case (optA, optB) of
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

-- | @mapOr f def opt@ returns @f a@ if @opt@ is @Just a@, otherwise @def@.
mapOr :: (a -> b) -> b -> Maybe a -> b
mapOr f def opt =
  case opt of
    Just a -> f a
    Nothing -> def

-- | @mapOrElse fJust fNothing@ returns @fJust a@ if @opt@ is @Just a@,
-- otherwise @fNothing ()@.
mapOrElse :: (a -> b) -> (() -> b) -> Maybe a -> b
mapOrElse fJust fNothing opt =
  case opt of
    Just a -> fJust a
    Nothing -> fNothing ()

-- | @unwrapOr def opt@ returns @a@ if @opt@ is @Just a@, otherwise @def@.
unwrapOr :: a -> Maybe a -> a
unwrapOr def opt =
  case opt of
    Just a -> a
    Nothing -> def

-- | @unwrapOrElse f opt@ returns @a@ if @opt@ is @Just a@, otherwise @f ()@.
unwrapOrElse :: (() -> a) -> Maybe a -> a
unwrapOrElse f opt =
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

-- | @expectWith f opt@ returns @a@ if @opt@ is @Just a@, otherwise an `error`
-- is thrown with message @f ()@.
expectWith :: (() -> String) -> Maybe a -> a
expectWith f opt =
  case opt of
    Just a -> a
    Nothing -> error $ f ()

-- | @collectList items@ returns @Just [its]@ if all elements of @items@ are
-- @Just @, otherwise @Nothing@.
collectList :: [Maybe a] -> Maybe [a]
collectList items =
  case items of
    [] -> Just []
    Nothing : _ -> Nothing
    (Just item) : rest ->
      case collectList rest of
        Nothing -> Nothing
        Just recRes -> Just (item : recRes)

