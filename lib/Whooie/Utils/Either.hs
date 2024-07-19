-- | Emulates Rust's @Either@ type, but on `Either`s.
module Whooie.Utils.Either
  ( Either (..)
  , isRight
  , isRightAnd
  , isLeft
  , isLeftAnd
  , getRight
  , getLeft
  , rightAnd
  , rightAndThen
  , rightThen
  , rightThenElse
  , rightOr
  , rightOrElse
  , map
  , mapOr
  , mapOrElse
  , mapLeft
  , justRightOr
  , justRightOrElse
  , unwrapOr
  , unwrapOrElse
  , unwrap
  , expect
  , expectWith
  , expectLeft
  , expectLeftWith
  , collectList
  , collectListLeft
  ) where

import Prelude
  ( Show
  , show
  , Bool (..)
  , Either (..)
  , Maybe (..)
  , String
  , IO
  , error
  , ($)
  , (++)
  , return
  )

-- | @isRight eith@ returns @True@ if @eith@ is @Right@.
isRight :: Either e a -> Bool
isRight eith =
  case eith of
    Right _ -> True
    Left _ -> False

-- | @isRightAnd f eith@ returns @f a@ if @eith@ is @Right a@, otherwise
-- @False@.
isRightAnd :: (a -> Bool) -> Either e a -> Bool
isRightAnd f eith =
  case eith of
    Right a -> f a
    Left _ -> False

-- | @isLeft eith@ returns @True@ if @eith@ is @Left@.
isLeft :: Either e a -> Bool
isLeft eith =
  case eith of
    Right _ -> False
    Left _ -> True

-- | @isLeftAnd f eith@ returns @f e@ if @eith@ is @Left e@, otherwise
-- @False@.
isLeftAnd :: (e -> Bool) -> Either e a -> Bool
isLeftAnd f eith =
  case eith of
    Right _ -> False
    Left e -> f e

-- | @getRight eith@ returns @Just a@ if @eith@ is @Right a@, otherwise
-- @Nothing@.
getRight :: Either e a -> Maybe a
getRight eith =
  case eith of
    Right a -> Just a
    Left _ -> Nothing

-- | @getLeft eith@ returns @Just e@ if @eith@ is @Left e@, otherwise
-- @Nothing@.
getLeft :: Either e a -> Maybe e
getLeft eith =
  case eith of
    Right _ -> Nothing
    Left e -> Just e

-- | @rightAnd eithA eithB@ returns @eithB@ if @eithA@ is @Right@,
-- otherwise @eithA@.
rightAnd :: Either e a -> Either e b -> Either e b
rightAnd eithA eithB =
  case eithA of
    Right _ -> eithB
    Left e -> Left e

-- | @rightAndThen f eith@ returns @f a@ if @eith@ is @Right a@, otherwise
-- @eith@.
rightAndThen :: (a -> Either e b) -> Either e a -> Either e b
rightAndThen f eith =
  case eith of
    Right a -> f a
    Left e -> Left e

-- | @rightThen f eith@ calls @f@ on the wrapped @Right@ value, otherwise
-- @return@s @()@.
rightThen :: (a -> IO ()) -> Either e a -> IO ()
rightThen f eith =
  case eith of
    Right a -> f a
    Left _ -> return ()

-- | @rightThenElse fLeft fRight eith@ calls @fRight@ on the wrapped
-- @Right@ value or @fLeft@ on the wrapped @Left@ value.
rightThenElse :: (e -> IO ()) -> (a -> IO ()) -> Either e a -> IO ()
rightThenElse fLeft fRight eith =
  case eith of
    Right a -> fRight a
    Left e -> fLeft e

-- | @rightOr eithA eithB@ returns @eithB@ if @eithA@ is @Left@, otherwise
-- @eithA@.
rightOr :: Either e a -> Either f a -> Either f a
rightOr eithA eithB =
  case eithA of
    Right a -> Right a
    Left _ -> eithB

-- | @rightOrElse f eith@ returns @f e@ if @eith@ is @Left e@, otherwise
-- @eith@.
rightOrElse :: (e -> Either f a) -> Either e a -> Either f a
rightOrElse f eith =
  case eith of
    Right a -> Right a
    Left e -> f e

-- | @map f eith@ returns @Right (f a)@ if @eith@ is @Right a@, otherwise @eith@.
map :: (a -> b) -> Either e a -> Either e b
map f eith =
  case eith of
    Right a -> Right (f a)
    Left e -> Left e

-- | @mapOr f def eith@ returns @f a@ if @eith@ is @Right a@, otherwise @def@.
mapOr :: (a -> b) -> b -> Either e a -> b
mapOr f def eith =
  case eith of
    Right a -> f a
    Left _ -> def

-- | @mapOrElse fLeft fRight eith@ returns @fRight a@ when @eith@ is @Right
-- a@, otherwise @fLeft e@ when @eith@ is @Left e@.
mapOrElse :: (e -> b) -> (a -> b) -> Either e a -> b
mapOrElse fLeft fRight eith =
  case eith of
    Right a -> fRight a
    Left e -> fLeft e

-- | @mapLeft f eith@ returns @Leftor (f e)@ if @eith@ is @Leftor e@, otherwise
-- @eith@.
mapLeft :: (e -> f) -> Either e a -> Either f a
mapLeft f eith =
  case eith of
    Right a -> Right a
    Left e -> Left (f e)

-- | @justRightOr err opt@ returns @Right a@ if @opt@ is @Just a@, otherwise
-- @Left err@. See `Either` for more info.
justRightOr :: e -> Maybe a -> Either e a
justRightOr err opt =
  case opt of
    Just a -> Right a
    Nothing -> Left err

-- | @justRightOrElse f opt@ returns @Right a@ if @opt@ is @Just a@,
-- otherwise @Left (f ())@. See `Either` for more info.
justRightOrElse :: (() -> e) -> Maybe a -> Either e a
justRightOrElse f opt =
  case opt of
    Just a -> Right a
    Nothing -> Left (f ())

-- | @unwrapOr def eith@ returns @a@ if @eith@ is @Right a@, otherwise @def@.
unwrapOr :: a -> Either e a -> a
unwrapOr def eith =
  case eith of
    Right a -> a
    Left _ -> def

-- | @unwrapOrElse f eith@ returns @a@ if @eith@ is @Right a@, otherwise @f e@
-- when @eith@ is @Left e@.
unwrapOrElse :: (e -> a) -> Either e a -> a
unwrapOrElse f eith =
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

-- | @expectWith f eith@ returns @a@ if @eith@ is @Right a@, otherwise an
-- `error` is thrown with message @f ()@.
expectWith :: (e -> String) -> Either e a -> a
expectWith f eith =
  case eith of
    Right a -> a
    Left e -> error $ f e

-- | @expectLeft msg eith@ returns @e@ if @eith@ is @Left e@, otherwise an
-- `error` is thrown with message @msg@.
expectLeft :: String -> Either e a -> e
expectLeft msg eith =
  case eith of
    Right _ -> error msg
    Left e -> e

-- | @expectLeftWith f eith@ returns @e@ if @eith@ is @Left e@, otherwise an
-- `error` is thrown with message @f ()@.
expectLeftWith :: (a -> String) -> Either e a -> e
expectLeftWith f eith =
  case eith of
    Right a -> error $ f a
    Left e -> e

-- | @collectList items@ returns @Right [its]@ if all elements of @items@ are
-- @Right @, otherwise the leftmost @Left@.
collectList :: [Either e a] -> Either e [a]
collectList items =
  case items of
    [] -> Right []
    (Left e) : _ -> Left e
    (Right item) : rest ->
      case collectList rest of
        Left e -> Left e
        Right recRes -> Right (item : recRes)

-- | @collectList items@ returns @Left [its]@ if all elements of @items@ are
-- @Left @, otherwise the leftmost @Right@.
collectListLeft :: [Either e a] -> Either [e] a
collectListLeft items =
  case items of
    [] -> Left []
    (Right a) : _ -> Right a
    (Left item) : rest ->
      case collectListLeft rest of
        Right a -> Right a
        Left recRes -> Left (item : recRes)

