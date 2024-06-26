-- | Emulates Rust's @Result@ type. Really just for practice.
module Whooie.Utils.Result
  ( Result (..)
  , fromEither
  , toEither
  , isOk
  , isOkAnd
  , isErr
  , isErrAnd
  , getOk
  , getErr
  , okAnd
  , okAndThen
  , okThen
  , okThenElse
  , okOr
  , okOrElse
  , map
  , mapOr
  , mapOrElse
  , mapErr
  , someOkOr
  , someOkOrElse
  , unwrapOr
  , unwrapOrElse
  , unwrap
  , expect
  , expectWith
  , expectErr
  , expectErrWith
  , collectList
  ) where

import qualified Whooie.Utils.Option as Opt
import Prelude
  (
    Eq,
    Show, show,
    Functor, fmap,
    Applicative, pure, (<*>),
    Monad, (>>=),
    Bool(..),
    Either(..),
    String,
    IO,
    error,
    ($),
    (++),
    return,
  )

-- | Rust-like subtitute for the `Prelude.Either` type.
data Result e a =
    Err e
  | Ok a
  deriving (Show, Eq)

-- | Convert from an `Either`, mapping `Left` to `Err` and `Right` to `Ok`.
fromEither :: Either e a -> Result e a
fromEither eith =
  case eith of
    Left e -> Err e
    Right a -> Ok a

-- | Convert to an `Either`, mapping `Err` to `Left` and `Ok` to `Right`.
toEither :: Result e a -> Either e a
toEither res =
  case res of
    Ok a -> Right a
    Err e -> Left e

-- | @isOk res@ returns @True@ if @res@ is @Ok@.
isOk :: Result e a -> Bool
isOk res =
  case res of
    Ok _ -> True
    Err _ -> False

-- | @isOkAnd f res@ returns @f a@ if @res@ is @Ok a@, otherwise @False@.
isOkAnd :: (a -> Bool) -> Result e a -> Bool
isOkAnd f res =
  case res of
    Ok a -> f a
    Err _ -> False

-- | @isErr res@ returns @True@ if @res@ is @Err@.
isErr :: Result e a -> Bool
isErr res =
  case res of
    Ok _ -> False
    Err _ -> True

-- | @isErrAnd f res@ returns @f e@ if @res@ is @Err e@, otherwise @False@.
isErrAnd :: (e -> Bool) -> Result e a -> Bool
isErrAnd f res =
  case res of
    Ok _ -> False
    Err e -> f e

-- | @getOk res@ returns @Some a@ if @res@ is @Ok a@, otherwise @None@.
getOk :: Result e a -> Opt.Option a
getOk res =
  case res of
    Ok a -> Opt.Some a
    Err _ -> Opt.None

-- | @getErr res@ returns @Some e@ if @res@ is @Err e@, otherwise @None@.
getErr :: Result e a -> Opt.Option e
getErr res =
  case res of
    Ok _ -> Opt.None
    Err e -> Opt.Some e

-- | @okAnd resA resB@ returns @resB@ if @resA@ is @Ok@, otherwise @resA@.
okAnd :: Result e a -> Result e b -> Result e b
okAnd resA resB =
  case resA of
    Ok _ -> resB
    Err e -> Err e

-- | @okAndThen f res@ returns @f a@ if @res@ is @Ok a@, otherwise @res@.
okAndThen :: (a -> Result e b) -> Result e a -> Result e b
okAndThen f res =
  case res of
    Ok a -> f a
    Err e -> Err e

-- | @okThen f res@ calls @f@ on the wrapped @Ok@ value, otherwise @return@s
-- @()@.
okThen :: (a -> IO ()) -> Result e a -> IO ()
okThen f res =
  case res of
    Ok a -> f a
    Err _ -> return ()

-- | @okThenElse fErr fOk res@ calls @fOk@ on the wrapped @Ok@ value or
-- @fErr@ on the wrapped @Err@ value.
okThenElse :: (e -> IO ()) -> (a -> IO ()) -> Result e a -> IO ()
okThenElse fErr fOk res =
  case res of
    Ok a -> fOk a
    Err e -> fErr e

-- | @okOr resA resB@ returns @resB@ if @resA@ is @Err@, otherwise @resA@.
okOr :: Result e a -> Result f a -> Result f a
okOr resA resB =
  case resA of
    Ok a -> Ok a
    Err _ -> resB

-- | @okOrElse f res@ returns @f e@ if @res@ is @Err e@, otherwise @res@.
okOrElse :: (e -> Result f a) -> Result e a -> Result f a
okOrElse f res =
  case res of
    Ok a -> Ok a
    Err e -> f e

-- | @map f res@ returns @Ok (f a)@ if @res@ is @Ok a@, otherwise @res@.
map :: (a -> b) -> Result e a -> Result e b
map f res =
  case res of
    Ok a -> Ok (f a)
    Err e -> Err e

-- | @mapOr f def res@ returns @f a@ if @res@ is @Ok a@, otherwise @def@.
mapOr :: (a -> b) -> b -> Result e a -> b
mapOr f def res =
  case res of
    Ok a -> f a
    Err _ -> def

-- | @mapOrElse fErr fOk res@ returns @fOk a@ when @res@ is @Ok a@,
-- otherwise @fErr e@ when @res@ is @Err e@.
mapOrElse :: (e -> b) -> (a -> b) -> Result e a -> b
mapOrElse fErr fOk res =
  case res of
    Ok a -> fOk a
    Err e -> fErr e

-- | @mapErr f res@ returns @Error (f e)@ if @res@ is @Error e@, otherwise
-- @res@.
mapErr :: (e -> f) -> Result e a -> Result f a
mapErr f res =
  case res of
    Ok a -> Ok a
    Err e -> Err (f e)

-- | @someOkOr err opt@ returns @Ok a@ if @opt@ is @Some a@, otherwise @Err
-- err@. See `Result` for more info.
someOkOr :: e -> Opt.Option a -> Result e a
someOkOr err opt =
  case opt of
    Opt.Some a -> Ok a
    Opt.None -> Err err

-- | @someOkOrElse f opt@ returns @Ok a@ if @opt@ is @Some a@, otherwise @Err
-- (f ())@. See `Result` for more info.
someOkOrElse :: (() -> e) -> Opt.Option a -> Result e a
someOkOrElse f opt =
  case opt of
    Opt.Some a -> Ok a
    Opt.None -> Err (f ())

-- | @unwrapOr def res@ returns @a@ if @res@ is @Ok a@, otherwise @def@.
unwrapOr :: a -> Result e a -> a
unwrapOr def res =
  case res of
    Ok a -> a
    Err _ -> def

-- | @unwrapOrElse f res@ returns @a@ if @res@ is @Ok a@, otherwise @f e@ when
-- @res@ is @Err e@.
unwrapOrElse :: (e -> a) -> Result e a -> a
unwrapOrElse f res =
  case res of
    Ok a -> a
    Err e -> f e

-- | @unwrap res@ returns @a@ if @res@ is @Ok a@, otherwise an `error` is
-- thrown.
unwrap :: Show e => Result e a -> a
unwrap res =
  case res of
    Ok a -> a
    Err e ->
      error $ "called `unwrap` on an `Err` variant with error: " ++ (show e)

-- | @expect msg res@ returns @a@ if @res@ is @Ok a@, otherwise an `error` is
-- thrown with message @msg@.
expect :: String -> Result e a -> a
expect msg res =
  case res of
    Ok a -> a
    Err _ -> error msg

-- | @expectWith f res@ returns @a@ if @res@ is @Ok a@, otherwise an `error` is
-- thrown with message @f ()@.
expectWith :: (e -> String) -> Result e a -> a
expectWith f res =
  case res of
    Ok a -> a
    Err e -> error $ f e

-- | @expectErr msg res@ returns @e@ if @res@ is @Err e@, otherwise an `error`
-- is thrown with message @msg@.
expectErr :: String -> Result e a -> e
expectErr msg res =
  case res of
    Ok _ -> error msg
    Err e -> e

-- | @expectErrWith f res@ returns @e@ if @res@ is @Err e@, otherwise an
-- `error` is thrown with message @f ()@.
expectErrWith :: (a -> String) -> Result e a -> e
expectErrWith f res =
  case res of
    Ok a -> error $ f a
    Err e -> e

-- | @collectList items@ returns @Ok [its]@ if all elements of @items@ are
-- @Ok @, otherwise the leftmost @Err@.
collectList :: [Result e a] -> Result e [a]
collectList items =
  case items of
    [] -> Ok []
    (Err e) : _ -> Err e
    (Ok item) : rest ->
      case collectList rest of
        Err e -> Err e
        Ok recRes -> Ok (item : recRes)

instance Functor (Result e) where
  fmap = map

instance Applicative (Result e) where
  pure = Ok
  resF <*> m =
    case resF of
      Ok f -> fmap f m
      Err e -> Err e

instance Monad (Result e) where
  res >>= f = okAndThen f res

