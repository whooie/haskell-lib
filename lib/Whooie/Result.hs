-- | Emulates Rust's @Result@ type. Really just for practice.
module Whooie.Result
  (
    Result(..),
    from_either,
    to_either,
    is_ok,
    is_ok_and,
    is_err,
    is_err_and,
    get_ok,
    get_err,
    ok_and,
    ok_and_then,
    ok_then,
    ok_then_else,
    ok_or,
    ok_or_else,
    map,
    map_or,
    map_or_else,
    map_err,
    some_ok_or,
    some_ok_or_else,
    unwrap_or,
    unwrap_or_else,
    unwrap,
    expect,
    expect_with,
    expect_err,
    expect_err_with,
    collect_list,
  ) where

import qualified Whooie.Option as Opt
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
from_either :: Either e a -> Result e a
from_either eith =
  case eith of
    Left e -> Err e
    Right a -> Ok a

-- | Convert to an `Either`, mapping `Err` to `Left` and `Ok` to `Right`.
to_either :: Result e a -> Either e a
to_either res =
  case res of
    Ok a -> Right a
    Err e -> Left e

-- | @is_ok res@ returns @True@ if @res@ is @Ok@.
is_ok :: Result e a -> Bool
is_ok res =
  case res of
    Ok _ -> True
    Err _ -> False

-- | @is_ok_and f res@ returns @f a@ if @res@ is @Ok a@, otherwise @False@.
is_ok_and :: (a -> Bool) -> Result e a -> Bool
is_ok_and f res =
  case res of
    Ok a -> f a
    Err _ -> False

-- | @is_err res@ returns @True@ if @res@ is @Err@.
is_err :: Result e a -> Bool
is_err res =
  case res of
    Ok _ -> False
    Err _ -> True

-- | @is_err_and f res@ returns @f e@ if @res@ is @Err e@, otherwise @False@.
is_err_and :: (e -> Bool) -> Result e a -> Bool
is_err_and f res =
  case res of
    Ok _ -> False
    Err e -> f e

-- | @get_ok res@ returns @Some a@ if @res@ is @Ok a@, otherwise @None@.
get_ok :: Result e a -> Opt.Option a
get_ok res =
  case res of
    Ok a -> Opt.Some a
    Err _ -> Opt.None

-- | @get_err res@ returns @Some e@ if @res@ is @Err e@, otherwise @None@.
get_err :: Result e a -> Opt.Option e
get_err res =
  case res of
    Ok _ -> Opt.None
    Err e -> Opt.Some e

-- | @ok_and res_a res_b@ returns @res_b@ if @res_a@ is @Ok@, otherwise @res_a@.
ok_and :: Result e a -> Result e b -> Result e b
ok_and res_a res_b =
  case res_a of
    Ok _ -> res_b
    Err e -> Err e

-- | @ok_and_then f res@ returns @f a@ if @res@ is @Ok a@, otherwise @res@.
ok_and_then :: (a -> Result e b) -> Result e a -> Result e b
ok_and_then f res =
  case res of
    Ok a -> f a
    Err e -> Err e

-- | @ok_then f res@ calls @f@ on the wrapped @Ok@ value, otherwise @return@s
-- @()@.
ok_then :: (a -> IO ()) -> Result e a -> IO ()
ok_then f res =
  case res of
    Ok a -> f a
    Err _ -> return ()

-- | @ok_then_else f_err f_ok res@ calls @f_ok@ on the wrapped @Ok@ value or
-- @f_err@ on the wrapped @Err@ value.
ok_then_else :: (e -> IO ()) -> (a -> IO ()) -> Result e a -> IO ()
ok_then_else f_err f_ok res =
  case res of
    Ok a -> f_ok a
    Err e -> f_err e

-- | @ok_or res_a res_b@ returns @res_b@ if @res_a@ is @Err@, otherwise @res_a@.
ok_or :: Result e a -> Result f a -> Result f a
ok_or res_a res_b =
  case res_a of
    Ok a -> Ok a
    Err _ -> res_b

-- | @ok_or_else f res@ returns @f e@ if @res@ is @Err e@, otherwise @res@.
ok_or_else :: (e -> Result f a) -> Result e a -> Result f a
ok_or_else f res =
  case res of
    Ok a -> Ok a
    Err e -> f e

-- | @map f res@ returns @Ok (f a)@ if @res@ is @Ok a@, otherwise @res@.
map :: (a -> b) -> Result e a -> Result e b
map f res =
  case res of
    Ok a -> Ok (f a)
    Err e -> Err e

-- | @map_or f def res@ returns @f a@ if @res@ is @Ok a@, otherwise @def@.
map_or :: (a -> b) -> b -> Result e a -> b
map_or f def res =
  case res of
    Ok a -> f a
    Err _ -> def

-- | @map_or_else f_err f_ok res@ returns @f_ok a@ when @res@ is @Ok a@,
-- otherwise @f_err e@ when @res@ is @Err e@.
map_or_else :: (e -> b) -> (a -> b) -> Result e a -> b
map_or_else f_err f_ok res =
  case res of
    Ok a -> f_ok a
    Err e -> f_err e

-- | @map_err f res@ returns @Error (f e)@ if @res@ is @Error e@, otherwise
-- @res@.
map_err :: (e -> f) -> Result e a -> Result f a
map_err f res =
  case res of
    Ok a -> Ok a
    Err e -> Err (f e)

-- | @some_ok_or err opt@ returns @Ok a@ if @opt@ is @Some a@, otherwise @Err
-- err@. See `Result` for more info.
some_ok_or :: e -> Opt.Option a -> Result e a
some_ok_or err opt =
  case opt of
    Opt.Some a -> Ok a
    Opt.None -> Err err

-- | @some_ok_or_else f opt@ returns @Ok a@ if @opt@ is @Some a@, otherwise @Err
-- (f ())@. See `Result` for more info.
some_ok_or_else :: (() -> e) -> Opt.Option a -> Result e a
some_ok_or_else f opt =
  case opt of
    Opt.Some a -> Ok a
    Opt.None -> Err (f ())

-- | @unwrap_or def res@ returns @a@ if @res@ is @Ok a@, otherwise @def@.
unwrap_or :: a -> Result e a -> a
unwrap_or def res =
  case res of
    Ok a -> a
    Err _ -> def

-- | @unwrap_or_else f res@ returns @a@ if @res@ is @Ok a@, otherwise @f e@ when
-- @res@ is @Err e@.
unwrap_or_else :: (e -> a) -> Result e a -> a
unwrap_or_else f res =
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

-- | @expect_with f res@ returns @a@ if @res@ is @Ok a@, otherwise an `error` is
-- thrown with message @f ()@.
expect_with :: (e -> String) -> Result e a -> a
expect_with f res =
  case res of
    Ok a -> a
    Err e -> error $ f e

-- | @expect_err msg res@ returns @e@ if @res@ is @Err e@, otherwise an `error`
-- is thrown with message @msg@.
expect_err :: String -> Result e a -> e
expect_err msg res =
  case res of
    Ok _ -> error msg
    Err e -> e

-- | @expect_err_with f res@ returns @e@ if @res@ is @Err e@, otherwise an
-- `error` is thrown with message @f ()@.
expect_err_with :: (a -> String) -> Result e a -> e
expect_err_with f res =
  case res of
    Ok a -> error $ f a
    Err e -> e

-- | @collect_list items@ returns @Ok [its]@ if all elements of @items@ are
-- @Ok _@, otherwise the leftmost @Err@.
collect_list :: [Result e a] -> Result e [a]
collect_list items =
  case items of
    [] -> Ok []
    (Err e) : _ -> Err e
    (Ok item) : rest ->
      case collect_list rest of
        Err e -> Err e
        Ok rec_res -> Ok (item : rec_res)

instance Functor (Result e) where
  fmap = map

instance Applicative (Result e) where
  pure = Ok
  res_f <*> m =
    case res_f of
      Ok f -> fmap f m
      Err e -> Err e

instance Monad (Result e) where
  res >>= f = ok_and_then f res

