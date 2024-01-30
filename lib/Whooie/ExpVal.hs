-- | Implements a real value associated with an experimental error that is
-- automatically propagated through various operations.
module Whooie.ExpVal
  (
    ExpVal,
    val,
    err,
    new,
    of_float,
    of_pair,
    ValueStrSci(..),
    ValueStrOpts(..),
    value_str,
  ) where

import Text.Printf (printf)

-- | Main data type. Each value of this type is a number @val@ associated with a
-- non-negative-valued error @err@.
data ExpVal = ExpVal { val :: Float, err :: Float }

-- | Create a new @ExpVal@.
new :: Float -> Float -> ExpVal
new v e = ExpVal { val = v, err = abs e }

-- | Convert a @Float@ to an @ExpVal@ with zero error.
of_float :: Float -> ExpVal
of_float v = ExpVal { val = v, err = 0.0 }

-- | Convert a @(Float, Float)@ pair to an @ExpVal@, with the second item
-- corresponding to the experimental error.
of_pair :: (Float, Float) -> ExpVal
of_pair (v, e) = ExpVal { val = v, err = abs e }

-- | Used in `ValueStrOpts`.
data ValueStrSci = No | Upper | Lower

is_yes :: ValueStrSci -> Bool
is_yes value_str_sci =
  case value_str_sci of
    No -> False
    Upper -> True
    Lower -> True

unwrap_or :: a -> Maybe a -> a
unwrap_or def mbe =
  case mbe of
    Just a -> a
    Nothing -> def

fround :: Float -> Float
fround = fromInteger . round

ffloor :: Float -> Float
ffloor = fromInteger . floor

float_of_int :: Int -> Float
float_of_int = fromInteger . toInteger

-- | Passed to `value_str` to control formatting.
--
-- - @trunc@: Use "truncated" notation; e.g. @0.12(3)@ as opposed to @0.12 +/-
-- 0.03@.
-- - @sign@: Include a @+@ for positive values.
-- - @sci@: Controls the use of scientific notation.
-- - @latex@: Output string include surrounding @$@'s and replaces @+/-@ with
-- @\pm@.
-- @dec@: Print numbers with this number of decimal places. @Nothing@ leaves
-- this up to the experimental error value.
data ValueStrOpts =
  ValueStrOpts {
    trunc :: Bool,
    sign :: Bool,
    sci :: ValueStrSci,
    latex :: Bool,
    dec :: Maybe Int
  }

-- | Render an @ExpVal@ as a @String@.
value_str :: ValueStrOpts -> ExpVal -> String
value_str opts expval =
  let
    x = val expval
    e = err expval
    ord_x = ffloor $ logBase 10.0 $ abs $ val expval
    ord_e =
      if (err expval) /= 0.0 then
        Just $ ffloor $ logBase 10.0 $ abs $ err expval
      else
        Nothing
    (xp, ep, zp) =
      if is_yes $ sci opts then
        let
          o_e = unwrap_or 0.0 ord_e
          xp' = (fround $ x / (10.0 ** o_e)) * (10.0 ** (o_e - ord_x))
          ep' =
            let f = (\o -> (fround $ e / (10.0 ** o)) * (10.0 ** (o - ord_x)))
             in fmap f ord_e
          zp' = floor $ max 0.0 (ord_x - o_e)
        in (xp', ep', zp')
      else
        let
          o_e = unwrap_or 0.0 ord_e
          xp' = (fround $ x / (10.0 ** o_e)) * (10.0 ** o_e)
          ep' =
            let f = (\o -> (fround $ e / (10.0 ** o)) * (10.0 ** o))
             in fmap f ord_e
          zp' = floor $ max 0.0 (negate o_e)
        in (xp', ep', zp')
    z =
      case dec opts of
        Just d -> min d zp
        Nothing -> zp
    outstr =
      let
        ex =
          case sci opts of
            No -> ""
            Upper ->
              let
                exp_sign = if ord_x < 0.0 then "-" else "+"
                exp_val = abs ord_x
               in printf "E%s%02.0f" exp_sign exp_val :: String
            Lower ->
              let
                exp_sign = if ord_x < 0.0 then "-" else "+"
                exp_val = abs ord_x
               in printf "e%s%02.0f" exp_sign exp_val :: String
       in
        if trunc opts then
          let
            value =
              printf (if sign opts then "%+.*f" else "%.*f") z xp :: String
            error_dig =
              case ep of
                Just er -> printf "%.0f" (er * (10.0 ** (float_of_int z)))
                Nothing -> "nan"
           in printf "%s(%s)%s" value error_dig ex :: String
        else
          let
            value =
              printf (if sign opts then "%+.*f" else "%.*f") z xp :: String
            pm = if latex opts then "\\pm" else "+/-"
            error_val =
              case ep of
                Just er -> printf "%.*f" z er
                Nothing -> "nan"
           in printf "%s%s %s %s%s" value ex pm error_val ex :: String
  in if latex opts then "$" ++ outstr ++ "$" else outstr

-- | Calls `value_str` with [formatting options](#t:ValueStrOpts) @trunc =
-- True@, @sign = False@, @sci = No@, @latex = False@, @dec = Nothing@.
instance Show ExpVal where
  show expval = value_str opts expval
    where
      opts = ValueStrOpts { trunc = True
                          , sign = False
                          , sci = No
                          , latex = False
                          , dec = Nothing }

instance Eq ExpVal where
  l == r = (val l) == (val r)
  l /= r = (val l) /= (val r)

instance Ord ExpVal where
  compare l r = Prelude.compare (val l) (val r)
  l < r = o == LT
    where o = compare l r
  l <= r = o == LT || o == EQ
    where o = compare l r
  l > r = o == GT
    where o = compare l r
  l >= r = o == GT || o == EQ
    where o = compare l r
  min l r =
    case Prelude.compare (val l) (val r) of
      LT -> l
      EQ -> l
      GT -> r
  max l r =
    case Prelude.compare (val l) (val r) of
      GT -> l
      EQ -> l
      LT -> r

instance Num ExpVal where
  l + r = ExpVal { val = v, err = e }
    where
      v = (val l) + (val r)
      e = sqrt $ ((err l) ** 2.0) + ((err r) ** 2.0)
  l - r = ExpVal { val = v, err = e }
    where
      v = (val l) - (val r)
      e = sqrt $ ((err l) ** 2.0) + ((err r) ** 2.0)
  l * r = ExpVal { val = v, err = e }
    where
      v = (val l) * (val r)
      e = sqrt $ (((err l) * (val r)) ** 2.0) + (((val l) * (err r)) ** 2.0)
  negate x = ExpVal { val = negate $ val x, err = err x }
  abs x = ExpVal { val = abs $ val x, err = err x }
  signum x = ExpVal { val = signum $ val x, err = 0.0 }
  fromInteger n = ExpVal { val = fromInteger n, err = 0.0 }

instance Fractional ExpVal where
  l / r = ExpVal { val = v, err = e }
    where
      v = (val l) / (val r)
      e = sqrt $ (t1 ** 2.0) + (t2 ** 2.0)
        where
          t1 = (err l) / (val r)
          t2 = (err r) * (val l) / ((val r) ** 2.0)
  recip x = ExpVal { val = v, err = e }
    where
      v = recip $ val x
      e = abs $ (err x) / ((val x) ** 2.0)
  fromRational r = ExpVal { val = fromRational r, err = 0.0 }

instance Floating ExpVal where
  pi = ExpVal { val = Prelude.pi, err = 0.0 }
  exp x = ExpVal { val = v, err = (err x) * v }
    where v = exp $ val x
  log x = ExpVal { val = log $ val x, err = (err x) / (abs $ val x) }
  sqrt x = ExpVal { val = v, err = (err x) / v / 2.0 }
    where v = sqrt $ val x
  x ** y = ExpVal { val = v, err = e }
    where
      v = (val x) ** (val y)
      e = ((val x) ** ((val y) - 1.0)) * (sqrt $ (t1 ** 2.0) + (t2 ** 2.0))
        where
          t1 = (err x) * (val y)
          t2 = (err y) * (val x) * (log $ val x)
  logBase b x = ExpVal { val = v, err = e }
    where
      v = logBase (val b) (val x)
      e = (sqrt $ (t1 ** 2.0) + (t2 ** 2.0)) / ((log $ val b) ** 2.0)
        where
          t1 = (err x) / (val x)
          t2 = (err b) * v / (val b)
  sin x = ExpVal { val = v, err = e }
    where
      v = sin $ val x
      e = (err x) * (abs $ cos $ val x)
  cos x = ExpVal { val = v, err = e }
    where
      v = cos $ val x
      e = (err x) * (abs $ sin $ val x)
  tan x = ExpVal { val = v, err = e }
    where
      v = tan $ val x
      e = (err x) / ((cos $ val x) ** 2.0)
  asin x = ExpVal { val = v, err = e }
    where
      v = asin $ val x
      e = (err x) / (sqrt $ 1.0 - (val x) ** 2.0)
  acos x = ExpVal { val = v, err = e }
    where
      v = acos $ val x
      e = (err x) / (sqrt $ 1.0 - (val x) ** 2.0)
  atan x = ExpVal { val = v, err = e }
    where
      v = atan $ val x
      e = (err x) / ((val x) ** 2.0 + 1.0)
  sinh x = ExpVal { val = v, err = e }
    where
      v = sinh $ val x
      e = (err x) * (cosh $ val x)
  cosh x = ExpVal { val = v, err = e }
    where
      v = cosh $ val x
      e = (err x) * (abs $ sinh $ val x)
  tanh x = ExpVal { val = v, err = e }
    where
      v = tanh $ val x
      e = (err x) / ((cosh $ val x) ** 2.0)
  asinh x = ExpVal { val = v, err = e }
    where
      v = asinh $ val x
      e = (err x) / (sqrt $ (val x) ** 2.0 + 1.0)
  acosh x = ExpVal { val = v, err = e }
    where
      v = acosh $ val x
      e = (err x) / (sqrt $ (val x) ** 2.0 - 1.0)
  atanh x = ExpVal { val = v, err = e }
    where
      v = atanh $ val x
      e = (err x) / (abs $ (val x) ** 2.0 - 1.0)

