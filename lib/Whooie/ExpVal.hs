-- | Implements a real value associated with an experimental error that is
-- automatically propagated through various operations.
module Whooie.ExpVal
  ( ExpVal
  , val
  , err
  , new
  , ofDouble
  , ofPair
  , ValueStrSci (..)
  , ValueStrOpts (..)
  , valueStr
  ) where

import Text.Printf (printf)

-- | Main data type. Each value of this type is a number @val@ associated with a
-- non-negative-valued error @err@.
data ExpVal = ExpVal { val :: Double, err :: Double }

-- | Create a new @ExpVal@.
new :: Double -> Double -> ExpVal
new v e = ExpVal { val = v, err = abs e }

-- | Convert a @Double@ to an @ExpVal@ with zero error.
ofDouble :: Double -> ExpVal
ofDouble v = ExpVal { val = v, err = 0.0 }

-- | Convert a @(Double, Double)@ pair to an @ExpVal@, with the second item
-- corresponding to the experimental error.
ofPair :: (Double, Double) -> ExpVal
ofPair (v, e) = ExpVal { val = v, err = abs e }

-- | Used in `ValueStrOpts`.
data ValueStrSci = No | Upper | Lower

isYes :: ValueStrSci -> Bool
isYes valueStrSci =
  case valueStrSci of
    No -> False
    Upper -> True
    Lower -> True

unwrapOr :: a -> Maybe a -> a
unwrapOr def mbe =
  case mbe of
    Just a -> a
    Nothing -> def

fround :: Double -> Double
fround = fromInteger . round

ffloor :: Double -> Double
ffloor = fromInteger . floor

floatOfInt :: Int -> Double
floatOfInt = fromInteger . toInteger

-- | Passed to `valueStr` to control formatting.
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
  ValueStrOpts { trunc :: Bool
               , sign :: Bool
               , sci :: ValueStrSci
               , latex :: Bool
               , dec :: Maybe Int }

-- | Render an @ExpVal@ as a @String@.
valueStr :: ValueStrOpts -> ExpVal -> String
valueStr opts expval =
  let
    x = val expval
    e = err expval
    ordX = ffloor $ logBase 10.0 $ abs $ val expval
    ordE =
      if (err expval) /= 0.0 then
        Just $ ffloor $ logBase 10.0 $ abs $ err expval
      else
        Nothing
    (xp, ep, zp) =
      if isYes $ sci opts then
        let
          oE = unwrapOr 0.0 ordE
          xp' = (fround $ x / (10.0 ** oE)) * (10.0 ** (oE - ordX))
          ep' =
            let f = (\o -> (fround $ e / (10.0 ** o)) * (10.0 ** (o - ordX)))
             in fmap f ordE
          zp' = floor $ max 0.0 (ordX - oE)
        in (xp', ep', zp')
      else
        let
          oE = unwrapOr 0.0 ordE
          xp' = (fround $ x / (10.0 ** oE)) * (10.0 ** oE)
          ep' =
            let f = (\o -> (fround $ e / (10.0 ** o)) * (10.0 ** o))
             in fmap f ordE
          zp' = floor $ max 0.0 (negate oE)
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
                expSign = if ordX < 0.0 then "-" else "+"
                expVal = abs ordX
               in printf "E%s%02.0f" expSign expVal :: String
            Lower ->
              let
                expSign = if ordX < 0.0 then "-" else "+"
                expVal = abs ordX
               in printf "e%s%02.0f" expSign expVal :: String
       in
        if trunc opts then
          let
            value =
              printf (if sign opts then "%+.*f" else "%.*f") z xp :: String
            errorDig =
              case ep of
                Just er -> printf "%.0f" (er * (10.0 ** (floatOfInt z)))
                Nothing -> "nan"
           in printf "%s(%s)%s" value errorDig ex :: String
        else
          let
            value =
              printf (if sign opts then "%+.*f" else "%.*f") z xp :: String
            pm = if latex opts then "\\pm" else "+/-"
            errorVal =
              case ep of
                Just er -> printf "%.*f" z er
                Nothing -> "nan"
           in printf "%s%s %s %s%s" value ex pm errorVal ex :: String
  in if latex opts then "$" ++ outstr ++ "$" else outstr

-- | Calls `valueStr` with [formatting options](#t:ValueStrOpts) @trunc =
-- True@, @sign = False@, @sci = No@, @latex = False@, @dec = Nothing@.
instance Show ExpVal where
  show expval = valueStr opts expval
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

