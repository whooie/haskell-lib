-- | Definitions for manipulating total-spin/spin-projection pairs and
-- calculating quantities associated therewith.
module Whooie.Phys.Spin
  ( Spin
  , SpinError (..)
  , SpinResult
  , StretchedState (..)
  , new
  , ofHalves
  , ofHalvesPair
  , halves
  , newStretchedPos
  , newStretchedNeg
  , newStretched
  , isStretched
  , toFloats
  , cmp
  , refl
  , raise
  , lower
  , projections
  , projectionsRev
  , cg
  , w3jSel
  , w3j
  , w6j
  ) where

import Text.Printf (printf)
import qualified Whooie.Phys.SpinProj as Proj
import qualified Whooie.Phys.SpinTotal as Tot

-- | Error data type returned by certain operations.
data SpinError =
    InvalidProj String
  | InvalidRaise String
  | InvalidLower String
  deriving (Eq)

instance Show SpinError where
  show spinErr =
    case spinErr of
      InvalidProj msg -> printf "InvalidProj: %s" msg
      InvalidRaise msg -> printf "InvalidRaise: %s" msg
      InvalidLower msg -> printf "InvalidLower: %s" msg

type SpinResult a = Either SpinError a

-- | Main type representing a spin-@(total, projection)@ pair of quantum
-- numbers.
data Spin = Spin Tot.SpinTotal Proj.SpinProj

instance Show Spin where
  show (Spin tot proj) = printf "(%s, %s)" (show tot) (show proj)

-- | Create a new @Spin@, checking that the projection number does not exceed
-- the total in magnitude, and that both numbers have the same parity.
new :: Tot.SpinTotal -> Proj.SpinProj -> SpinResult Spin
new tot proj =
  let j = Tot.halves tot
      m = Proj.halves proj
      checkParity = j `mod` 2 == m `mod` 2
      checkMag = m `elem` [-j .. j]
      t = show tot
      p = show proj
   in if checkParity then
        if checkMag then
          Right (Spin tot proj)
        else
          let msg = printf "proj %s must not exceed tot %s in magnitude" p t
           in Left (InvalidProj msg)
      else
        let msg = printf "proj %s and tot %s must have equal parity" p t
         in Left (InvalidProj msg)

-- | @ofHalves tot proj@: Create a new @Spin@ from raw half-quanta.
ofHalves :: Int -> Int -> SpinResult Spin
ofHalves tot proj = new (Tot.new tot) (Proj.new proj)

-- | @ofHalvesPair (tot, proj)@: Create a new @Spin@ from raw half-quanta.
ofHalvesPair :: (Int, Int) -> SpinResult Spin
ofHalvesPair (tot, proj) = ofHalves tot proj

-- | Convert to a pair of numbers of half-quanta.
halves :: Spin -> (Int, Int)
halves (Spin tot proj) = (Tot.halves tot, Proj.halves proj)

-- | Convert to a pair of ordinary floating-point values.
toFloats :: Spin -> (Float, Float)
toFloats (Spin tot proj) = (Tot.toFloat tot, Proj.toFloat proj)

-- | Get the total-spin part of a @Spin@.
tot :: Spin -> Tot.SpinTotal
tot (Spin t _) = t

-- | Get the spin-projection part of a @Spin@.
proj :: Spin -> Proj.SpinProj
proj (Spin _ p) = p

data StretchedState = Pos | Neg

-- | Create a new stretched state where the projection number is equal to the
-- total in magnitude and oriented in the positive direction.
newStretchedPos :: Tot.SpinTotal -> Spin
newStretchedPos tot =
  case ofHalves (Tot.halves tot) (Tot.halves tot) of
    Right s -> s
    Left _ -> error "unreachable"

-- | Create a new stretched state where the projection number is equal to the
-- total in magnitude and oriented in the negative direction.
newStretchedNeg :: Tot.SpinTotal -> Spin
newStretchedNeg tot =
  case ofHalves (Tot.halves tot) (-(Tot.halves tot)) of
    Right s -> s
    Left _ -> error "unreachable"

-- | Create a new stretched state.
newStretched :: StretchedState -> Tot.SpinTotal -> Spin
newStretched dir tot =
  case dir of
    Pos -> newStretchedPos tot
    Neg -> newStretchedNeg tot

-- | @isStretchedPos s@: Return @True@ if @s@ is a stretched state pointing in
-- the positive direction.
isStretchedPos :: Spin -> Bool
isStretchedPos (Spin tot proj) = m == j
  where m = Proj.halves proj
        j = Tot.halves tot

-- | @isStretchedNeg s@: Return @True@ if @s@ is a stretched state pointing in
-- the negative direction.
isStretchedNeg :: Spin -> Bool
isStretchedNeg (Spin tot proj) = m == (-j)
  where m = Proj.halves proj
        j = Tot.halves tot

-- | @isStretched s@: Return @True@ if @s@ is a stretched state.
isStretched :: Spin -> Bool
isStretched s = (isStretchedPos s) || (isStretchedNeg s)

-- | Flip the sign of the projection number.
refl :: Spin -> Spin
refl (Spin tot proj) = Spin tot (Proj.refl proj)

-- | Increase the projection number by 1 if the state is not already a
-- positively stretched state.
raise :: Spin -> SpinResult Spin
raise s =
  if isStretchedPos s then
    let msg = printf "cannot raise stretched state %s" (show s)
     in Left (InvalidRaise msg)
  else
    let Spin tot proj = s
     in Right $ Spin tot (Proj.raise proj)

-- | Decrease the projection number by 1 if the state is not already a
-- negatively stretched state.
lower :: Spin -> SpinResult Spin
lower s =
  if isStretchedNeg s then
    let msg = printf "cannot lower stretched state %s" (show s)
     in Left (InvalidLower msg)
  else
    let Spin tot proj = s
     in Right $ Spin tot (Proj.lower proj)

-- | Return a list containing all available @Spin@ states with fixed total spin,
-- starting from the most negative.
projections :: Tot.SpinTotal -> [Spin]
projections tot = [Spin tot (Proj.new m) | m <- [-j, -j + 2 .. j]]
  where j = Tot.halves tot

-- | Return a list containing all available @Spin@ states with fixed total spin,
-- starting from the most positive.
projectionsRev :: Tot.SpinTotal -> [Spin]
projectionsRev tot = [Spin tot (Proj.new m) | m <- [j, j - 2 .. -j]]
  where j = Tot.halves tot

-- | Compare the projection numbers of two states if they have the same total
-- spin.
cmp :: Spin -> Spin -> Maybe Ordering
cmp (Spin totl projl) (Spin totr projr) =
  if totl == totr then Just $ compare projl projr else Nothing

ffactorial :: Int -> Float
ffactorial n = fromInteger $ toInteger $ foldl (*) 1 [2 .. n]

(//) :: Int -> Int -> Int
(//) = quot

cgSummand :: (Int, Int, Int, Int, Int, Int) -> Int -> Float
cgSummand (j1, m1, j2, m2, j12, _m12) k =
  let
    sign = if k `mod` 2 == 0 then 1.0 else -1.0
    fK = ffactorial k
    fJ1Pj2Mj12Mk = ffactorial ((j1 + j2 - j12) // 2 - k)
    fJ1Mm1Mk = ffactorial ((j1 - m1) // 2 - k)
    fJ2Pm2Mk = ffactorial ((j2 + m2) // 2 - k)
    fJ12Mj2Pm1Pk = ffactorial ((j12 - j2 + m1) // 2 + k)
    fJ12Mj1Mm2Pk = ffactorial ((j12 - j1 - m2) // 2 + k)
   in
    sign
      / fK
      / fJ1Pj2Mj12Mk
      / fJ1Mm1Mk
      / fJ2Pm2Mk
      / fJ12Mj2Pm1Pk
      / fJ12Mj1Mm2Pk

-- | @cg jm1 jm2 jm12@: Compute the Clebsch-Gordan coefficient
-- @⟨jm1, jm2∣jm12⟩@.
cg :: Spin -> Spin -> Spin -> Float
cg jm1 jm2 jm12 =
  let
    (j1, m1) = halves jm1
    (j2, m2) = halves jm2
    (j12, m12) = halves jm12
    kmin = max 0 $ max (-(j12 - j2 + m1) // 2) (-(j12 - j1 - m2) // 2)
    kmax = min ((j1 + j2 - j12) // 2) $ min ((j1 - m1) // 2) ((j2 + m2) // 2)
    fsum =
      foldl (+) 0.0 $ map (cgSummand (j1, m1, j2, m2, j12, m12)) [kmin .. kmax]
    j12t2P1 = fromInteger $ toInteger (j12 + 1)
    fJ12Pj1Mj2 = ffactorial ((j12 + j1 - j2) // 2)
    fJ12Mj1Pj2 = ffactorial ((j12 - j1 + j2) // 2)
    fJ1Pj2Mj12 = ffactorial ((j1 + j2 - j12) // 2)
    fJ1Pj2Pj12P1 = ffactorial ((j1 + j2 + j12) // 2 + 1)
    fJ12Pm12 = ffactorial ((j12 + m12) // 2)
    fJ12Mm12 = ffactorial ((j12 - m12) // 2)
    fJ1Mm1 = ffactorial ((j1 - m1) // 2)
    fJ1Pm1 = ffactorial ((j1 + m1) // 2)
    fJ2Mm2 = ffactorial ((j2 - m2) // 2)
    fJ2Pm2 = ffactorial ((j2 + m2) // 2)
   in
    if m1 + m2 /= m12 || (j1 + j2) `mod` 2 /= j12 `mod` 2 then
      0.0
    else if kmax < kmin then
      0.0
    else
      sqrt (
        j12t2P1
        * fJ12Pj1Mj2
        * fJ12Mj1Pj2
        * fJ1Pj2Mj12
        * fJ12Pm12
        * fJ12Mm12
        * fJ1Mm1
        * fJ1Pm1
        * fJ2Mm2
        * fJ2Pm2
        / fJ1Pj2Pj12P1
      ) * fsum

-- | @w3jSel jm1 jm2 jm3@: Return @True@ if @jm1@ @jm2@, and @jm3@ satisfy the
-- selection rules of the Wigner 3*j* symbol @(jm1 jm2 jm3)@.
w3jSel :: Spin -> Spin -> Spin -> Bool
w3jSel jm1 jm2 jm3 = msum0 && jsumBounds && m0ImpliesJsumEven
  where (j1, m1) = halves jm1
        (j2, m2) = halves jm2
        (j3, m3) = halves jm3
        msum0 = m1 + m2 + m3 == 0
        jsumBounds = abs (j1 - j2) <= j3 && j3 <= j1 + j2
        m0ImpliesJsumEven =
          not (m1 == 0 && m2 == 0 && m3 == 0)
          && ((j1 + j2 + j3) // 2) `mod` 2 == 0

-- | @w3j jm1 jm2 jm3@: Compute the Wigner 3*j* symbol @(jm1 jm2 jm3)@.
w3j :: Spin -> Spin -> Spin -> Float
w3j jm1 jm2 jm3 =
  let
    j1 = Tot.halves $ tot jm1
    j2 = Tot.halves $ tot jm2
    j3 = Tot.halves $ tot jm3
    m3 = Proj.halves $ proj jm3
    sign = if ((j1 - j2 - m3) // 2) `mod` 2 == 0 then 1.0 else -1.0
    denom = sqrt $ fromInteger $ toInteger $ j3 + 1
    cgVal = cg jm1 jm2 (refl jm3)
   in sign * cgVal / denom

w6jFilter :: (Spin, Spin, Spin, Spin, Spin, Spin) -> Bool
w6jFilter (jm1, jm2, jm3, jm4, jm5, jm6) =
  (w3jSel (refl jm1) (refl jm2) (refl jm3))
  && (w3jSel jm1 (refl jm5) jm6)
  && (w3jSel jm4 jm2 (refl jm6))
  && (w3jSel (refl jm4) jm5 jm3)

w6jSign :: (Spin, Spin, Spin, Spin, Spin, Spin) -> Float
w6jSign (jm1, jm2, jm3, jm4, jm5, jm6) =
  let
    (j1, m1) = halves jm1
    (j2, m2) = halves jm2
    (j3, m3) = halves jm3
    (j4, m4) = halves jm4
    (j5, m5) = halves jm5
    (j6, m6) = halves jm6
    k = (j1 - m1 + j2 - m2 + j3 - m3 + j4 - m4 + j5 - m5 + j6 - m6) // 2
   in if k `mod` 2 == 0 then 1.0 else -1.0

w6jMap :: (Spin, Spin, Spin, Spin, Spin, Spin) -> Float
w6jMap (jm1, jm2, jm3, jm4, jm5, jm6) =
  (w6jSign (jm1, jm2, jm3, jm4, jm5, jm6))
  * (w3j (refl jm1) (refl jm2) (refl jm3))
  * (w3j jm1 (refl jm5) jm6)
  * (w3j jm4 jm2 (refl jm6))
  * (w3j (refl jm4) jm5 jm6)

-- | @w6j j1 j2 j3 j4 j5 j6@: Compute the Wigner 6*j* symbol
-- @{j1 j2 j3; j4 j5 j6}@ (where @j1, ..., j3@ are on the top row).
w6j
  :: Tot.SpinTotal
  -> Tot.SpinTotal
  -> Tot.SpinTotal
  -> Tot.SpinTotal
  -> Tot.SpinTotal
  -> Tot.SpinTotal
  -> Float
w6j j1 j2 j3 j4 j5 j6 =
  foldl (+) 0.0 $ map w6jMap $ filter w6jFilter spins
    where
      pj1 = projections j1
      pj2 = projections j2
      pj3 = projections j3
      pj4 = projections j4
      pj5 = projections j5
      pj6 = projections j6
      spins = [ (jm1, jm2, jm3, jm4, jm5, jm6)
                | jm1 <- pj1
                , jm2 <- pj2
                , jm3 <- pj3
                , jm4 <- pj4
                , jm5 <- pj5
                , jm6 <- pj6 ]

