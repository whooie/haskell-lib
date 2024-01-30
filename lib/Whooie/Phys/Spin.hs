{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Definitions for manipulating total-spin/spin-projection pairs and
-- calculating quantities associated therewith.
module Whooie.Phys.Spin
  (
    Spin,
    SpinError(..),
    SpinResult,
    StretchedState(..),
    new,
    of_halves,
    of_halves_pair,
    halves,
    new_stretched_pos,
    new_stretched_neg,
    new_stretched,
    tot,
    proj,
    is_stretched_pos,
    is_stretched_neg,
    is_stretched,
    to_floats,
    cmp,
    refl,
    raise,
    lower,
    projections,
    projections_rev,
    cg,
    w3j_sel,
    w3j,
    w6j,
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
  show spin_err =
    case spin_err of
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
  let
    j = Tot.halves tot
    m = Proj.halves proj
    check_parity = j `mod` 2 == m `mod` 2
    check_mag = m `elem` [-j .. j]
    t = show tot
    p = show proj
   in
    if check_parity then
      if check_mag then
        Right (Spin tot proj)
      else
        let
          msg = printf "projection %s must not exceed total %s in magnitude" p t
         in Left (InvalidProj msg)
    else
      let msg = printf "projection %s and total %s must have equal parity" p t
       in Left (InvalidProj msg)

-- | @of_halves tot proj@: Create a new @Spin@ from raw half-quanta.
of_halves :: Int -> Int -> SpinResult Spin
of_halves tot proj = new (Tot.new tot) (Proj.new proj)

-- | @of_halves_pair (tot, proj)@: Create a new @Spin@ from raw half-quanta.
of_halves_pair :: (Int, Int) -> SpinResult Spin
of_halves_pair (tot, proj) = of_halves tot proj

-- | Convert to a pair of numbers of half-quanta.
halves :: Spin -> (Int, Int)
halves (Spin tot proj) = (Tot.halves $ tot, Proj.halves $ proj)

-- | Convert to a pair of ordinary floating-point values.
to_floats :: Spin -> (Float, Float)
to_floats (Spin tot proj) = (Tot.to_float $ tot, Proj.to_float $ proj)

-- | Get the total-spin part of a @Spin@.
tot :: Spin -> Tot.SpinTotal
tot (Spin t _) = t

-- | Get the spin-projection part of a @Spin@.
proj :: Spin -> Proj.SpinProj
proj (Spin _ p) = p

data StretchedState = Pos | Neg

-- | Create a new stretched state where the projection number is equal to the
-- total in magnitude and oriented in the positive direction.
new_stretched_pos :: Tot.SpinTotal -> Spin
new_stretched_pos tot =
  case of_halves (Tot.halves tot) (Tot.halves tot) of
    Right s -> s
    Left _ -> error "impossible"

-- | Create a new stretched state where the projection number is equal to the
-- total in magnitude and oriented in the negative direction.
new_stretched_neg :: Tot.SpinTotal -> Spin
new_stretched_neg tot =
  case of_halves (Tot.halves tot) (-(Tot.halves tot)) of
    Right s -> s
    Left _ -> error "impossible"

-- | Create a new stretched state.
new_stretched :: StretchedState -> Tot.SpinTotal -> Spin
new_stretched dir tot =
  case dir of
    Pos -> new_stretched_pos tot
    Neg -> new_stretched_neg tot

-- | @is_stretched_pos s@: Return @True@ if @s@ is a stretched state pointing in
-- the positive direction.
is_stretched_pos :: Spin -> Bool
is_stretched_pos (Spin tot proj) = m == j
  where m = Proj.halves proj
        j = Tot.halves tot

-- | @is_stretched_neg s@: Return @True@ if @s@ is a stretched state pointing in
-- the negative direction.
is_stretched_neg :: Spin -> Bool
is_stretched_neg (Spin tot proj) = m == (-j)
  where m = Proj.halves proj
        j = Tot.halves tot

-- | @is_stretched s@: Return @True@ if @s@ is a stretched state.
is_stretched :: Spin -> Bool
is_stretched s = (is_stretched_pos s) || (is_stretched_neg s)

-- | Flip the sign of the projection number.
refl :: Spin -> Spin
refl (Spin tot proj) = Spin tot (Proj.refl proj)

-- | Increase the projection number by 1 if the state is not already a
-- positively stretched state.
raise :: Spin -> SpinResult Spin
raise s =
  if is_stretched_pos s then
    let msg = printf "cannot raise stretched state %s" (show s)
     in Left (InvalidRaise msg)
  else
    let Spin tot proj = s
     in Right $ Spin tot (Proj.raise proj)

-- | Decrease the projection number by 1 if the state is not already a
-- negatively stretched state.
lower :: Spin -> SpinResult Spin
lower s =
  if is_stretched_neg s then
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
projections_rev :: Tot.SpinTotal -> [Spin]
projections_rev tot = [Spin tot (Proj.new m) | m <- [j, j - 2 .. -j]]
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

cg_summand :: (Int, Int, Int, Int, Int, Int) -> Int -> Float
cg_summand (j1, m1, j2, m2, j12, _m12) k =
  let
    sign = if k `mod` 2 == 0 then 1.0 else -1.0
    f_k = ffactorial k
    f_j1_pj2_mj12_mk = ffactorial ((j1 + j2 - j12) // 2 - k)
    f_j1_mm1_mk = ffactorial ((j1 - m1) // 2 - k)
    f_j2_pm2_mk = ffactorial ((j2 + m2) // 2 - k)
    f_j12_mj2_pm1_pk = ffactorial ((j12 - j2 + m1) // 2 + k)
    f_j12_mj1_mm2_pk = ffactorial ((j12 - j1 - m2) // 2 + k)
   in
    sign
      / f_k
      / f_j1_pj2_mj12_mk
      / f_j1_mm1_mk
      / f_j2_pm2_mk
      / f_j12_mj2_pm1_pk
      / f_j12_mj1_mm2_pk

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
      foldl (+) 0.0 $ map (cg_summand (j1, m1, j2, m2, j12, m12)) [kmin .. kmax]
    j12t2_p1 = fromInteger $ toInteger (j12 + 1)
    f_j12_pj1_mj2 = ffactorial ((j12 + j1 - j2) // 2)
    f_j12_mj1_pj2 = ffactorial ((j12 - j1 + j2) // 2)
    f_j1_pj2_mj12 = ffactorial ((j1 + j2 - j12) // 2)
    f_j1_pj2_pj12_p1 = ffactorial ((j1 + j2 + j12) // 2 + 1)
    f_j12_pm12 = ffactorial ((j12 + m12) // 2)
    f_j12_mm12 = ffactorial ((j12 - m12) // 2)
    f_j1_mm1 = ffactorial ((j1 - m1) // 2)
    f_j1_pm1 = ffactorial ((j1 + m1) // 2)
    f_j2_mm2 = ffactorial ((j2 - m2) // 2)
    f_j2_pm2 = ffactorial ((j2 + m2) // 2)
   in
    if m1 + m2 /= m12 || (j1 + j2) `mod` 2 /= j12 `mod` 2 then
      0.0
    else if kmax < kmin then
      0.0
    else
      sqrt (
        j12t2_p1
        * f_j12_pj1_mj2
        * f_j12_mj1_pj2
        * f_j1_pj2_mj12
        * f_j12_pm12
        * f_j12_mm12
        * f_j1_mm1
        * f_j1_pm1
        * f_j2_mm2
        * f_j2_pm2
        / f_j1_pj2_pj12_p1
      ) * fsum

-- | @w3j_sel jm1 jm2 jm3@: Return @True@ if @jm1@ @jm2@, and @jm3@ satisfy the
-- selection rules of the Wigner 3*j* symbol @(jm1 jm2 jm3)@.
w3j_sel :: Spin -> Spin -> Spin -> Bool
w3j_sel jm1 jm2 jm3 = msum_0 && jsum_bounds && m_0_implies_jsum_even
  where (j1, m1) = halves jm1
        (j2, m2) = halves jm2
        (j3, m3) = halves jm3
        msum_0 = m1 + m2 + m3 == 0
        jsum_bounds = abs (j1 - j2) <= j3 && j3 <= j1 + j2
        m_0_implies_jsum_even =
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
    cg_val = cg jm1 jm2 (refl jm3)
   in sign * cg_val / denom

w6j_filter :: (Spin, Spin, Spin, Spin, Spin, Spin) -> Bool
w6j_filter (jm1, jm2, jm3, jm4, jm5, jm6) =
  (w3j_sel (refl jm1) (refl jm2) (refl jm3))
  && (w3j_sel jm1 (refl jm5) jm6)
  && (w3j_sel jm4 jm2 (refl jm6))
  && (w3j_sel (refl jm4) jm5 jm3)

w6j_sign :: (Spin, Spin, Spin, Spin, Spin, Spin) -> Float
w6j_sign (jm1, jm2, jm3, jm4, jm5, jm6) =
  let
    (j1, m1) = halves jm1
    (j2, m2) = halves jm2
    (j3, m3) = halves jm3
    (j4, m4) = halves jm4
    (j5, m5) = halves jm5
    (j6, m6) = halves jm6
    k = (j1 - m1 + j2 - m2 + j3 - m3 + j4 - m4 + j5 - m5 + j6 - m6) // 2
   in if k `mod` 2 == 0 then 1.0 else -1.0

w6j_map :: (Spin, Spin, Spin, Spin, Spin, Spin) -> Float
w6j_map (jm1, jm2, jm3, jm4, jm5, jm6) =
  (w6j_sign (jm1, jm2, jm3, jm4, jm5, jm6))
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
  foldl (+) 0.0 $ map w6j_map $ filter w6j_filter spins
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

