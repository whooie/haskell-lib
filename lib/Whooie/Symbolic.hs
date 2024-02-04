{-# OPTIONS_GHC -Wno-unused-imports #-}

-- Implements very simple symbolic math on the field of real numbers.
module Whooie.Symbolic
  (
    S(..),
    const_e,
    const_frac_1_pi,
    const_frac_1_sqrt_2,
    const_frac_2_pi,
    const_frac_2_sqrt_pi,
    const_frac_pi_2,
    const_frac_pi_3,
    const_frac_pi_4,
    const_frac_pi_6,
    const_frac_pi_8,
    const_ln_2,
    const_ln_10,
    const_log2_10,
    const_log2_e,
    const_log10_2,
    const_log10_e,
    const_pi,
    const_sqrt_2,
    const_tau,
    cmp,
    is_literal,
    is_symbol,
    is_neg,
    is_abs,
    is_sign,
    is_add,
    is_sub,
    is_mul,
    is_div,
    is_sqrt,
    is_sin,
    is_cos,
    is_tan,
    is_asin,
    is_acos,
    is_atan,
    is_sinh,
    is_cosh,
    is_tanh,
    is_asinh,
    is_acosh,
    is_atanh,
    is_pow,
    is_exp,
    is_log,
    is_ln,
    is_atom,
    contains,
    get_symbols,
    get_atoms,
    diff,
    reval,
    subs_single,
    subs,
    subsf_single,
    subsf,
    Error(..),
    Result,
    evalf_single,
    evalf,
    to_string,
    to_string_haskell,
    to_string_rust,
    to_string_python,
    to_string_numpy,
  ) where

import Text.Printf (printf)

-- | Represents an unevaluated expression on the field of real numbers.
--
-- Expressions can be numeric, symbolic, or a combination of the two through a
-- series of operations. Use the constructors to create arbitrary, verbatim
-- expressions or methods in `Num`, `Fractional`, or `Floating` to make
-- simplifications where possible.
data S =
    Literal Float
  | Symbol String
  | Neg S
  | Abs S
  | Sign S
  | Add S S
  | Sub S S
  | Mul S S
  | Div S S
  | Sqrt S
  | Sin S
  | Cos S
  | Tan S
  | ASin S
  | ACos S
  | ATan S
  | Sinh S
  | Cosh S
  | Tanh S
  | ASinh S
  | ACosh S
  | ATanh S
  | Pow S S
  | Exp S
  | Log S S
  | Ln S

-- | Euler's number (e)
const_e :: S
const_e = Literal 2.718281828459045

-- | 1/π
const_frac_1_pi :: S
const_frac_1_pi = Literal 0.3183098861837907

-- | 1/√2
const_frac_1_sqrt_2 :: S
const_frac_1_sqrt_2 = Literal 0.7071067811865476

-- | 2/π
const_frac_2_pi :: S
const_frac_2_pi = Literal 0.6366197723675814

-- | 2/√π
const_frac_2_sqrt_pi :: S
const_frac_2_sqrt_pi = Literal 1.1283791670955126

-- | π/2
const_frac_pi_2 :: S
const_frac_pi_2 = Literal 1.5707963267948966

-- | π/3
const_frac_pi_3 :: S
const_frac_pi_3 = Literal 1.0471975511965979

-- | π/4
const_frac_pi_4 :: S
const_frac_pi_4 = Literal 0.7853981633974483

-- | π/6
const_frac_pi_6 :: S
const_frac_pi_6 = Literal 0.5235987755982989

-- | π/8
const_frac_pi_8 :: S
const_frac_pi_8 = Literal 0.39269908169872414

-- | ln(2)
const_ln_2 :: S
const_ln_2 = Literal 0.6931471805599453

-- | ln(10)
const_ln_10 :: S
const_ln_10 = Literal 2.302585092994046

-- | log_2(10)
const_log2_10 :: S
const_log2_10 = Literal 3.321928094887362

-- | log_2(e)
const_log2_e :: S
const_log2_e = Literal 1.4426950408889634

-- | log_10(2)
const_log10_2 :: S
const_log10_2 = Literal 0.3010299956639812

-- | log_10(e)
const_log10_e :: S
const_log10_e = Literal 0.4342944819032518

-- | Archimedes' constant (π)
const_pi :: S
const_pi = Literal 3.141592653589793

-- | √2
const_sqrt_2 :: S
const_sqrt_2 = Literal 1.4142135623730951

-- | Full circle constant (τ)
const_tau :: S
const_tau = Literal 6.283185307179586

instance Eq S where
  -- | Two expressions are equal only if:
  -- - they are both `Literal`s holding equal values
  -- - they are both identical `Symbol`s
  -- - they are completely identical
  l == r =
    case (l, r) of
      (Literal fl, Literal fr) -> fl == fr
      (Symbol sl, Symbol sr) -> sl == sr
      (Neg l, Neg r) -> l == r
      (Abs l, Abs r) -> l == r
      (Sign l, Sign r) -> l == r
      (Add ll lr, Add rl rr) -> ll == rl && lr == rr
      (Sub ll lr, Sub rl rr) -> ll == rl && lr == rr
      (Mul ll lr, Mul rl rr) -> ll == rl && lr == rr
      (Div ll lr, Div rl rr) -> ll == rl && lr == rr
      (Sqrt l, Sqrt r) -> l == r
      (Sin l, Sin r) -> l == r
      (Cos l, Cos r) -> l == r
      (Tan l, Tan r) -> l == r
      (ASin l, ASin r) -> l == r
      (ACos l, ACos r) -> l == r
      (ATan l, ATan r) -> l == r
      (Sinh l, Sinh r) -> l == r
      (Cosh l, Cosh r) -> l == r
      (Tanh l, Tanh r) -> l == r
      (ASinh l, ASinh r) -> l == r
      (ACosh l, ACosh r) -> l == r
      (ATanh l, ATanh r) -> l == r
      (Pow bl el, Pow br er) -> bl == br && el == er
      (Exp l, Exp r) -> l == r
      (Log bl xl, Log br xr) -> bl == br && xl == xr
      (Ln l, Ln r) -> l == r
      _ -> False

-- | Compare two expressions, if possible.
--
-- Two expressions are comparable if they are either identical or both
-- `Literal`s.
cmp :: S -> S -> Maybe Ordering
cmp l r =
  case (l, r) of
    (Literal lf, Literal rf) -> Just (compare lf rf)
    (l_, r_) -> if l_ == r_ then Just EQ else Nothing

instance Num S where
  fromInteger = Literal . fromInteger
  signum x =
    case x of
      Literal f -> Literal (signum f)
      Abs _ -> Literal 1.0
      Neg (Abs _) -> Literal (-1.0)
      x -> Sign x
  negate x =
    case x of
      Literal f -> Literal (negate f)
      Neg x -> x
      Mul (Neg l) r -> l * r
      Mul l (Neg r) -> l * r
      Div (Neg l) r -> l / r
      Div l (Neg r) -> l / r
      x -> Neg x
  abs x =
    case x of
      Literal f -> Literal (abs f)
      Neg x -> abs x
      x -> Abs x
  l + r =
    case (l, r) of
      (Literal l, Literal r) -> Literal (l + r)
      (l, Literal r) | r == 0.0 -> l
      (Literal l, r) | l == 0.0 -> r
      (Mul ll lr, Mul rl rr) | ll == rl -> Mul (lr + rr) ll
      (Mul ll lr, Mul rl rr) | ll == rr -> Mul (lr + rl) ll
      (Mul ll lr, Mul rl rr) | lr == rl -> Mul (ll + rr) lr
      (Mul ll lr, Mul rl rr) | lr == rr -> Mul (ll + rl) lr
      (Mul ll lr, Mul rl rr) -> ll * lr + rl * rr
      (l, Mul rl rr) | l == rl -> Mul (rr + (Literal 1.0)) l
      (l, Mul rl rr) | l == rr -> Mul (rl + (Literal 1.0)) l
      (l, Mul rl rr) -> Add l (rl * rr)
      (Mul ll lr, r) | ll == r -> Mul (lr + (Literal 1.0)) r
      (Mul ll lr, r) | lr == r -> Mul (ll + (Literal 1.0)) r
      (Mul ll lr, r) -> Add (ll * lr) r
      (Neg l, r) -> r - l
      (l, Neg r) -> l - r
      (l, r) | l == r -> Mul (Literal 2.0) l
      (l, r) -> Add l r
  l - r =
    case (l, r) of
      (Literal l, Literal r) -> Literal (l - r)
      (l, Literal r) | r == 0.0 -> l
      (Literal l, r) | l == 0.0 -> negate r
      (Mul ll lr, Mul rl rr) | ll == rl -> Mul (lr - rr) ll
      (Mul ll lr, Mul rl rr) | ll == rr -> Mul (lr - rl) ll
      (Mul ll lr, Mul rl rr) | lr == rl -> Mul (ll - rr) lr
      (Mul ll lr, Mul rl rr) | lr == rr -> Mul (ll - rl) lr
      (Mul ll lr, Mul rl rr) -> Sub (ll * lr) (rl * rr)
      (l, Mul rl rr) | l == rl -> Mul ((Literal 1.0) - rr) l
      (l, Mul rl rr) | l == rr -> Mul ((Literal 1.0) - rl) l
      (l, Mul rl rr) -> Sub l (rl * rr)
      (Mul ll lr, r) | ll == r -> Mul (lr - (Literal 1.0)) r
      (Mul ll lr, r) | lr == r -> Mul (ll - (Literal 1.0)) r
      (Mul ll lr, r) -> Sub (ll * lr) r
      (l, Neg r) -> l + r
      (l, r) | l == r -> Literal 0.0
      (l, r) -> Sub l r
  l * r =
    case (l, r) of
      (Literal l, Literal r) -> Literal (l * r)
      (_, Literal r) | r == 0.0 -> Literal 0.0
      (l, Literal r) | r == 1.0 -> l
      (l, Literal r) | r == -1.0 -> negate l
      (l, Literal r) -> Mul (Literal r) l
      (Literal l, _) | l == 0.0 -> Literal 0.0
      (Literal l, r) | l == 1.0 -> r
      (Literal l, r) | l == -1.0 -> negate r
      (Literal l, r) -> Mul (Literal l) r
      (Neg l, Neg r) -> l * r
      (l, Neg r) -> negate (l * r)
      (Neg l, r) -> negate (l * r)
      (Exp el, Exp er) -> exp (el + er)
      (Exp el, Pow br er) | br == const_e -> exp (el + er)
      (Exp el, Pow br er) -> Mul (exp el) (br ** er)
      (Pow bl el, Exp er) | bl == const_e -> exp (el + er)
      (Pow bl el, Exp er) -> Mul (bl ** el) (exp er)
      (Pow bl el, Pow br er) | bl == br -> bl ** (el + er)
      (Pow bl el, Pow br er) -> Mul (bl ** el) (br ** er)
      (l, Pow br er) | l == br -> br ** ((Literal 1.0) + er)
      (l, Pow br er) -> Mul l (br ** er)
      (Pow bl el, r) | bl == r -> bl ** (el + (Literal 1.0))
      (Pow bl el, r) -> Mul (bl ** el) r
      (l, Div rl rr) | l == rr -> rl
      (l, Div rl rr) -> Mul l (rl / rr)
      (Div ll lr, r) | lr == r -> ll
      (Div ll lr, r) -> Mul (ll / lr) r
      (l, r) | l == r -> l ** (Literal 2.0)
      (l, r) -> Mul l r

instance Fractional S where
  fromRational = Literal . fromRational
  l / r =
    case (l, r) of
      (Literal l, Literal r) -> Literal (l / r)
      (l, Literal r) | r == 1.0 -> l
      (l, Literal r) | r == -1.0 -> negate l
      (l, Literal r) -> Div l (Literal r)
      (Literal l, _) | l == 0.0 -> Literal 0.0
      (Literal l, r) -> Div (Literal l) r
      (Neg l, Neg r) -> l / r
      (l, Neg r) -> negate (l / r)
      (Neg l, r) -> negate (l / r)
      (Exp el, Exp er) -> exp (el - er)
      (Exp el, Pow br er) | const_e == br -> exp (el - er)
      (Exp el, Pow br er) -> Div (exp el) (br ** er)
      (Pow bl el, Exp er) | bl == const_e -> exp (el - er)
      (Pow bl _ , Exp er) -> Div (bl ** er) (exp er)
      (Pow bl el, Pow br er) | bl == br -> bl ** (el - er)
      (Pow bl el, Pow br er) -> Div (bl ** el) (br ** er)
      (l, Pow br er) | l == br -> br ** ((Literal 1.0) - er)
      (l, Pow br er) -> Div l (br ** er)
      (Pow bl el, r) | bl == r -> bl ** (el - (Literal 1.0))
      (Pow bl el, r) -> Div (bl ** el) r
      (l, Mul rl rr) | l == rl -> (Literal 1.0) / rr
      (l, Mul rl rr) | l == rr -> (Literal 1.0) / rl
      (l, Mul rl rr) -> Div l (rl * rr)
      (l, Div rl rr) -> (l * rr) / rl
      (Mul ll lr, r) | ll == r -> lr
      (Mul ll lr, r) | lr == r -> ll
      (Mul ll lr, r) -> Div (ll * lr) r
      (Div ll lr, r) | ll == r -> (Literal 1.0) / lr
      (Div ll lr, r) -> Div ll (lr * r)
      (l, r) | l == r -> Literal 1.0
      (l, r) -> Div l r
  recip x = (Literal 1.0) / x

instance Floating S where
  pi = const_pi
  sqrt x =
    case x of
      Literal x -> Literal (sqrt x)
      Pow b e | e == Literal 2.0 -> abs b
      x -> Sqrt x
  sin x =
    case x of
      Literal x -> Literal (sin x)
      x -> Sin x
  cos x =
    case x of
      Literal x -> Literal (cos x)
      x -> Cos x
  tan x =
    case x of
      Literal x -> Literal (tan x)
      x -> Tan x
  asin x =
    case x of
      Literal x -> Literal (asin x)
      x -> ASin x
  acos x =
    case x of
      Literal x -> Literal (acos x)
      x -> ACos x
  atan x =
    case x of
      Literal x -> Literal (atan x)
      x -> ATan x
  sinh x =
    case x of
      Literal x -> Literal (sinh x)
      x -> Sinh x
  cosh x =
    case x of
      Literal x -> Literal (cosh x)
      x -> Cosh x
  tanh x =
    case x of
      Literal x -> Literal (tanh x)
      x -> Tanh x
  asinh x =
    case x of
      Literal x -> Literal (asinh x)
      x -> ASinh x
  acosh x =
    case x of
      Literal x -> Literal (acosh x)
      x -> ACosh x
  atanh x =
    case x of
      Literal x -> Literal (atanh x)
      x -> ATanh x
  b ** e =
    case (b, e) of
      (Literal b, Literal e) -> Literal (b ** e)
      (Literal b, e) | (Literal b) == const_e -> exp e
      (Literal b, e) -> Pow (Literal b) e
      (Sqrt b, e) -> b ** (e / (Literal 2.0))
      (b, Log eb ex) | b == eb -> ex
      (b, Log eb ex) -> Pow b (logBase eb ex)
      (b, Ln ex) | b == const_e -> ex
      (b, Ln ex) -> Pow b (log ex)
      (_, Literal e) | e == 0.0 -> Literal 1.0
      (b, Literal e) | e == 0.5 -> sqrt b
      (b, Literal e) | e == 1.0 -> b
      (b, Literal e) | e == -1.0 -> (Literal 1.0) / b
      (b, Literal e) | e == -0.5 -> (Literal 1.0) / (sqrt b)
      (b, Literal e) -> Pow b (Literal e)
      (b, e) -> Pow b e
  exp x =
    case  x of
      Literal x -> Literal (exp x)
      Log b e | b == const_e -> e
      Ln e -> e
      x -> Exp x
  logBase b x =
    case (b, x) of
      (Literal b, Literal x) -> Literal ((log x) / (log b))
      (Literal b, x) | Literal b == const_e -> log x
      (Literal b, x) -> Log (Literal b) x
      (b, Sqrt x) -> (Literal 0.5) * (logBase b x)
      (b, Pow bb be) | bb == b -> be
      (b, Pow bb be) -> be * (Log b bb)
      (b, Exp be) | const_e == b -> be
      (b, Exp be) -> be * (Log b const_e)
      (b, e) -> Log b e
  log x =
    case x of
      Literal x -> Literal (log x)
      Sqrt x -> (Literal 0.5) * (log x)
      Pow b e | b == const_e -> e
      Pow b e -> e * (Ln b)
      Exp e -> e
      x -> Ln x

-- | @is_literal expr@ returns @True@ if @expr@ is `Literal`.
is_literal :: S -> Bool
is_literal expr =
  case expr of
    Literal _ -> True
    _ -> False

-- | @is_symbol expr@ returns @True@ if @expr@ is `Symbol`.
is_symbol :: S -> Bool
is_symbol expr =
  case expr of
    Symbol _ -> True
    _ -> False

-- | @is_neg expr@ returns @True@ if @expr@ is `Neg`.
is_neg :: S -> Bool
is_neg expr =
  case expr of
    Neg _ -> True
    _ -> False

-- | @is_abs expr@ returns @True@ if @expr@ is `Abs`.
is_abs :: S -> Bool
is_abs expr =
  case expr of
    Abs _ -> True
    _ -> False

-- | @is_sign expr@ returns @True@ if @expr@ is `Sign`.
is_sign :: S -> Bool
is_sign expr =
  case expr of
    Sign _ -> True
    _ -> False

-- | @is_add expr@ returns @True@ if @expr@ is `Add`.
is_add :: S -> Bool
is_add expr =
  case expr of
    Add _ _ -> True
    _ -> False

-- | @is_sub expr@ returns @True@ if @expr@ is `Sub`.
is_sub :: S -> Bool
is_sub expr =
  case expr of
    Sub _ _ -> True
    _ -> False

-- | @is_mul expr@ returns @True@ if @expr@ is `Mul`.
is_mul :: S -> Bool
is_mul expr =
  case expr of
    Mul _ _ -> True
    _ -> False

-- | @is_div expr@ returns @True@ if @expr@ is `Div`.
is_div :: S -> Bool
is_div expr =
  case expr of
    Div _ _ -> True
    _ -> False

-- | @is_sqrt expr@ returns @True@ if @expr@ is `Sqrt`.
is_sqrt :: S -> Bool
is_sqrt expr =
  case expr of
    Sqrt _ -> True
    _ -> False

-- | @is_sin expr@ returns @True@ if @expr@ is `Sin`.
is_sin :: S -> Bool
is_sin expr =
  case expr of
    Sin _ -> True
    _ -> False

-- | @is_cos expr@ returns @True@ if @expr@ is `Cos`.
is_cos :: S -> Bool
is_cos expr =
  case expr of
    Cos _ -> True
    _ -> False

-- | @is_tan expr@ returns @True@ if @expr@ is `Tan`.
is_tan :: S -> Bool
is_tan expr =
  case expr of
    Tan _ -> True
    _ -> False

-- | @is_asin expr@ returns @True@ if @expr@ is `Asin`.
is_asin :: S -> Bool
is_asin expr =
  case expr of
    ASin _ -> True
    _ -> False

-- | @is_acos expr@ returns @True@ if @expr@ is `Acos`.
is_acos :: S -> Bool
is_acos expr =
  case expr of
    ACos _ -> True
    _ -> False

-- | @is_atan expr@ returns @True@ if @expr@ is `Atan`.
is_atan :: S -> Bool
is_atan expr =
  case expr of
    ATan _ -> True
    _ -> False

-- | @is_sinh expr@ returns @True@ if @expr@ is `Sinh`.
is_sinh :: S -> Bool
is_sinh expr =
  case expr of
    Sinh _ -> True
    _ -> False

-- | @is_cosh expr@ returns @True@ if @expr@ is `Cosh`.
is_cosh :: S -> Bool
is_cosh expr =
  case expr of
    Cosh _ -> True
    _ -> False

-- | @is_tanh expr@ returns @True@ if @expr@ is `Tanh`.
is_tanh :: S -> Bool
is_tanh expr =
  case expr of
    Tanh _ -> True
    _ -> False

-- | @is_asinh expr@ returns @True@ if @expr@ is `Asinh`.
is_asinh :: S -> Bool
is_asinh expr =
  case expr of
    ASinh _ -> True
    _ -> False

-- | @is_acosh expr@ returns @True@ if @expr@ is `Acosh`.
is_acosh :: S -> Bool
is_acosh expr =
  case expr of
    ACosh _ -> True
    _ -> False

-- | @is_atanh expr@ returns @True@ if @expr@ is `Atanh`.
is_atanh :: S -> Bool
is_atanh expr =
  case expr of
    ATanh _ -> True
    _ -> False

-- | @is_pow expr@ returns @True@ if @expr@ is `Pow`.
is_pow :: S -> Bool
is_pow expr =
  case expr of
    Pow _ _ -> True
    _ -> False

-- | @is_exp expr@ returns @True@ if @expr@ is `Exp`.
is_exp :: S -> Bool
is_exp expr =
  case expr of
    Exp _ -> True
    _ -> False

-- | @is_log expr@ returns @True@ if @expr@ is `Log`.
is_log :: S -> Bool
is_log expr =
  case expr of
    Log _ _ -> True
    _ -> False

-- | @is_ln expr@ returns @True@ if @expr@ is `Ln`.
is_ln :: S -> Bool
is_ln expr =
  case expr of
    Ln _ -> True
    _ -> False

-- | @is_atom expr@ returns @True@ if @expr@ is either `Literal` or `Symbol`.
is_atom :: S -> Bool
is_atom expr = is_literal expr || is_symbol expr

-- | @contains expr subexpr@ returns @True@ if @expr@ contains at least one
-- instance of @subexpr@, verbatim.
contains :: S -> S -> Bool
contains expr subexpr =
  if expr == subexpr then
    True
  else
    case expr of
      Literal _ -> False
      Symbol _ -> False
      Neg x -> contains x subexpr
      Abs x -> contains x subexpr
      Sign x -> contains x subexpr
      Add l r -> contains l subexpr || contains r subexpr
      Sub l r -> contains l subexpr || contains r subexpr
      Mul l r -> contains l subexpr || contains r subexpr
      Div l r -> contains l subexpr || contains r subexpr
      Sqrt x -> contains x subexpr
      Sin x -> contains x subexpr
      Cos x -> contains x subexpr
      Tan x -> contains x subexpr
      ASin x -> contains x subexpr
      ACos x -> contains x subexpr
      ATan x -> contains x subexpr
      Sinh x -> contains x subexpr
      Cosh x -> contains x subexpr
      Tanh x -> contains x subexpr
      ASinh x -> contains x subexpr
      ACosh x -> contains x subexpr
      ATanh x -> contains x subexpr
      Pow b e -> contains b subexpr || contains e subexpr
      Exp x -> contains x subexpr
      Log b x -> contains b subexpr || contains x subexpr
      Ln x -> contains x subexpr

get_symbols_inner :: S -> [S] -> [S]
get_symbols_inner expr acc =
  case expr of
    Literal _ -> acc
    Symbol _ -> if expr `elem` acc then acc else expr : acc
    Neg x -> get_symbols_inner x acc
    Abs x -> get_symbols_inner x acc
    Sign x -> get_symbols_inner x acc
    Add l r -> get_symbols_inner l $ get_symbols_inner r acc
    Sub l r -> get_symbols_inner l $ get_symbols_inner r acc
    Mul l r -> get_symbols_inner l $ get_symbols_inner r acc
    Div l r -> get_symbols_inner l $ get_symbols_inner r acc
    Sqrt x -> get_symbols_inner x acc
    Sin x -> get_symbols_inner x acc
    Cos x -> get_symbols_inner x acc
    Tan x -> get_symbols_inner x acc
    ASin x -> get_symbols_inner x acc
    ACos x -> get_symbols_inner x acc
    ATan x -> get_symbols_inner x acc
    Sinh x -> get_symbols_inner x acc
    Cosh x -> get_symbols_inner x acc
    Tanh x -> get_symbols_inner x acc
    ASinh x -> get_symbols_inner x acc
    ACosh x -> get_symbols_inner x acc
    ATanh x -> get_symbols_inner x acc
    Pow b e -> get_symbols_inner b $ get_symbols_inner e acc
    Exp x -> get_symbols_inner x acc
    Log b x -> get_symbols_inner b $ get_symbols_inner x acc
    Ln x -> get_symbols_inner x acc

-- | @get_symbols expr@ returns a list of all the unique `Symbol`s contained by
-- @expr@.
get_symbols :: S -> [S]
get_symbols expr = get_symbols_inner expr []

get_atoms_inner :: S -> [S] -> [S]
get_atoms_inner expr acc =
  case expr of
    Literal _ -> if expr `elem` acc then acc else expr : acc
    Symbol _ -> if expr `elem` acc then acc else expr : acc
    Neg x -> get_atoms_inner x acc
    Abs x -> get_atoms_inner x acc
    Sign x -> get_atoms_inner x acc
    Add l r -> get_atoms_inner l $ get_atoms_inner r acc
    Sub l r -> get_atoms_inner l $ get_atoms_inner r acc
    Mul l r -> get_atoms_inner l $ get_atoms_inner r acc
    Div l r -> get_atoms_inner l $ get_atoms_inner r acc
    Sqrt x -> get_atoms_inner x acc
    Sin x -> get_atoms_inner x acc
    Cos x -> get_atoms_inner x acc
    Tan x -> get_atoms_inner x acc
    ASin x -> get_atoms_inner x acc
    ACos x -> get_atoms_inner x acc
    ATan x -> get_atoms_inner x acc
    Sinh x -> get_atoms_inner x acc
    Cosh x -> get_atoms_inner x acc
    Tanh x -> get_atoms_inner x acc
    ASinh x -> get_atoms_inner x acc
    ACosh x -> get_atoms_inner x acc
    ATanh x -> get_atoms_inner x acc
    Pow b e -> get_atoms_inner b $ get_atoms_inner e acc
    Exp x -> get_atoms_inner x acc
    Log b x -> get_atoms_inner b $ get_atoms_inner x acc
    Ln x -> get_atoms_inner x acc

-- | @get_atoms expr@ returns a list of all the unique `Symbol`s or `Literal`s
-- contained by @expr@.
get_atoms :: S -> [S]
get_atoms expr = get_atoms_inner expr []

-- | @diff var expr@ returns the partial derivative of @expr@ with respect to
-- @var@.
diff :: S -> S -> S
diff var expr =
  let dv = diff var
   in
    if expr == var then
      Literal 1.0
    else if not (contains expr var) then
      Literal 0.0
    else
      case expr of
        Literal _ -> Literal 0.0
        Symbol x -> if (Symbol x) == var then Literal 1.0 else Literal 0.0
        Neg x -> negate (dv x)
        Abs x -> (signum x) * (dv x)
        Sign _ -> Literal 0.0
        Add l r -> (dv l) + (dv r)
        Sub l r -> (dv l) - (dv r)
        Mul l r -> ((dv l) * r) + ((dv r) * l)
        Div l r -> ((dv l) / r) - ((dv r) * (l / (r ** (Literal 2.0))))
        Sqrt x -> (dv x) / ((Literal 2.0) * (sqrt x))
        Sin x -> (dv x) * (cos x)
        Cos x -> negate $ (dv x) * (sin x)
        Tan x -> (dv x) / ((cos x) ** (Literal 2.0))
        ASin x -> (dv x) / (sqrt ((Literal 1.0) - (x ** (Literal 2.0))))
        ACos x -> negate $ (dv x) / (sqrt ((Literal 1.0) - (x ** (Literal 2.0))))
        ATan x -> (dv x) / ((Literal 1.0) + (x ** (Literal 2.0)))
        Sinh x -> (dv x) * (cosh x)
        Cosh x -> (dv x) * (sinh x)
        Tanh x -> negate $ (dv x) / ((x ** (Literal 2.0)) - (Literal 1.0))
        ASinh x -> (dv x) / (sqrt ((x ** (Literal 2.0)) + (Literal 1.0)))
        ACosh x -> (dv x) / (sqrt ((x ** (Literal 2.0)) - (Literal 1.0)))
        ATanh x -> negate $ (dv x) / ((x ** (Literal 2.0)) - (Literal 1.0))
        Pow b e -> (((dv e) * (log b)) + ((dv b) * (e / b))) * (b ** e)
        Exp x -> (dv x) * (exp x)
        Log b x ->
          (negate ((dv b) / (b * ((log b) ** (Literal 2.0)))))
          + ((dv x) / (x * (log b)))
        Ln x -> (dv x) / x

-- | Re-evaluate the entire expression tree.
reval :: S -> S
reval expr =
  case expr of
    Literal f -> Literal f
    Symbol s -> Symbol s
    Neg x -> negate (reval x)
    Abs x -> abs (reval x)
    Sign x -> signum (reval x)
    Add l r -> (reval l) + (reval r)
    Sub l r -> (reval l) - (reval r)
    Mul l r -> (reval l) * (reval r)
    Div l r -> (reval l) / (reval r)
    Sqrt x -> sqrt (reval x)
    Sin x -> sin (reval x)
    Cos x -> cos (reval x)
    Tan x -> tan (reval x)
    ASin x -> asin (reval x)
    ACos x -> acos (reval x)
    ATan x -> atan (reval x)
    Sinh x -> sinh (reval x)
    Cosh x -> cosh (reval x)
    Tanh x -> tanh (reval x)
    ASinh x -> asinh (reval x)
    ACosh x -> acosh (reval x)
    ATanh x -> atanh (reval x)
    Pow b e -> (reval b) ** (reval e)
    Exp x -> exp (reval x)
    Log b x -> logBase (reval b) (reval x)
    Ln x -> log (reval x)

-- | @subs_single target replace expr@ replaces all instances of @target@ within
-- @expr@ with @replace@ and re-evaluate all sub-expressions.
--
-- @target@ must be matched verbatim in order for the substitution to be made.
subs_single :: S -> S -> S -> S
subs_single target replace expr =
  let do_subs = subs_single target replace
   in
    if expr == target then
      replace
    else
      case expr of
        Literal f -> Literal f
        Symbol s -> Symbol s
        Neg x -> negate (do_subs x)
        Abs x -> abs (do_subs x)
        Sign x -> signum (do_subs x)
        Add l r -> (do_subs l) + (do_subs r)
        Sub l r -> (do_subs l) - (do_subs r)
        Mul l r -> (do_subs l) * (do_subs r)
        Div l r -> (do_subs l) / (do_subs r)
        Sqrt x -> sqrt (do_subs x)
        Sin x -> sin (do_subs x)
        Cos x -> cos (do_subs x)
        Tan x -> tan (do_subs x)
        ASin x -> asin (do_subs x)
        ACos x -> acos (do_subs x)
        ATan x -> atan (do_subs x)
        Sinh x -> sinh (do_subs x)
        Cosh x -> cosh (do_subs x)
        Tanh x -> tanh (do_subs x)
        ASinh x -> asinh (do_subs x)
        ACosh x -> acosh (do_subs x)
        ATanh x -> atanh (do_subs x)
        Pow b e -> (do_subs b) ** (do_subs e)
        Exp x -> exp (do_subs x)
        Log b x -> logBase (do_subs b) (do_subs x)
        Ln x -> log (do_subs x)

-- | Make a series of substitutions and re-evaluate all sub-expressions.
subs :: [(S, S)] -> S -> S
subs ss expr =
  case ss of
    [] -> expr
    (target, replace) : rest -> subs rest $ subs_single target replace expr

-- | @subsf_single target replace expr@ replaces all instances of @target@
-- within @expr@ with @replace@ and re-evaluate all sub-expressions.
--
-- @target@ must be matched verbatim in order for the subtitition to be made.
subsf_single :: S -> Float -> S -> S
subsf_single target replace expr =
  let do_subs = subsf_single target replace
   in
    if expr == target then
      Literal replace
    else
      case expr of
        Literal f -> Literal f
        Symbol s -> Symbol s
        Neg x -> negate (do_subs x)
        Abs x -> abs (do_subs x)
        Sign x -> signum (do_subs x)
        Add l r -> (do_subs l) + (do_subs r)
        Sub l r -> (do_subs l) - (do_subs r)
        Mul l r -> (do_subs l) * (do_subs r)
        Div l r -> (do_subs l) / (do_subs r)
        Sqrt x -> sqrt (do_subs x)
        Sin x -> sin (do_subs x)
        Cos x -> cos (do_subs x)
        Tan x -> tan (do_subs x)
        ASin x -> asin (do_subs x)
        ACos x -> acos (do_subs x)
        ATan x -> atan (do_subs x)
        Sinh x -> sinh (do_subs x)
        Cosh x -> cosh (do_subs x)
        Tanh x -> tanh (do_subs x)
        ASinh x -> asinh (do_subs x)
        ACosh x -> acosh (do_subs x)
        ATanh x -> atanh (do_subs x)
        Pow b e -> (do_subs b) ** (do_subs e)
        Exp x -> exp (do_subs x)
        Log b x -> logBase (do_subs b) (do_subs x)
        Ln x -> log (do_subs x)

-- | Make a series of substititions and re-evaluate all sub-expressions.
subsf :: [(S, Float)] -> S -> S
subsf ss expr =
  case ss of
    [] -> expr
    (target, replace) : rest -> subsf rest $ subsf_single target replace expr

-- | Returned by @evalf*@ functions.
data Error = EvalfError [S]

-- | Alias for @Either 'Error' a@.
type Result a = Either Error a

-- | @evalf_single target replace expr@ replaces all instances of @target@
-- within @expr@ with @replace@ and attempt to re-evaluate to a single @Float@.
--
-- @Error syms@ is returned if there are any symbols remaining, where @syms@ is
-- a list of all such unique symbols.
evalf_single :: S -> Float -> S -> Result Float
evalf_single target replace expr =
  case subsf_single target replace expr of
    Literal x -> Right x
    x -> Left $ EvalfError (get_symbols x)

-- | Make a series of substitutions and attempt to re-evaluate to a single
-- @Float@.
--
-- @Error syms@ is returned if there are any symbols remaining, where @syms@ is
-- a list of all such unique symbols.
evalf :: [(S, Float)] -> S -> Result Float
evalf ss expr =
  case subsf ss expr of
    Literal x -> Right x
    x -> Left $ EvalfError (get_symbols x)

-- | @to_string expr@ is a string representation of @expr@.
to_string :: S -> String
to_string expr =
  let
    with_parens_if x tests =
      if foldl (\acc f -> acc || f x) False tests then
        "(" ++ (to_string_inner "" x) ++ ")"
      else
        to_string_inner "" x
    display_function fn_name x =
      fn_name ++ "(" ++ (to_string_inner "" x) ++ ")"
    to_string_inner acc x =
      let
        x_str =
          case x of
            Literal f -> show f
            Symbol s -> s
            Neg x -> "-" ++ (with_parens_if x [is_add, is_sub, is_neg])
            Abs x -> "|" ++ (to_string_inner "" x) ++ "|"
            Sign x -> display_function "sign" x
            Add l (Neg r) ->
              (to_string_inner "" l)
              ++ " - "
              ++ (with_parens_if r [is_add, is_sub, is_neg])
            Add l r ->
              (to_string_inner "" l)
              ++ " + "
              ++ (with_parens_if r [is_neg])
            Sub l (Neg r) ->
              (to_string_inner "" l)
              ++ " + "
              ++ (with_parens_if r [is_neg])
            Sub l r ->
              (to_string_inner "" l)
              ++ " - "
              ++ (with_parens_if r [is_add, is_sub, is_neg])
            Mul l r ->
              (with_parens_if l [is_add, is_sub])
              ++ " * "
              ++ (with_parens_if r [is_add, is_sub])
            Div l r ->
              (with_parens_if l [is_add, is_sub])
              ++ " / "
              ++ (with_parens_if r [is_add, is_sub, is_mul, is_div])
            Sqrt x -> display_function "√" x
            Sin x -> display_function "sin" x
            Cos x -> display_function "cos" x
            Tan x -> display_function "tan" x
            ASin x -> display_function "arcsin" x
            ACos x -> display_function "arccos" x
            ATan x -> display_function "arctan" x
            Sinh x -> display_function "sinh" x
            Cosh x -> display_function "cosh" x
            Tanh x -> display_function "tanh" x
            ASinh x -> display_function "arsinh" x
            ACosh x -> display_function "arcosh" x
            ATanh x -> display_function "artanh" x
            Pow b e ->
              (with_parens_if b [is_add, is_sub, is_mul, is_div, is_neg, is_pow])
              ++ "^"
              ++ (with_parens_if e [is_add, is_sub, is_mul, is_div, is_neg, is_pow])
            Exp x -> display_function "exp" x
            Log b x ->
              "log_"
              ++ (with_parens_if b [is_add, is_sub, is_mul, is_div, is_neg, is_pow])
              ++ "(" ++ (to_string_inner "" x) ++ ")"
            Ln x -> display_function "ln" x
       in acc ++ x_str
   in to_string_inner "" expr

instance Show S where
  -- | Convert to a string using `to_string`.
  show = to_string

-- | @to_string_haskell expr@ is a Haskell representation of @expr@.
to_string_haskell :: S -> String
to_string_haskell expr =
  let
    subexpr_str x =
      case x of
        Literal f -> if signum f < 0.0 then "(" ++ (show f) ++ ")" else show f
        Symbol s -> s
        _ -> "(" ++ (to_string_inner "" x) ++ ")"
    display_function fn_name arg =
      fn_name ++ " " ++ (subexpr_str arg)
    display_function2 fn_name arg1 arg2 =
      fn_name ++ " " ++ (subexpr_str arg1) ++ " " ++ (subexpr_str arg2)
    to_string_inner acc x =
      let
        x_str =
          case x of
            Literal f -> show f
            Symbol s -> s
            Neg x -> display_function "negate" x
            Abs x -> display_function "abs" x
            Sign x -> display_function "signum" x
            Add l (Neg r) -> (subexpr_str l) ++ " - " ++ (subexpr_str r)
            Add l r -> (subexpr_str l) ++ " + " ++ (subexpr_str r)
            Sub l (Neg r) -> (subexpr_str l) ++ " + " ++ (subexpr_str r)
            Sub l r -> (subexpr_str l) ++ " - " ++ (subexpr_str r)
            Mul l r -> (subexpr_str l) ++ " * " ++ (subexpr_str r)
            Div l r -> (subexpr_str l) ++ " / " ++ (subexpr_str r)
            Sqrt x -> display_function "sqrt" x
            Sin x -> display_function "sin" x
            Cos x -> display_function "cos" x
            Tan x -> display_function "tan" x
            ASin x -> display_function "asin" x
            ACos x -> display_function "acos" x
            ATan x -> display_function "atan" x
            Sinh x -> display_function "sinh" x
            Cosh x -> display_function "cosh" x
            Tanh x -> display_function "tanh" x
            ASinh x -> display_function "asinh" x
            ACosh x -> display_function "acosh" x
            ATanh x -> display_function "atanh" x
            Pow b e -> (subexpr_str b) ++ " ** " ++ (subexpr_str e)
            Exp x -> display_function "exp" x
            Log b x -> display_function2 "logBase" b x
            Ln x -> display_function "ln" x
       in acc ++ x_str
   in to_string_inner "" expr

-- | @to_string_rust expr@ is a Rust representation of @expr@.
to_string_rust :: S -> String
to_string_rust expr =
  let
    with_parens_if x tests =
      if foldl (\acc f -> acc || f x) False tests then
        "(" ++ (to_string_inner "" x) ++ ")"
      else
        to_string_inner "" x
    display_function fn_name x =
      (with_parens_if x [is_add, is_sub, is_mul, is_div, is_neg])
      ++ "." ++ fn_name ++ "()"
    to_string_inner acc x =
      let
        x_str =
          case x of
            Literal f -> show f
            Symbol s -> s
            Neg x -> "-" ++ (with_parens_if x [is_add, is_sub, is_neg])
            Abs x -> display_function "abs" x
            Sign x -> display_function "signum" x
            Add l (Neg r) ->
              (to_string_inner "" l)
              ++ " - "
              ++ (with_parens_if r [is_add, is_sub, is_neg])
            Add l r ->
              (to_string_inner "" l)
              ++ " + "
              ++ (with_parens_if r [is_neg])
            Sub l (Neg r) ->
              (to_string_inner "" l)
              ++ " + "
              ++ (with_parens_if r [is_neg])
            Sub l r ->
              (to_string_inner "" l)
              ++ " - "
              ++ (with_parens_if r [is_add, is_sub, is_neg])
            Mul l r ->
              (with_parens_if l [is_add, is_sub])
              ++ " * "
              ++ (with_parens_if r [is_add, is_sub])
            Div l r ->
              (with_parens_if l [is_add, is_sub])
              ++ " / "
              ++ (with_parens_if r [is_add, is_sub, is_mul, is_div])
            Sqrt x -> display_function "sqrt" x
            Sin x -> display_function "sin" x
            Cos x -> display_function "cos" x
            Tan x -> display_function "tan" x
            ASin x -> display_function "asin" x
            ACos x -> display_function "acos" x
            ATan x -> display_function "atan" x
            Sinh x -> display_function "sinh" x
            Cosh x -> display_function "cosh" x
            Tanh x -> display_function "tanh" x
            ASinh x -> display_function "asinh" x
            ACosh x -> display_function "acosh" x
            ATanh x -> display_function "atanh" x
            Pow b e ->
              (with_parens_if b [is_add, is_sub, is_mul, is_div, is_neg])
              ++ ".powf(" ++ (to_string_inner "" e) ++ ")"
            Exp x -> display_function "exp" x
            Log b x ->
              (with_parens_if x [is_add, is_sub, is_mul, is_div, is_neg])
              ++ ".log(" ++ (to_string_inner "" b) ++ ")"
            Ln x -> display_function "ln" x
       in acc ++ x_str
   in to_string_inner "" expr

-- | @to_string_python expr@ is a Python representation of @expr@.
to_string_python :: S -> String
to_string_python expr =
  let
    with_parens_if x tests =
      if foldl (\acc f -> acc || f x) False tests then
        "(" ++ (to_string_inner "" x) ++ ")"
      else
        to_string_inner "" x
    display_function fn_name x =
      fn_name ++ "(" ++ (to_string_inner "" x) ++ ")"
    to_string_inner acc x =
      let
        x_str =
          case x of
            Literal f -> show f
            Symbol s -> s
            Neg x -> "-" ++ (with_parens_if x [is_add, is_sub, is_neg])
            Abs x -> display_function "abs" x
            Sign x -> "copysign(1.0, " ++ (to_string_inner "" x) ++ ")"
            Add l (Neg r) ->
              (to_string_inner "" l)
              ++ " - "
              ++ (with_parens_if r [is_add, is_sub, is_neg])
            Add l r ->
              (to_string_inner "" l)
              ++ " + "
              ++ (with_parens_if r [is_neg])
            Sub l (Neg r) ->
              (to_string_inner "" l)
              ++ " + "
              ++ (with_parens_if r [is_neg])
            Sub l r ->
              (to_string_inner "" l)
              ++ " - "
              ++ (with_parens_if r [is_add, is_sub, is_neg])
            Mul l r ->
              (with_parens_if l [is_add, is_sub])
              ++ " * "
              ++ (with_parens_if r [is_add, is_sub])
            Div l r ->
              (with_parens_if l [is_add, is_sub])
              ++ " / "
              ++ (with_parens_if r [is_add, is_sub, is_mul, is_div])
            Sqrt x -> display_function "sqrt" x
            Sin x -> display_function "sin" x
            Cos x -> display_function "cos" x
            Tan x -> display_function "tan" x
            ASin x -> display_function "asin" x
            ACos x -> display_function "acos" x
            ATan x -> display_function "atan" x
            Sinh x -> display_function "sinh" x
            Cosh x -> display_function "cosh" x
            Tanh x -> display_function "tanh" x
            ASinh x -> display_function "asinh" x
            ACosh x -> display_function "acosh" x
            ATanh x -> display_function "atanh" x
            Pow b e ->
              (with_parens_if b [is_add, is_sub, is_mul, is_div, is_neg])
              ++ " ** "
              ++ (with_parens_if e [is_add, is_sub, is_mul, is_div, is_neg])
            Exp x -> display_function "exp" x
            Log b x ->
              "log("
              ++ (to_string_inner "" x)
              ++ ", "
              ++ (to_string_inner "" b)
              ++ ")"
            Ln x -> display_function "log" x
       in acc ++ x_str
   in to_string_inner "" expr

-- | @to_string_numpy expr@ is a NumPy representation of @expr@.
to_string_numpy :: S -> String
to_string_numpy expr =
  let
    with_parens_if x tests =
      if foldl (\acc f -> acc || f x) False tests then
        "(" ++ (to_string_inner "" x) ++ ")"
      else
        to_string_inner "" x
    display_function fn_name x =
      "np." ++ fn_name ++ "(" ++ (to_string_inner "" x) ++ ")"
    to_string_inner acc x =
      let
        x_str =
          case x of
            Literal f -> show f
            Symbol s -> s
            Neg x -> "-" ++ (with_parens_if x [is_add, is_sub, is_neg])
            Abs x -> display_function "abs" x
            Sign x -> display_function "sign" x
            Add l (Neg r) ->
              (to_string_inner "" l)
              ++ " - "
              ++ (with_parens_if r [is_add, is_sub, is_neg])
            Add l r ->
              (to_string_inner "" l)
              ++ " + "
              ++ (with_parens_if r [is_neg])
            Sub l (Neg r) ->
              (to_string_inner "" l)
              ++ " + "
              ++ (with_parens_if r [is_neg])
            Sub l r ->
              (to_string_inner "" l)
              ++ " - "
              ++ (with_parens_if r [is_add, is_sub, is_neg])
            Mul l r ->
              (with_parens_if l [is_add, is_sub])
              ++ " * "
              ++ (with_parens_if r [is_add, is_sub])
            Div l r ->
              (with_parens_if l [is_add, is_sub])
              ++ " / "
              ++ (with_parens_if r [is_add, is_sub, is_mul, is_div])
            Sqrt x -> display_function "sqrt" x
            Sin x -> display_function "sin" x
            Cos x -> display_function "cos" x
            Tan x -> display_function "tan" x
            ASin x -> display_function "arcsin" x
            ACos x -> display_function "arccos" x
            ATan x -> display_function "arctan" x
            Sinh x -> display_function "sinh" x
            Cosh x -> display_function "cosh" x
            Tanh x -> display_function "tanh" x
            ASinh x -> display_function "arcsinh" x
            ACosh x -> display_function "arccosh" x
            ATanh x -> display_function "arctanh" x
            Pow b e ->
              (with_parens_if b [is_add, is_sub, is_mul, is_div, is_neg, is_log])
              ++ " ** "
              ++ (with_parens_if e [is_add, is_sub, is_mul, is_div, is_neg, is_log])
            Exp x -> display_function "exp" x
            Log b x ->
              "np.log("
              ++ (to_string_inner "" x)
              ++ ") / np.log( "
              ++ (to_string_inner "" b)
              ++ ")"
            Ln x -> display_function "log" x
       in acc ++ x_str
   in to_string_inner "" expr

-- Literal f
-- Symbol s
-- Neg x
-- Abs x
-- Sign x
-- Add l r
-- Sub l r
-- Mul l r
-- Div l r
-- Sqrt x
-- Sin x
-- Cos x
-- Tan x
-- ASin x
-- ACos x
-- ATan x
-- Sinh x
-- Cosh x
-- Tanh x
-- ASinh x
-- ACosh x
-- ATanh x
-- Pow b e
-- Exp x
-- Log b x
-- Ln x
