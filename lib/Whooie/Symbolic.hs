{-# OPTIONS_GHC -Wno-unused-imports #-}

-- Implements very simple symbolic math on the field of real numbers.
module Whooie.Symbolic
  ( S (..)
  , constE
  , constFrac1Pi
  , constFrac1Sqrt2
  , constFrac2Pi
  , constFrac2SqrtPi
  , constFracPi2
  , constFracPi3
  , constFracPi4
  , constFracPi6
  , constFracPi8
  , constLn2
  , constLn10
  , constLog210
  , constLog2E
  , constLog102
  , constLog10E
  , constPi
  , constSqrt2
  , constTau
  , cmp
  , isLiteral
  , isSymbol
  , isNeg
  , isAbs
  , isSign
  , isAdd
  , isSub
  , isMul
  , isDiv
  , isSqrt
  , isSin
  , isCos
  , isTan
  , isAsin
  , isAcos
  , isAtan
  , isSinh
  , isCosh
  , isTanh
  , isAsinh
  , isAcosh
  , isAtanh
  , isPow
  , isExp
  , isLog
  , isLn
  , isAtom
  , contains
  , getSymbols
  , getAtoms
  , diff
  , reval
  , subsSingle
  , subs
  , subsfSingle
  , subsf
  , Error (..)
  , Result
  , evalfSingle
  , evalf
  , toString
  , toStringHaskell
  , toStringRust
  , toStringPython
  , toStringNumpy
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
constE :: S
constE = Literal 2.718281828459045

-- | 1/π
constFrac1Pi :: S
constFrac1Pi = Literal 0.3183098861837907

-- | 1/√2
constFrac1Sqrt2 :: S
constFrac1Sqrt2 = Literal 0.7071067811865476

-- | 2/π
constFrac2Pi :: S
constFrac2Pi = Literal 0.6366197723675814

-- | 2/√π
constFrac2SqrtPi :: S
constFrac2SqrtPi = Literal 1.1283791670955126

-- | π/2
constFracPi2 :: S
constFracPi2 = Literal 1.5707963267948966

-- | π/3
constFracPi3 :: S
constFracPi3 = Literal 1.0471975511965979

-- | π/4
constFracPi4 :: S
constFracPi4 = Literal 0.7853981633974483

-- | π/6
constFracPi6 :: S
constFracPi6 = Literal 0.5235987755982989

-- | π/8
constFracPi8 :: S
constFracPi8 = Literal 0.39269908169872414

-- | ln(2)
constLn2 :: S
constLn2 = Literal 0.6931471805599453

-- | ln(10)
constLn10 :: S
constLn10 = Literal 2.302585092994046

-- | log2(10)
constLog210 :: S
constLog210 = Literal 3.321928094887362

-- | log2(e)
constLog2E :: S
constLog2E = Literal 1.4426950408889634

-- | log10(2)
constLog102 :: S
constLog102 = Literal 0.3010299956639812

-- | log10(e)
constLog10E :: S
constLog10E = Literal 0.4342944819032518

-- | Archimedes' constant (π)
constPi :: S
constPi = Literal 3.141592653589793

-- | √2
constSqrt2 :: S
constSqrt2 = Literal 1.4142135623730951

-- | Full circle constant (τ)
constTau :: S
constTau = Literal 6.283185307179586

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
    (l, r) -> if l == r then Just EQ else Nothing

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
      (Exp el, Pow br er) | br == constE -> exp (el + er)
      (Exp el, Pow br er) -> Mul (exp el) (br ** er)
      (Pow bl el, Exp er) | bl == constE -> exp (el + er)
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
      (Exp el, Pow br er) | constE == br -> exp (el - er)
      (Exp el, Pow br er) -> Div (exp el) (br ** er)
      (Pow bl el, Exp er) | bl == constE -> exp (el - er)
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
  pi = constPi
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
      (Literal b, e) | (Literal b) == constE -> exp e
      (Literal b, e) -> Pow (Literal b) e
      (Sqrt b, e) -> b ** (e / (Literal 2.0))
      (b, Log eb ex) | b == eb -> ex
      (b, Log eb ex) -> Pow b (logBase eb ex)
      (b, Ln ex) | b == constE -> ex
      (b, Ln ex) -> Pow b (log ex)
      (_, Literal e) | e == 0.0 -> Literal 1.0
      (b, Literal e) | e == 0.5 -> sqrt b
      (b, Literal e) | e == 1.0 -> b
      (b, Literal e) | e == -1.0 -> (Literal 1.0) / b
      (b, Literal e) | e == -0.5 -> (Literal 1.0) / (sqrt b)
      (b, Literal e) -> Pow b (Literal e)
      (b, e) -> Pow b e
  exp x =
    case x of
      Literal x -> Literal (exp x)
      Log b e | b == constE -> e
      Ln e -> e
      x -> Exp x
  logBase b x =
    case (b, x) of
      (Literal b, Literal x) -> Literal ((log x) / (log b))
      (Literal b, x) | Literal b == constE -> log x
      (Literal b, x) -> Log (Literal b) x
      (b, Sqrt x) -> (Literal 0.5) * (logBase b x)
      (b, Pow bb be) | bb == b -> be
      (b, Pow bb be) -> be * (Log b bb)
      (b, Exp be) | constE == b -> be
      (b, Exp be) -> be * (Log b constE)
      (b, e) -> Log b e
  log x =
    case x of
      Literal x -> Literal (log x)
      Sqrt x -> (Literal 0.5) * (log x)
      Pow b e | b == constE -> e
      Pow b e -> e * (Ln b)
      Exp e -> e
      x -> Ln x

-- | @isLiteral expr@ returns @True@ if @expr@ is `Literal`.
isLiteral :: S -> Bool
isLiteral expr =
  case expr of
    Literal _ -> True
    _ -> False

-- | @isSymbol expr@ returns @True@ if @expr@ is `Symbol`.
isSymbol :: S -> Bool
isSymbol expr =
  case expr of
    Symbol _ -> True
    _ -> False

-- | @isNeg expr@ returns @True@ if @expr@ is `Neg`.
isNeg :: S -> Bool
isNeg expr =
  case expr of
    Neg _ -> True
    _ -> False

-- | @isAbs expr@ returns @True@ if @expr@ is `Abs`.
isAbs :: S -> Bool
isAbs expr =
  case expr of
    Abs _ -> True
    _ -> False

-- | @isSign expr@ returns @True@ if @expr@ is `Sign`.
isSign :: S -> Bool
isSign expr =
  case expr of
    Sign _ -> True
    _ -> False

-- | @isAdd expr@ returns @True@ if @expr@ is `Add`.
isAdd :: S -> Bool
isAdd expr =
  case expr of
    Add _ _ -> True
    _ -> False

-- | @isSub expr@ returns @True@ if @expr@ is `Sub`.
isSub :: S -> Bool
isSub expr =
  case expr of
    Sub _ _ -> True
    _ -> False

-- | @isMul expr@ returns @True@ if @expr@ is `Mul`.
isMul :: S -> Bool
isMul expr =
  case expr of
    Mul _ _ -> True
    _ -> False

-- | @isDiv expr@ returns @True@ if @expr@ is `Div`.
isDiv :: S -> Bool
isDiv expr =
  case expr of
    Div _ _ -> True
    _ -> False

-- | @isSqrt expr@ returns @True@ if @expr@ is `Sqrt`.
isSqrt :: S -> Bool
isSqrt expr =
  case expr of
    Sqrt _ -> True
    _ -> False

-- | @isSin expr@ returns @True@ if @expr@ is `Sin`.
isSin :: S -> Bool
isSin expr =
  case expr of
    Sin _ -> True
    _ -> False

-- | @isCos expr@ returns @True@ if @expr@ is `Cos`.
isCos :: S -> Bool
isCos expr =
  case expr of
    Cos _ -> True
    _ -> False

-- | @isTan expr@ returns @True@ if @expr@ is `Tan`.
isTan :: S -> Bool
isTan expr =
  case expr of
    Tan _ -> True
    _ -> False

-- | @isAsin expr@ returns @True@ if @expr@ is `Asin`.
isAsin :: S -> Bool
isAsin expr =
  case expr of
    ASin _ -> True
    _ -> False

-- | @isAcos expr@ returns @True@ if @expr@ is `Acos`.
isAcos :: S -> Bool
isAcos expr =
  case expr of
    ACos _ -> True
    _ -> False

-- | @isAtan expr@ returns @True@ if @expr@ is `Atan`.
isAtan :: S -> Bool
isAtan expr =
  case expr of
    ATan _ -> True
    _ -> False

-- | @isSinh expr@ returns @True@ if @expr@ is `Sinh`.
isSinh :: S -> Bool
isSinh expr =
  case expr of
    Sinh _ -> True
    _ -> False

-- | @isCosh expr@ returns @True@ if @expr@ is `Cosh`.
isCosh :: S -> Bool
isCosh expr =
  case expr of
    Cosh _ -> True
    _ -> False

-- | @isTanh expr@ returns @True@ if @expr@ is `Tanh`.
isTanh :: S -> Bool
isTanh expr =
  case expr of
    Tanh _ -> True
    _ -> False

-- | @isAsinh expr@ returns @True@ if @expr@ is `Asinh`.
isAsinh :: S -> Bool
isAsinh expr =
  case expr of
    ASinh _ -> True
    _ -> False

-- | @isAcosh expr@ returns @True@ if @expr@ is `Acosh`.
isAcosh :: S -> Bool
isAcosh expr =
  case expr of
    ACosh _ -> True
    _ -> False

-- | @isAtanh expr@ returns @True@ if @expr@ is `Atanh`.
isAtanh :: S -> Bool
isAtanh expr =
  case expr of
    ATanh _ -> True
    _ -> False

-- | @isPow expr@ returns @True@ if @expr@ is `Pow`.
isPow :: S -> Bool
isPow expr =
  case expr of
    Pow _ _ -> True
    _ -> False

-- | @isExp expr@ returns @True@ if @expr@ is `Exp`.
isExp :: S -> Bool
isExp expr =
  case expr of
    Exp _ -> True
    _ -> False

-- | @isLog expr@ returns @True@ if @expr@ is `Log`.
isLog :: S -> Bool
isLog expr =
  case expr of
    Log _ _ -> True
    _ -> False

-- | @isLn expr@ returns @True@ if @expr@ is `Ln`.
isLn :: S -> Bool
isLn expr =
  case expr of
    Ln _ -> True
    _ -> False

-- | @isAtom expr@ returns @True@ if @expr@ is either `Literal` or `Symbol`.
isAtom :: S -> Bool
isAtom expr = isLiteral expr || isSymbol expr

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

getSymbolsInner :: S -> [S] -> [S]
getSymbolsInner expr acc =
  case expr of
    Literal _ -> acc
    Symbol _ -> if expr `elem` acc then acc else expr : acc
    Neg x -> getSymbolsInner x acc
    Abs x -> getSymbolsInner x acc
    Sign x -> getSymbolsInner x acc
    Add l r -> getSymbolsInner l $ getSymbolsInner r acc
    Sub l r -> getSymbolsInner l $ getSymbolsInner r acc
    Mul l r -> getSymbolsInner l $ getSymbolsInner r acc
    Div l r -> getSymbolsInner l $ getSymbolsInner r acc
    Sqrt x -> getSymbolsInner x acc
    Sin x -> getSymbolsInner x acc
    Cos x -> getSymbolsInner x acc
    Tan x -> getSymbolsInner x acc
    ASin x -> getSymbolsInner x acc
    ACos x -> getSymbolsInner x acc
    ATan x -> getSymbolsInner x acc
    Sinh x -> getSymbolsInner x acc
    Cosh x -> getSymbolsInner x acc
    Tanh x -> getSymbolsInner x acc
    ASinh x -> getSymbolsInner x acc
    ACosh x -> getSymbolsInner x acc
    ATanh x -> getSymbolsInner x acc
    Pow b e -> getSymbolsInner b $ getSymbolsInner e acc
    Exp x -> getSymbolsInner x acc
    Log b x -> getSymbolsInner b $ getSymbolsInner x acc
    Ln x -> getSymbolsInner x acc

-- | @getSymbols expr@ returns a list of all the unique `Symbol`s contained by
-- @expr@.
getSymbols :: S -> [S]
getSymbols expr = getSymbolsInner expr []

getAtomsInner :: S -> [S] -> [S]
getAtomsInner expr acc =
  case expr of
    Literal _ -> if expr `elem` acc then acc else expr : acc
    Symbol _ -> if expr `elem` acc then acc else expr : acc
    Neg x -> getAtomsInner x acc
    Abs x -> getAtomsInner x acc
    Sign x -> getAtomsInner x acc
    Add l r -> getAtomsInner l $ getAtomsInner r acc
    Sub l r -> getAtomsInner l $ getAtomsInner r acc
    Mul l r -> getAtomsInner l $ getAtomsInner r acc
    Div l r -> getAtomsInner l $ getAtomsInner r acc
    Sqrt x -> getAtomsInner x acc
    Sin x -> getAtomsInner x acc
    Cos x -> getAtomsInner x acc
    Tan x -> getAtomsInner x acc
    ASin x -> getAtomsInner x acc
    ACos x -> getAtomsInner x acc
    ATan x -> getAtomsInner x acc
    Sinh x -> getAtomsInner x acc
    Cosh x -> getAtomsInner x acc
    Tanh x -> getAtomsInner x acc
    ASinh x -> getAtomsInner x acc
    ACosh x -> getAtomsInner x acc
    ATanh x -> getAtomsInner x acc
    Pow b e -> getAtomsInner b $ getAtomsInner e acc
    Exp x -> getAtomsInner x acc
    Log b x -> getAtomsInner b $ getAtomsInner x acc
    Ln x -> getAtomsInner x acc

-- | @getAtoms expr@ returns a list of all the unique `Symbol`s or `Literal`s
-- contained by @expr@.
getAtoms :: S -> [S]
getAtoms expr = getAtomsInner expr []

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

-- | @subsSingle target replace expr@ replaces all instances of @target@ within
-- @expr@ with @replace@ and re-evaluate all sub-expressions.
--
-- @target@ must be matched verbatim in order for the substitution to be made.
subsSingle :: S -> S -> S -> S
subsSingle target replace expr =
  let doSubs = subsSingle target replace
   in
    if expr == target then
      replace
    else
      case expr of
        Literal f -> Literal f
        Symbol s -> Symbol s
        Neg x -> negate (doSubs x)
        Abs x -> abs (doSubs x)
        Sign x -> signum (doSubs x)
        Add l r -> (doSubs l) + (doSubs r)
        Sub l r -> (doSubs l) - (doSubs r)
        Mul l r -> (doSubs l) * (doSubs r)
        Div l r -> (doSubs l) / (doSubs r)
        Sqrt x -> sqrt (doSubs x)
        Sin x -> sin (doSubs x)
        Cos x -> cos (doSubs x)
        Tan x -> tan (doSubs x)
        ASin x -> asin (doSubs x)
        ACos x -> acos (doSubs x)
        ATan x -> atan (doSubs x)
        Sinh x -> sinh (doSubs x)
        Cosh x -> cosh (doSubs x)
        Tanh x -> tanh (doSubs x)
        ASinh x -> asinh (doSubs x)
        ACosh x -> acosh (doSubs x)
        ATanh x -> atanh (doSubs x)
        Pow b e -> (doSubs b) ** (doSubs e)
        Exp x -> exp (doSubs x)
        Log b x -> logBase (doSubs b) (doSubs x)
        Ln x -> log (doSubs x)

-- | Make a series of substitutions and re-evaluate all sub-expressions.
subs :: [(S, S)] -> S -> S
subs ss expr =
  case ss of
    [] -> expr
    (target, replace) : rest -> subs rest $ subsSingle target replace expr

-- | @subsfSingle target replace expr@ replaces all instances of @target@
-- within @expr@ with @replace@ and re-evaluate all sub-expressions.
--
-- @target@ must be matched verbatim in order for the subtitition to be made.
subsfSingle :: S -> Float -> S -> S
subsfSingle target replace expr =
  let doSubs = subsfSingle target replace
   in
    if expr == target then
      Literal replace
    else
      case expr of
        Literal f -> Literal f
        Symbol s -> Symbol s
        Neg x -> negate (doSubs x)
        Abs x -> abs (doSubs x)
        Sign x -> signum (doSubs x)
        Add l r -> (doSubs l) + (doSubs r)
        Sub l r -> (doSubs l) - (doSubs r)
        Mul l r -> (doSubs l) * (doSubs r)
        Div l r -> (doSubs l) / (doSubs r)
        Sqrt x -> sqrt (doSubs x)
        Sin x -> sin (doSubs x)
        Cos x -> cos (doSubs x)
        Tan x -> tan (doSubs x)
        ASin x -> asin (doSubs x)
        ACos x -> acos (doSubs x)
        ATan x -> atan (doSubs x)
        Sinh x -> sinh (doSubs x)
        Cosh x -> cosh (doSubs x)
        Tanh x -> tanh (doSubs x)
        ASinh x -> asinh (doSubs x)
        ACosh x -> acosh (doSubs x)
        ATanh x -> atanh (doSubs x)
        Pow b e -> (doSubs b) ** (doSubs e)
        Exp x -> exp (doSubs x)
        Log b x -> logBase (doSubs b) (doSubs x)
        Ln x -> log (doSubs x)

-- | Make a series of substititions and re-evaluate all sub-expressions.
subsf :: [(S, Float)] -> S -> S
subsf ss expr =
  case ss of
    [] -> expr
    (target, replace) : rest -> subsf rest $ subsfSingle target replace expr

-- | Returned by @evalf*@ functions.
data Error = EvalfError [S]

-- | Alias for @Either 'Error' a@.
type Result a = Either Error a

-- | @evalfSingle target replace expr@ replaces all instances of @target@
-- within @expr@ with @replace@ and attempt to re-evaluate to a single @Float@.
--
-- @Error syms@ is returned if there are any symbols remaining, where @syms@ is
-- a list of all such unique symbols.
evalfSingle :: S -> Float -> S -> Result Float
evalfSingle target replace expr =
  case subsfSingle target replace expr of
    Literal x -> Right x
    x -> Left $ EvalfError (getSymbols x)

-- | Make a series of substitutions and attempt to re-evaluate to a single
-- @Float@.
--
-- @Error syms@ is returned if there are any symbols remaining, where @syms@ is
-- a list of all such unique symbols.
evalf :: [(S, Float)] -> S -> Result Float
evalf ss expr =
  case subsf ss expr of
    Literal x -> Right x
    x -> Left $ EvalfError (getSymbols x)

-- | @toString expr@ is a string representation of @expr@.
toString :: S -> String
toString expr =
  let
    withParensIf x tests =
      if foldl (\acc f -> acc || f x) False tests then
        "(" ++ (toStringInner "" x) ++ ")"
      else
        toStringInner "" x
    displayFunction fnName x =
      fnName ++ "(" ++ (toStringInner "" x) ++ ")"
    toStringInner acc x =
      let
        xStr =
          case x of
            Literal f -> show f
            Symbol s -> s
            Neg x -> "-" ++ (withParensIf x [isAdd, isSub, isNeg])
            Abs x -> "|" ++ (toStringInner "" x) ++ "|"
            Sign x -> displayFunction "sign" x
            Add l (Neg r) ->
              (toStringInner "" l)
              ++ " - "
              ++ (withParensIf r [isAdd, isSub, isNeg])
            Add l r ->
              (toStringInner "" l)
              ++ " + "
              ++ (withParensIf r [isNeg])
            Sub l (Neg r) ->
              (toStringInner "" l)
              ++ " + "
              ++ (withParensIf r [isNeg])
            Sub l r ->
              (toStringInner "" l)
              ++ " - "
              ++ (withParensIf r [isAdd, isSub, isNeg])
            Mul l r ->
              (withParensIf l [isAdd, isSub])
              ++ " * "
              ++ (withParensIf r [isAdd, isSub])
            Div l r ->
              (withParensIf l [isAdd, isSub])
              ++ " / "
              ++ (withParensIf r [isAdd, isSub, isMul, isDiv])
            Sqrt x -> displayFunction "√" x
            Sin x -> displayFunction "sin" x
            Cos x -> displayFunction "cos" x
            Tan x -> displayFunction "tan" x
            ASin x -> displayFunction "arcsin" x
            ACos x -> displayFunction "arccos" x
            ATan x -> displayFunction "arctan" x
            Sinh x -> displayFunction "sinh" x
            Cosh x -> displayFunction "cosh" x
            Tanh x -> displayFunction "tanh" x
            ASinh x -> displayFunction "arsinh" x
            ACosh x -> displayFunction "arcosh" x
            ATanh x -> displayFunction "artanh" x
            Pow b e ->
              (withParensIf b [isAdd, isSub, isMul, isDiv, isNeg, isPow])
              ++ "^"
              ++ (withParensIf e [isAdd, isSub, isMul, isDiv, isNeg, isPow])
            Exp x -> displayFunction "exp" x
            Log b x ->
              "log"
              ++ (withParensIf b [isAdd, isSub, isMul, isDiv, isNeg, isPow])
              ++ "(" ++ (toStringInner "" x) ++ ")"
            Ln x -> displayFunction "ln" x
       in acc ++ xStr
   in toStringInner "" expr

instance Show S where
  -- | Convert to a string using `toString`.
  show = toString

-- | @toStringHaskell expr@ is a Haskell representation of @expr@.
toStringHaskell :: S -> String
toStringHaskell expr =
  let
    subexprStr x =
      case x of
        Literal f -> if signum f < 0.0 then "(" ++ (show f) ++ ")" else show f
        Symbol s -> s
        _ -> "(" ++ (toStringInner "" x) ++ ")"
    displayFunction fnName arg =
      fnName ++ " " ++ (subexprStr arg)
    displayFunction2 fnName arg1 arg2 =
      fnName ++ " " ++ (subexprStr arg1) ++ " " ++ (subexprStr arg2)
    toStringInner acc x =
      let
        xStr =
          case x of
            Literal f -> show f
            Symbol s -> s
            Neg x -> displayFunction "negate" x
            Abs x -> displayFunction "abs" x
            Sign x -> displayFunction "signum" x
            Add l (Neg r) -> (subexprStr l) ++ " - " ++ (subexprStr r)
            Add l r -> (subexprStr l) ++ " + " ++ (subexprStr r)
            Sub l (Neg r) -> (subexprStr l) ++ " + " ++ (subexprStr r)
            Sub l r -> (subexprStr l) ++ " - " ++ (subexprStr r)
            Mul l r -> (subexprStr l) ++ " * " ++ (subexprStr r)
            Div l r -> (subexprStr l) ++ " / " ++ (subexprStr r)
            Sqrt x -> displayFunction "sqrt" x
            Sin x -> displayFunction "sin" x
            Cos x -> displayFunction "cos" x
            Tan x -> displayFunction "tan" x
            ASin x -> displayFunction "asin" x
            ACos x -> displayFunction "acos" x
            ATan x -> displayFunction "atan" x
            Sinh x -> displayFunction "sinh" x
            Cosh x -> displayFunction "cosh" x
            Tanh x -> displayFunction "tanh" x
            ASinh x -> displayFunction "asinh" x
            ACosh x -> displayFunction "acosh" x
            ATanh x -> displayFunction "atanh" x
            Pow b e -> (subexprStr b) ++ " ** " ++ (subexprStr e)
            Exp x -> displayFunction "exp" x
            Log b x -> displayFunction2 "logBase" b x
            Ln x -> displayFunction "ln" x
       in acc ++ xStr
   in toStringInner "" expr

-- | @toStringRust expr@ is a Rust representation of @expr@.
toStringRust :: S -> String
toStringRust expr =
  let
    withParensIf x tests =
      if foldl (\acc f -> acc || f x) False tests then
        "(" ++ (toStringInner "" x) ++ ")"
      else
        toStringInner "" x
    displayFunction fnName x =
      (withParensIf x [isAdd, isSub, isMul, isDiv, isNeg])
      ++ "." ++ fnName ++ "()"
    toStringInner acc x =
      let
        xStr =
          case x of
            Literal f -> show f
            Symbol s -> s
            Neg x -> "-" ++ (withParensIf x [isAdd, isSub, isNeg])
            Abs x -> displayFunction "abs" x
            Sign x -> displayFunction "signum" x
            Add l (Neg r) ->
              (toStringInner "" l)
              ++ " - "
              ++ (withParensIf r [isAdd, isSub, isNeg])
            Add l r ->
              (toStringInner "" l)
              ++ " + "
              ++ (withParensIf r [isNeg])
            Sub l (Neg r) ->
              (toStringInner "" l)
              ++ " + "
              ++ (withParensIf r [isNeg])
            Sub l r ->
              (toStringInner "" l)
              ++ " - "
              ++ (withParensIf r [isAdd, isSub, isNeg])
            Mul l r ->
              (withParensIf l [isAdd, isSub])
              ++ " * "
              ++ (withParensIf r [isAdd, isSub])
            Div l r ->
              (withParensIf l [isAdd, isSub])
              ++ " / "
              ++ (withParensIf r [isAdd, isSub, isMul, isDiv])
            Sqrt x -> displayFunction "sqrt" x
            Sin x -> displayFunction "sin" x
            Cos x -> displayFunction "cos" x
            Tan x -> displayFunction "tan" x
            ASin x -> displayFunction "asin" x
            ACos x -> displayFunction "acos" x
            ATan x -> displayFunction "atan" x
            Sinh x -> displayFunction "sinh" x
            Cosh x -> displayFunction "cosh" x
            Tanh x -> displayFunction "tanh" x
            ASinh x -> displayFunction "asinh" x
            ACosh x -> displayFunction "acosh" x
            ATanh x -> displayFunction "atanh" x
            Pow b e ->
              (withParensIf b [isAdd, isSub, isMul, isDiv, isNeg])
              ++ ".powf(" ++ (toStringInner "" e) ++ ")"
            Exp x -> displayFunction "exp" x
            Log b x ->
              (withParensIf x [isAdd, isSub, isMul, isDiv, isNeg])
              ++ ".log(" ++ (toStringInner "" b) ++ ")"
            Ln x -> displayFunction "ln" x
       in acc ++ xStr
   in toStringInner "" expr

-- | @toStringPython expr@ is a Python representation of @expr@.
toStringPython :: S -> String
toStringPython expr =
  let
    withParensIf x tests =
      if foldl (\acc f -> acc || f x) False tests then
        "(" ++ (toStringInner "" x) ++ ")"
      else
        toStringInner "" x
    displayFunction fnName x =
      fnName ++ "(" ++ (toStringInner "" x) ++ ")"
    toStringInner acc x =
      let
        xStr =
          case x of
            Literal f -> show f
            Symbol s -> s
            Neg x -> "-" ++ (withParensIf x [isAdd, isSub, isNeg])
            Abs x -> displayFunction "abs" x
            Sign x -> "copysign(1.0, " ++ (toStringInner "" x) ++ ")"
            Add l (Neg r) ->
              (toStringInner "" l)
              ++ " - "
              ++ (withParensIf r [isAdd, isSub, isNeg])
            Add l r ->
              (toStringInner "" l)
              ++ " + "
              ++ (withParensIf r [isNeg])
            Sub l (Neg r) ->
              (toStringInner "" l)
              ++ " + "
              ++ (withParensIf r [isNeg])
            Sub l r ->
              (toStringInner "" l)
              ++ " - "
              ++ (withParensIf r [isAdd, isSub, isNeg])
            Mul l r ->
              (withParensIf l [isAdd, isSub])
              ++ " * "
              ++ (withParensIf r [isAdd, isSub])
            Div l r ->
              (withParensIf l [isAdd, isSub])
              ++ " / "
              ++ (withParensIf r [isAdd, isSub, isMul, isDiv])
            Sqrt x -> displayFunction "sqrt" x
            Sin x -> displayFunction "sin" x
            Cos x -> displayFunction "cos" x
            Tan x -> displayFunction "tan" x
            ASin x -> displayFunction "asin" x
            ACos x -> displayFunction "acos" x
            ATan x -> displayFunction "atan" x
            Sinh x -> displayFunction "sinh" x
            Cosh x -> displayFunction "cosh" x
            Tanh x -> displayFunction "tanh" x
            ASinh x -> displayFunction "asinh" x
            ACosh x -> displayFunction "acosh" x
            ATanh x -> displayFunction "atanh" x
            Pow b e ->
              (withParensIf b [isAdd, isSub, isMul, isDiv, isNeg])
              ++ " ** "
              ++ (withParensIf e [isAdd, isSub, isMul, isDiv, isNeg])
            Exp x -> displayFunction "exp" x
            Log b x ->
              "log("
              ++ (toStringInner "" x)
              ++ ", "
              ++ (toStringInner "" b)
              ++ ")"
            Ln x -> displayFunction "log" x
       in acc ++ xStr
   in toStringInner "" expr

-- | @toStringNumpy expr@ is a NumPy representation of @expr@.
toStringNumpy :: S -> String
toStringNumpy expr =
  let
    withParensIf x tests =
      if foldl (\acc f -> acc || f x) False tests then
        "(" ++ (toStringInner "" x) ++ ")"
      else
        toStringInner "" x
    displayFunction fnName x =
      "np." ++ fnName ++ "(" ++ (toStringInner "" x) ++ ")"
    toStringInner acc x =
      let
        xStr =
          case x of
            Literal f -> show f
            Symbol s -> s
            Neg x -> "-" ++ (withParensIf x [isAdd, isSub, isNeg])
            Abs x -> displayFunction "abs" x
            Sign x -> displayFunction "sign" x
            Add l (Neg r) ->
              (toStringInner "" l)
              ++ " - "
              ++ (withParensIf r [isAdd, isSub, isNeg])
            Add l r ->
              (toStringInner "" l)
              ++ " + "
              ++ (withParensIf r [isNeg])
            Sub l (Neg r) ->
              (toStringInner "" l)
              ++ " + "
              ++ (withParensIf r [isNeg])
            Sub l r ->
              (toStringInner "" l)
              ++ " - "
              ++ (withParensIf r [isAdd, isSub, isNeg])
            Mul l r ->
              (withParensIf l [isAdd, isSub])
              ++ " * "
              ++ (withParensIf r [isAdd, isSub])
            Div l r ->
              (withParensIf l [isAdd, isSub])
              ++ " / "
              ++ (withParensIf r [isAdd, isSub, isMul, isDiv])
            Sqrt x -> displayFunction "sqrt" x
            Sin x -> displayFunction "sin" x
            Cos x -> displayFunction "cos" x
            Tan x -> displayFunction "tan" x
            ASin x -> displayFunction "arcsin" x
            ACos x -> displayFunction "arccos" x
            ATan x -> displayFunction "arctan" x
            Sinh x -> displayFunction "sinh" x
            Cosh x -> displayFunction "cosh" x
            Tanh x -> displayFunction "tanh" x
            ASinh x -> displayFunction "arcsinh" x
            ACosh x -> displayFunction "arccosh" x
            ATanh x -> displayFunction "arctanh" x
            Pow b e ->
              (withParensIf b [isAdd, isSub, isMul, isDiv, isNeg, isLog])
              ++ " ** "
              ++ (withParensIf e [isAdd, isSub, isMul, isDiv, isNeg, isLog])
            Exp x -> displayFunction "exp" x
            Log b x ->
              "np.log("
              ++ (toStringInner "" x)
              ++ ") / np.log( "
              ++ (toStringInner "" b)
              ++ ")"
            Ln x -> displayFunction "log" x
       in acc ++ xStr
   in toStringInner "" expr

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
