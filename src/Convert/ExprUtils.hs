{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Utilities for expressions and ranges
 -}

module Convert.ExprUtils
    ( simplify
    , simplifyStep
    , rangeSize
    , rangeSizeHiLo
    , endianCondExpr
    , endianCondRange
    , dimensionsSize
    , stringToNumber
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (ord)

import Convert.Traverse
import Language.SystemVerilog.AST

simplify :: Expr -> Expr
simplify = simplifyStep . traverseSinglyNestedExprs simplify . simplifyStep

simplifyStep :: Expr -> Expr

simplifyStep (UniOp LogNot (Number n)) =
    case numberToInteger n of
        Just 0 -> bool True
        Just _ -> bool False
        Nothing -> UniOp LogNot $ Number n
simplifyStep (UniOp LogNot (BinOp Eq a b)) = BinOp Ne a b
simplifyStep (UniOp LogNot (BinOp Ne a b)) = BinOp Eq a b

simplifyStep (UniOp UniSub (UniOp UniSub e)) = e
simplifyStep (UniOp UniSub (BinOp Sub e1 e2)) = BinOp Sub e2 e1

simplifyStep (Concat [Number (Decimal size _ value)]) =
    Number $ Decimal size False value
simplifyStep (Concat [Number (Based size _ base value kinds)]) =
    Number $ Based size False base value kinds
simplifyStep (Concat [e@Stream{}]) = e
simplifyStep (Concat [e@Repeat{}]) = e
simplifyStep (Concat es) = Concat $ flattenConcat es
simplifyStep (Repeat (Dec 0) _) = Concat []
simplifyStep (Repeat (Dec 1) es) = Concat es
simplifyStep (Mux (Number n) e1 e2) =
    case numberToInteger n of
        Just 0 -> e2
        Just _ -> e1
        Nothing -> Mux (Number n) e1 e2

simplifyStep (Call (Ident "$clog2") (Args [SizDec k] [])) =
    simplifyStep $ Call (Ident "$clog2") (Args [RawNum k] [])
simplifyStep (Call (Ident "$clog2") (Args [Dec k] [])) =
    toDec $ clog2 k
    where
        clog2Help :: Integer -> Integer -> Integer
        clog2Help p n = if p >= n then 0 else 1 + clog2Help (p*2) n
        clog2 :: Integer -> Integer
        clog2 n = if n < 2 then 0 else clog2Help 1 n

-- TODO: add full constant evaluation for all number literals to avoid the
-- anti-loop hack below
simplifyStep e@(BinOp _ (BinOp _ Number{} Number{}) Number{}) = e

simplifyStep (BinOp op e1 e2) = simplifyBinOp op e1 e2
simplifyStep other = other

-- flatten and coalesce concatenations
flattenConcat :: [Expr] -> [Expr]
flattenConcat (Number n1 : Number n2 : es) =
    flattenConcat $ Number (n1 <> n2) : es
flattenConcat (Concat es1 : es2) =
    flattenConcat $ es1 ++ es2
flattenConcat (e : es) =
    e : flattenConcat es
flattenConcat [] = []


simplifyBinOp :: BinOp -> Expr -> Expr -> Expr

simplifyBinOp Sub e (Dec 0) = e
simplifyBinOp Sub (Dec 0) e = UniOp UniSub e
simplifyBinOp Mul (Dec 0) _ = toDec 0
simplifyBinOp Mul _ (Dec 0) = toDec 0
simplifyBinOp Mod _ (Dec 1) = toDec 0

simplifyBinOp Add e1 (UniOp UniSub e2) = BinOp Sub e1 e2
simplifyBinOp Add (UniOp UniSub e1) e2 = BinOp Sub e2 e1
simplifyBinOp Sub e1 (UniOp UniSub e2) = BinOp Add e1 e2
simplifyBinOp Sub (UniOp UniSub e1) e2 = UniOp UniSub $ BinOp Add e1 e2

simplifyBinOp Add (BinOp Add e n1@Number{}) n2@Number{} =
    BinOp Add e (BinOp Add n1 n2)
simplifyBinOp Sub n1@Number{} (BinOp Sub n2@Number{} e) =
    BinOp Add (BinOp Sub n1 n2) e
simplifyBinOp Sub n1@Number{} (BinOp Sub e n2@Number{}) =
    BinOp Sub (BinOp Add n1 n2) e
simplifyBinOp Sub (BinOp Add e n1@Number{}) n2@Number{} =
    BinOp Add e (BinOp Sub n1 n2)
simplifyBinOp Add n1@Number{} (BinOp Add n2@Number{} e) =
    BinOp Add (BinOp Add n1 n2) e
simplifyBinOp Add n1@Number{} (BinOp Sub e n2@Number{}) =
    BinOp Add e (BinOp Sub n1 n2)
simplifyBinOp Sub (BinOp Sub e n1@Number{}) n2@Number{} =
    BinOp Sub e (BinOp Add n1 n2)
simplifyBinOp Add (BinOp Sub e n1@Number{}) n2@Number{} =
    BinOp Sub e (BinOp Sub n1 n2)
simplifyBinOp Add (BinOp Sub n1@Number{} e) n2@Number{} =
    BinOp Sub (BinOp Add n1 n2) e
simplifyBinOp Ge (BinOp Sub e (Dec 1)) (Dec 0) = BinOp Ge e (toDec 1)

-- simplify bit shifts of decimal literals
simplifyBinOp op (Dec x) (Number yRaw)
    | ShiftAL <- op = decShift shiftL
    | ShiftAR <- op = decShift shiftR
    | ShiftL  <- op = decShift shiftL
    | ShiftR  <- op = decShift shiftR
    where
        decShift shifter =
            case numberToInteger yRaw of
                Just y -> toDec $ shifter x (fromIntegral y)
                Nothing -> constantFold undefined Div undefined 0

-- simply comparisons with string literals
simplifyBinOp op (Number n) (String s) | isCmpOp op =
    simplifyBinOp op (Number n) (sizeStringAs s n)
simplifyBinOp op (String s) (Number n) | isCmpOp op =
    simplifyBinOp op (sizeStringAs s n) (Number n)
simplifyBinOp op (String s1) (String s2) | isCmpOp op =
    simplifyBinOp op (stringToNumber s1) (stringToNumber s2)

-- simply basic arithmetic comparisons
simplifyBinOp op (Number n1) (Number n2)
    | Eq <- op = cmp (==)
    | Ne <- op = cmp (/=)
    | Lt <- op = cmp (<)
    | Le <- op = cmp (<=)
    | Gt <- op = cmp (>)
    | Ge <- op = cmp (>=)
    where
        cmp :: (Integer -> Integer -> Bool) -> Expr
        cmp folder =
            case (numberToInteger n1', numberToInteger n2') of
                (Just i1, Just i2) -> bool $ folder i1 i2
                _ -> BinOp op (Number n1') (Number n2')
        sg = numberIsSigned n1 && numberIsSigned n2
        sz = fromIntegral $ max (numberBitLength n1) (numberBitLength n2)
        n1' = numberCast sg sz n1
        n2' = numberCast sg sz n2

-- simply comparisons with unbased unsized literals
simplifyBinOp op (Number n) (ConvertedUU sz v k) | isCmpOp op =
    simplifyBinOp op (Number n) (uuExtend sz v k)
simplifyBinOp op (ConvertedUU sz v k) (Number n) | isCmpOp op =
    simplifyBinOp op (uuExtend sz v k) (Number n)

simplifyBinOp op e1 e2 =
    case (e1, e2) of
        (Dec    x, Dec    y) -> constantFold orig op   x    y
        (SizDec x, Dec    y) -> constantFold orig op   x    y
        (Dec    x, SizDec y) -> constantFold orig op   x    y
        (Bas    x, Dec    y) -> constantFold orig op   x    y
        (Dec    x, Bas    y) -> constantFold orig op   x    y
        (NegDec x, Dec    y) -> constantFold orig op (-x)   y
        (Dec    x, NegDec y) -> constantFold orig op   x  (-y)
        (NegDec x, NegDec y) -> constantFold orig op (-x) (-y)
        _ -> orig
    where orig = BinOp op e1 e2


-- attempt to constant fold a binary operation on integers
constantFold :: Expr -> BinOp -> Integer -> Integer -> Expr
constantFold _ Add x y = toDec (x + y)
constantFold _ Sub x y = toDec (x - y)
constantFold _ Mul x y = toDec (x * y)
constantFold _ Div _ 0 = Number $ Based (-32) True Hex 0 bits
    where bits = 2 ^ (32 :: Integer) - 1
constantFold _ Div x y = toDec (x `quot` y)
constantFold _ Mod x y = toDec (x `rem` y)
constantFold _ Pow x y = toDec (x ^ y)
constantFold _ Eq  x y = bool $ x == y
constantFold _ Ne  x y = bool $ x /= y
constantFold _ Gt  x y = bool $ x >  y
constantFold _ Ge  x y = bool $ x >= y
constantFold _ Lt  x y = bool $ x <  y
constantFold _ Le  x y = bool $ x <= y
constantFold _ BitAnd x y = toDec $ x .&. y
constantFold _ BitOr  x y = toDec $ x .|. y
constantFold fallback _ _ _ = fallback


bool :: Bool -> Expr
bool True = Number $ Decimal 1 False 1
bool False = Number $ Decimal 1 False 0

toDec :: Integer -> Expr
toDec n =
    if n < 0 then
        UniOp UniSub $ toDec (-n)
    else if n >= 4294967296 `div` 2 then
        let size = fromIntegral $ bits $ n * 2
        in Number $ Decimal size True n
    else
        RawNum n
    where
        bits :: Integer -> Integer
        bits 0 = 0
        bits v = 1 + bits (quot v 2)

pattern Dec :: Integer -> Expr
pattern Dec n <- Number (Decimal (-32) _ n)

pattern SizDec :: Integer -> Expr
pattern SizDec n <- Number (Decimal 32 _ n)

pattern NegDec :: Integer -> Expr
pattern NegDec n <- UniOp UniSub (Dec n)

pattern Bas :: Integer -> Expr
pattern Bas n <- Number (Based _ False _ n 0)


-- returns the size of a range
rangeSize :: Range -> Expr
rangeSize (s, e) =
    endianCondExpr (s, e) a b
    where
        a = rangeSizeHiLo (s, e)
        b = rangeSizeHiLo (e, s)

-- returns the size of a range known to be ordered
rangeSizeHiLo :: Range -> Expr
rangeSizeHiLo (SizedRange size) = size
rangeSizeHiLo (hi, lo) =
    simplify $ BinOp Add (BinOp Sub hi lo) (RawNum 1)

-- chooses one or the other expression based on the endianness of the given
-- range; [hi:lo] chooses the first expression
endianCondExpr :: Range -> Expr -> Expr -> Expr
endianCondExpr SizedRange{} e _ = e
endianCondExpr RevSzRange{} _ e = e
endianCondExpr r e1 e2 = simplify $ Mux (uncurry (BinOp Ge) r) e1 e2

-- chooses one or the other range based on the endianness of the given range,
-- but in such a way that the result is itself also usable as a range even if
-- the endianness cannot be resolved during conversion, i.e. if it's dependent
-- on a parameter value; [hi:lo] chooses the first range
endianCondRange :: Range -> Range -> Range -> Range
endianCondRange r r1 r2 =
    ( endianCondExpr r (fst r1) (fst r2)
    , endianCondExpr r (snd r1) (snd r2)
    )

-- returns the total size of a set of dimensions
dimensionsSize :: [Range] -> Expr
dimensionsSize [] = RawNum 1
dimensionsSize ranges =
    simplify $
    foldl1 (BinOp Mul) $
    map rangeSize $
    ranges

-- "sized ranges" are of the form [E-1:0], where E is any expression; in most
-- designs, we can safely assume that E >= 1, allowing for more succinct output
pattern SizedRange :: Expr -> Range
pattern SizedRange expr = (BinOp Sub expr (RawNum 1), RawNum 0)

-- similar to the above pattern, we assume E >= 1 for any range like [0:E-1]
pattern RevSzRange :: Expr -> Range
pattern RevSzRange expr = (RawNum 0, BinOp Sub expr (RawNum 1))

-- convert a string to decimal number
stringToNumber :: String -> Expr
stringToNumber str =
    Number $ Decimal size False value
    where
        size = 8 * length str
        value = stringToInteger str

-- convert a string to big integer
stringToInteger :: String -> Integer
stringToInteger [] = 0
stringToInteger (x : xs) =
    fromIntegral (ord x) + (256 :: Integer) * stringToInteger xs

-- cast string to number at least as big as the width of the given number
sizeStringAs :: String -> Number -> Expr
sizeStringAs str num =
    Cast (Left typ) (stringToNumber str)
    where
        typ = IntegerVector TReg Unspecified [(RawNum size, RawNum 1)]
        size = max strSize numSize
        strSize = fromIntegral $ 8 * length str
        numSize = numberBitLength num

-- excludes wildcard and strict comparison operators
isCmpOp :: BinOp -> Bool
isCmpOp Eq = True
isCmpOp Ne = True
isCmpOp Lt = True
isCmpOp Le = True
isCmpOp Gt = True
isCmpOp Ge = True
isCmpOp _ = False

-- sign extend a converted unbased unsized literal into a based number
uuExtend :: Integer -> Integer -> Integer -> Expr
uuExtend sz v k =
    Number $
    numberCast False (fromIntegral sz) $
    Based 1 True Hex v k

pattern ConvertedUU :: Integer -> Integer -> Integer -> Expr
pattern ConvertedUU sz v k <- Repeat
    (RawNum sz)
    [Number (Based 1 True Binary v k)]
