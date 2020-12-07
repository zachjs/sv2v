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
    ) where

import Data.Bits (shiftL, shiftR)

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

simplifyStep (e @ (Concat [Pattern{}])) = e
simplifyStep (Concat [e]) = e
simplifyStep (Concat es) = Concat $ filter (/= Concat []) es
simplifyStep (Repeat (Dec 0) _) = Concat []
simplifyStep (Repeat (Dec 1) es) = Concat es
simplifyStep (Mux (Number n) e1 e2) =
    case numberToInteger n of
        Just 0 -> e2
        Just _ -> e1
        Nothing -> Mux (Number n) e1 e2

simplifyStep (Call (Ident "$clog2") (Args [Dec k] [])) =
    toDec $ clog2 k
    where
        clog2Help :: Integer -> Integer -> Integer
        clog2Help p n = if p >= n then 0 else 1 + clog2Help (p*2) n
        clog2 :: Integer -> Integer
        clog2 n = if n < 2 then 0 else clog2Help 1 n

simplifyStep (BinOp op e1 e2) = simplifyBinOp op e1 e2
simplifyStep other = other


simplifyBinOp :: BinOp -> Expr -> Expr -> Expr

simplifyBinOp Add (Dec 0) e = e
simplifyBinOp Add e (Dec 0) = e
simplifyBinOp Sub e (Dec 0) = e
simplifyBinOp Sub (Dec 0) e = UniOp UniSub e
simplifyBinOp Mul (Dec 0) _ = toDec 0
simplifyBinOp Mul (Dec 1) e = e
simplifyBinOp Mul _ (Dec 0) = toDec 0
simplifyBinOp Mul e (Dec 1) = e

simplifyBinOp Add e1 (UniOp UniSub e2) = BinOp Sub e1 e2
simplifyBinOp Add (UniOp UniSub e1) e2 = BinOp Sub e2 e1
simplifyBinOp Sub e1 (UniOp UniSub e2) = BinOp Add e1 e2
simplifyBinOp Sub (UniOp UniSub e1) e2 = UniOp UniSub $ BinOp Add e1 e2

simplifyBinOp Add (BinOp Add e (n1 @ Number{})) (n2 @ Number{}) =
    BinOp Add e (BinOp Add n1 n2)
simplifyBinOp Sub (n1 @ Number{}) (BinOp Sub (n2 @ Number{}) e) =
    BinOp Add (BinOp Sub n1 n2) e
simplifyBinOp Sub (n1 @ Number{}) (BinOp Sub e (n2 @ Number{})) =
    BinOp Sub (BinOp Add n1 n2) e
simplifyBinOp Sub (BinOp Add e (n1 @ Number{})) (n2 @ Number{}) =
    BinOp Add e (BinOp Sub n1 n2)
simplifyBinOp Add (n1 @ Number{}) (BinOp Add (n2 @ Number{}) e) =
    BinOp Add (BinOp Add n1 n2) e
simplifyBinOp Add (n1 @ Number{}) (BinOp Sub e (n2 @ Number{})) =
    BinOp Add e (BinOp Sub n1 n2)
simplifyBinOp Sub (BinOp Sub e (n1 @ Number{})) (n2 @ Number{}) =
    BinOp Sub e (BinOp Add n1 n2)
simplifyBinOp Add (BinOp Sub e (n1 @ Number{})) (n2 @ Number{}) =
    BinOp Sub e (BinOp Sub n1 n2)
simplifyBinOp Add (BinOp Sub (n1 @ Number{}) e) (n2 @ Number{}) =
    BinOp Sub (BinOp Add n1 n2) e
simplifyBinOp Ge (BinOp Sub e (Dec 1)) (Dec 0) = BinOp Ge e (toDec 1)

simplifyBinOp ShiftAL (Dec x) (Dec y) = toDec $ shiftL x (fromIntegral y)
simplifyBinOp ShiftAR (Dec x) (Dec y) = toDec $ shiftR x (fromIntegral y)
simplifyBinOp ShiftL  (Dec x) (Dec y) = toDec $ shiftL x (fromIntegral y)
simplifyBinOp ShiftR  (Dec x) (Dec y) = toDec $ shiftR x (fromIntegral y)

simplifyBinOp op e1 e2 =
    case (e1, e2) of
        (Dec    x, Dec    y) -> constantFold orig op   x    y
        (SizDec x, Dec    y) -> constantFold orig op   x    y
        (Dec    x, SizDec y) -> constantFold orig op   x    y
        (Bas    x, Dec    y) -> constantFold orig op   x    y
        (Dec    x, Bas    y) -> constantFold orig op   x    y
        (Bas    x, Bas    y) -> constantFold orig op   x    y
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
pattern Bas n <- Number (Based _ _ _ n 0)


-- returns the size of a range
rangeSize :: Range -> Expr
rangeSize (s, e) =
    endianCondExpr (s, e) a b
    where
        a = rangeSizeHiLo (s, e)
        b = rangeSizeHiLo (e, s)

-- returns the size of a range known to be ordered
rangeSizeHiLo :: Range -> Expr
rangeSizeHiLo (hi, lo) =
    simplify $ BinOp Add (BinOp Sub hi lo) (RawNum 1)

-- chooses one or the other expression based on the endianness of the given
-- range; [hi:lo] chooses the first expression
endianCondExpr :: Range -> Expr -> Expr -> Expr
endianCondExpr SizedRange{} e _ = e
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
dimensionsSize ranges =
    simplify $
    foldl (BinOp Mul) (RawNum 1) $
    map rangeSize $
    ranges

-- "sized ranges" are of the form [E-1:0], where E is any expression; in most
-- designs, we can safely assume that E >= 1, allowing for more succinct output
pattern SizedRange :: Expr -> Range
pattern SizedRange expr = (BinOp Sub expr (RawNum 1), RawNum 0)
