{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog expressions
 -}

module Language.SystemVerilog.AST.Expr
    ( Expr (..)
    , pattern Ident
    , pattern PSIdent
    , Range
    , TypeOrExpr
    , ExprOrRange
    , Args (..)
    , PartSelectMode (..)
    , DimsFn (..)
    , DimFn (..)
    , showAssignment
    , showRanges
    , showExprOrRange
    , simplify
    , rangeSize
    , rangeSizeHiLo
    , endianCondExpr
    , endianCondRange
    , dimensionsSize
    , readNumber
    , ParamBinding
    , showParams
    ) where

import Data.Bits (shiftL, shiftR)
import Data.List (intercalate)
import Numeric (readHex)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Language.SystemVerilog.AST.Op
import Language.SystemVerilog.AST.ShowHelp
import {-# SOURCE #-} Language.SystemVerilog.AST.Type

type Range = (Expr, Expr)

type TypeOrExpr = Either Type Expr
type ExprOrRange = Either Expr Range

data Expr
    = String  String
    | Number  String
    | Time    String
    | CSIdent Identifier [ParamBinding] Identifier
    | Range   Expr PartSelectMode Range
    | Bit     Expr Expr
    | Repeat  Expr [Expr]
    | Concat  [Expr]
    | Stream  StreamOp Expr [Expr]
    | Call    Expr Args
    | UniOp   UniOp Expr
    | BinOp   BinOp Expr Expr
    | Mux     Expr Expr Expr
    | Cast    TypeOrExpr Expr
    | DimsFn  DimsFn TypeOrExpr
    | DimFn   DimFn  TypeOrExpr Expr
    | Dot     Expr Identifier
    | Pattern [(Identifier, Expr)]
    | Inside Expr [ExprOrRange]
    | MinTypMax Expr Expr Expr
    | Nil
    deriving (Eq, Ord)

pattern Ident :: Identifier -> Expr
pattern Ident x = PSIdent "" x

pattern PSIdent :: Identifier -> Identifier -> Expr
pattern PSIdent x y = CSIdent x [] y

instance Show Expr where
    show (Nil          ) = ""
    show (Number  str  ) = str
    show (Time    str  ) = str
    show (Ident   str  ) = str
    show (PSIdent x y  ) = printf "%s::%s" x y
    show (CSIdent x p y) = printf "%s#%s::%s" x (showParams p) y
    show (String  str  ) = printf "\"%s\"" str
    show (Bit     e b  ) = printf "%s[%s]"     (show e) (show b)
    show (Range   e m r) = printf "%s[%s%s%s]" (show e) (show $ fst r) (show m) (show $ snd r)
    show (Repeat  e l  ) = printf "{%s {%s}}"  (show e) (commas $ map show l)
    show (Concat  l    ) = printf "{%s}"                (commas $ map show l)
    show (Stream  o e l) = printf "{%s %s%s}"  (show o) (show e) (show $ Concat l)
    show (Cast tore e  ) = printf "%s'(%s)"    (showEither tore) (show e)
    show (DimsFn  f v  ) = printf "%s(%s)"     (show f) (showEither v)
    show (DimFn   f v e) = printf "%s(%s, %s)" (show f) (showEither v) (show e)
    show (Inside  e l  ) = printf "(%s inside { %s })"  (show e) (intercalate ", " strs)
        where
            strs = map showExprOrRange l
    show (Pattern l    ) =
        printf "'{\n%s\n}" (indent $ intercalate ",\n" $ map showPatternItem l)
        where
            showPatternItem :: (Identifier, Expr) -> String
            showPatternItem (""     , e) = show e
            showPatternItem (':' : n, e) = showPatternItem (n, e)
            showPatternItem (n      , e) = printf "%s: %s" n (show e)
    show (MinTypMax a b c) = printf "(%s : %s : %s)" (show a) (show b) (show c)
    show (e @ UniOp{}) = showsPrec 0 e ""
    show (e @ BinOp{}) = showsPrec 0 e ""
    show (e @ Dot  {}) = showsPrec 0 e ""
    show (e @ Mux  {}) = showsPrec 0 e ""
    show (e @ Call {}) = showsPrec 0 e ""

    showsPrec _ (UniOp   o e  ) =
        shows o .
        showUniOpPrec e
    showsPrec _ (BinOp   o a b) =
        showBinOpPrec a .
        showChar ' ' .
        shows o .
        showChar ' ' .
        showBinOpPrec b
    showsPrec _ (Dot     e n  ) =
        shows e .
        showChar '.' .
        showString n
    showsPrec _ (Mux     c a b) =
        showChar '(' .
        shows c .
        showString " ? " .
        shows a .
        showString " : " .
        shows b .
        showChar ')'
    showsPrec _ (Call    e l  ) =
        shows e .
        shows l
    showsPrec _ e = \s -> show e ++ s

data Args
    = Args [Expr] [(Identifier, Expr)]
    deriving (Eq, Ord)

instance Show Args where
    show (Args pnArgs kwArgs) = "(" ++ (commas strs) ++ ")"
        where
            strs = (map show pnArgs) ++ (map showKwArg kwArgs)
            showKwArg (x, e) = printf ".%s(%s)" x (show e)

data PartSelectMode
    = NonIndexed
    | IndexedPlus
    | IndexedMinus
    deriving (Eq, Ord)

instance Show PartSelectMode where
    show NonIndexed   = ":"
    show IndexedPlus  = "+:"
    show IndexedMinus = "-:"

data DimsFn
    = FnBits
    | FnDimensions
    | FnUnpackedDimensions
    deriving (Eq, Ord)

data DimFn
    = FnLeft
    | FnRight
    | FnLow
    | FnHigh
    | FnIncrement
    | FnSize
    deriving (Eq, Ord)

instance Show DimsFn where
    show FnBits               = "$bits"
    show FnDimensions         = "$dimensions"
    show FnUnpackedDimensions = "$unpacked_dimensions"

instance Show DimFn where
    show FnLeft               = "$left"
    show FnRight              = "$right"
    show FnLow                = "$low"
    show FnHigh               = "$high"
    show FnIncrement          = "$increment"
    show FnSize               = "$size"


showAssignment :: Expr -> String
showAssignment Nil = ""
showAssignment val = " = " ++ show val

showRanges :: [Range] -> String
showRanges [] = ""
showRanges l = " " ++ (concatMap showRange l)

showRange :: Range -> String
showRange (h, l) = printf "[%s:%s]" (show h) (show l)

showExprOrRange :: ExprOrRange -> String
showExprOrRange (Left  x) = show x
showExprOrRange (Right x) = show x

clog2Help :: Integer -> Integer -> Integer
clog2Help p n = if p >= n then 0 else 1 + clog2Help (p*2) n
clog2 :: Integer -> Integer
clog2 n = if n < 2 then 0 else clog2Help 1 n

readNumber :: String -> Maybe Integer
readNumber ('3' : '2' : '\'' : 'd' : rest) = readMaybe rest
readNumber (            '\'' : 'd' : rest) = readMaybe rest
readNumber ('3' : '2' : '\'' : 'h' : rest) =
    case readHex rest of
        [(v, _)] -> Just v
        _ -> Nothing
readNumber ('\'' : 'h' : rest) =
    case readHex rest of
        [(v, _)] -> Just v
        _ -> Nothing
readNumber n = readMaybe n

showUniOpPrec :: Expr -> ShowS
showUniOpPrec (e @ UniOp{}) = (showParen True . shows) e
showUniOpPrec (e @ BinOp{}) = (showParen True . shows) e
showUniOpPrec e = shows e

showBinOpPrec :: Expr -> ShowS
showBinOpPrec (e @ BinOp{}) = (showParen True . shows) e
showBinOpPrec e = shows e

-- basic expression simplfication utility to help us generate nicer code in the
-- common case of ranges like `[FOO-1:0]`
simplify :: Expr -> Expr
simplify (UniOp LogNot (Number "1")) = Number "0"
simplify (UniOp LogNot (Number "0")) = Number "1"
simplify (UniOp LogNot (BinOp Eq a b)) = BinOp Ne a b
simplify (orig @ (Repeat (Number n) exprs)) =
    case readNumber n of
        Nothing -> orig
        Just 0 -> Concat []
        Just 1 -> Concat exprs
        Just x ->
            if x < 0
                then error $ "negative repeat count: " ++ show orig
                else orig
simplify (Concat [expr]) = expr
simplify (Concat exprs) =
    Concat $ filter (/= Concat []) exprs
simplify (orig @ (Call (Ident "$clog2") (Args [Number n] []))) =
    case readNumber n of
        Nothing -> orig
        Just x -> toLiteral $ clog2 x
simplify (Mux cc e1 e2) =
    case cc' of
        Number "1" -> e1'
        Number "0" -> e2'
        _ -> Mux cc' e1' e2'
    where
        cc' = simplify cc
        e1' = simplify e1
        e2' = simplify e2
simplify (Range e NonIndexed r) = Range e NonIndexed r
simplify (Range e _ (i, Number "0")) = Bit e i
simplify (BinOp Sub (Number n1) (BinOp Sub (Number n2) e)) =
    simplify $ BinOp Add (BinOp Sub (Number n1) (Number n2)) e
simplify (BinOp Sub (Number n1) (BinOp Sub e (Number n2))) =
    simplify $ BinOp Sub (BinOp Add (Number n1) (Number n2)) e
simplify (BinOp Sub (BinOp Add e (Number n1)) (Number n2)) =
    simplify $ BinOp Add e (BinOp Sub (Number n1) (Number n2))
simplify (BinOp Add (Number n1) (BinOp Add (Number n2) e)) =
    simplify $ BinOp Add (BinOp Add (Number n1) (Number n2)) e
simplify (BinOp Ge (BinOp Sub e (Number "1")) (Number "0")) =
    simplify $ BinOp Ge e (Number "1")
simplify (BinOp Add e1 (UniOp UniSub e2)) =
    simplify $ BinOp Sub e1 e2
simplify (BinOp Add (UniOp UniSub e2) e1) =
    simplify $ BinOp Sub e1 e2
simplify (BinOp Add (BinOp Sub (Number n1) e) (Number n2)) =
    case (readNumber n1, readNumber n2) of
        (Just x, Just y) ->
            simplify $ BinOp Sub (toLiteral (x + y)) e'
        _ -> nochange
    where
        e' = simplify e
        nochange = BinOp Add (BinOp Sub (Number n1) e') (Number n2)
simplify (BinOp op e1 e2) =
    case (op, e1', e2') of
        (Add, Number "0", e) -> e
        (Add, e, Number "0") -> e
        (Mul, _, Number "0") -> Number "0"
        (Mul, Number "0", _) -> Number "0"
        (Mul, e, Number "1") -> e
        (Mul, Number "1", e) -> e
        (Sub, e, Number "0") -> e
        (Add, BinOp Sub e (Number "1"), Number "1") -> e
        (Add, e, BinOp Sub (Number "0") (Number "1")) -> BinOp Sub e (Number "1")
        (_  , Number a, Number b) ->
            case (op, readNumber a, readNumber b) of
                (Add, Just x, Just y) -> toLiteral (x + y)
                (Sub, Just x, Just y) -> toLiteral (x - y)
                (Mul, Just x, Just y) -> toLiteral (x * y)
                (Div, Just _, Just 0) -> Number "x"
                (Div, Just x, Just y) -> toLiteral (x `quot` y)
                (Mod, Just x, Just y) -> toLiteral (x `rem` y)
                (Pow, Just x, Just y) -> toLiteral (x ^ y)
                (Eq , Just x, Just y) -> bool $ x == y
                (Ne , Just x, Just y) -> bool $ x /= y
                (Gt , Just x, Just y) -> bool $ x >  y
                (Ge , Just x, Just y) -> bool $ x >= y
                (Lt , Just x, Just y) -> bool $ x <  y
                (Le , Just x, Just y) -> bool $ x <= y
                (ShiftAL, Just x, Just y) -> toLiteral $ shiftL x (toInt y)
                (ShiftAR, Just x, Just y) -> toLiteral $ shiftR x (toInt y)
                (ShiftL , Just x, Just y) -> toLiteral $ shiftL x (toInt y)
                (ShiftR , Just x, Just y) ->
                    if x < 0 && y > 0
                        then BinOp ShiftR (Number a) (Number b)
                        else toLiteral $ shiftR x (toInt y)
                _ -> BinOp op e1' e2'
            where
                toInt :: Integer -> Int
                toInt = fromIntegral
        (Add, BinOp Add e (Number a), Number b) ->
            case (readNumber a, readNumber b) of
                (Just x, Just y) -> BinOp Add e $ toLiteral (x + y)
                _ -> BinOp op e1' e2'
        (Sub, e, Number "-1") -> BinOp Add e (Number "1")
        _ -> BinOp op e1' e2'
    where
        e1' = simplify e1
        e2' = simplify e2
        bool True = Number "1"
        bool False = Number "0"
simplify other = other

toLiteral :: Integer -> Expr
toLiteral n =
    if n >= 4294967296
        then Number $ show (bits n) ++ "'d" ++ show n
        else Number $ show n

bits :: Integer -> Integer
bits 0 = 0
bits n = 1 + bits (quot n 2)

rangeSize :: Range -> Expr
rangeSize (s, e) =
    endianCondExpr (s, e) a b
    where
        a = rangeSizeHiLo (s, e)
        b = rangeSizeHiLo (e, s)

rangeSizeHiLo :: Range -> Expr
rangeSizeHiLo (hi, lo) =
    simplify $ BinOp Add (BinOp Sub hi lo) (Number "1")

-- chooses one or the other expression based on the endianness of the given
-- range; [hi:lo] chooses the first expression
endianCondExpr :: Range -> Expr -> Expr -> Expr
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

dimensionsSize :: [Range] -> Expr
dimensionsSize ranges =
    simplify $
    foldl (BinOp Mul) (Number "1") $
    map rangeSize $
    ranges

type ParamBinding = (Identifier, TypeOrExpr)

showParams :: [ParamBinding] -> String
showParams params = indentedParenList $ map showParam params

showParam :: ParamBinding -> String
showParam ("*", Right Nil) = ".*"
showParam (i, arg) =
    printf fmt i (either show show arg)
    where fmt = if i == "" then "%s%s" else ".%s(%s)"
