{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog expressions
 -}

module Language.SystemVerilog.AST.Expr
    ( Expr (..)
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
    ) where

import Data.Bits (shiftL, shiftR)
import Data.Int (Int32)
import Data.List (intercalate)
import Data.Word (Word32)
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
    | Ident   Identifier
    | PSIdent Identifier Identifier
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

instance Show Expr where
    show (Nil          ) = ""
    show (Number  str  ) = str
    show (Time    str  ) = str
    show (Ident   str  ) = str
    show (PSIdent x y  ) = printf "%s::%s" x y
    show (String  str  ) = printf "\"%s\"" str
    show (Bit     e b  ) = printf "%s[%s]"     (show e) (show b)
    show (Range   e m r) = printf "%s[%s%s%s]" (show e) (show $ fst r) (show m) (show $ snd r)
    show (Repeat  e l  ) = printf "{%s {%s}}"  (show e) (commas $ map show l)
    show (Concat  l    ) = printf "{%s}"                (commas $ map show l)
    show (Stream  o e l) = printf "{%s %s%s}"  (show o) (show e) (show $ Concat l)
    show (UniOp   o e  ) = printf "%s%s"       (show o) (showUniOpPrec e)
    show (BinOp   o a b) = printf "%s %s %s"   (showBinOpPrec a) (show o) (showBinOpPrec b)
    show (Dot     e n  ) = printf "%s.%s"      (show e) n
    show (Mux     c a b) = printf "(%s ? %s : %s)" (show c) (show a) (show b)
    show (Call    e l  ) = printf "%s%s"       (show e) (show l)
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

data Args
    = Args [Maybe Expr] [(Identifier, Maybe Expr)]
    deriving (Eq, Ord)

instance Show Args where
    show (Args pnArgs kwArgs) = "(" ++ (commas strs) ++ ")"
        where
            strs = (map showPnArg pnArgs) ++ (map showKwArg kwArgs)
            showPnArg = maybe "" show
            showKwArg (x, me) = printf ".%s(%s)" x (showPnArg me)

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


showAssignment :: Show a => Maybe a -> String
showAssignment Nothing = ""
showAssignment (Just val) = " = " ++ show val

showRanges :: [Range] -> String
showRanges [] = ""
showRanges l = " " ++ (concatMap showRange l)

showRange :: Range -> String
showRange (h, l) = printf "[%s:%s]" (show h) (show l)

showExprOrRange :: ExprOrRange -> String
showExprOrRange (Left  x) = show x
showExprOrRange (Right x) = show x

clog2Help :: Int32 -> Int32 -> Int32
clog2Help p n = if p >= n then 0 else 1 + clog2Help (p*2) n
clog2 :: Int32 -> Int32
clog2 n = if n < 2 then 0 else clog2Help 1 n

readNumber :: String -> Maybe Int32
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

showUniOpPrec :: Expr -> String
showUniOpPrec (e @ UniOp{}) = printf "(%s)" (show e)
showUniOpPrec (e @ BinOp{}) = printf "(%s)" (show e)
showUniOpPrec e = show e

showBinOpPrec :: Expr -> String
showBinOpPrec (e @ BinOp{}) = printf "(%s)" (show e)
showBinOpPrec e = show e

-- basic expression simplfication utility to help us generate nicer code in the
-- common case of ranges like `[FOO-1:0]`
simplify :: Expr -> Expr
simplify (UniOp LogNot (Number "1")) = Number "0"
simplify (UniOp LogNot (Number "0")) = Number "1"
simplify (UniOp LogNot (BinOp Eq a b)) = BinOp Ne a b
simplify (orig @ (UniOp UniSub (Number n))) =
    case readNumber n of
        Nothing -> orig
        Just x -> Number $ show (-x)
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
simplify (orig @ (Call (Ident "$clog2") (Args [Just (Number n)] []))) =
    case readNumber n of
        Nothing -> orig
        Just x -> Number $ show $ clog2 x
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
simplify (BinOp Add (BinOp Sub (Number n1) e) (Number n2)) =
    case (readNumber n1, readNumber n2) of
        (Just x, Just y) ->
            simplify $ BinOp Sub (Number $ show (x + y)) e'
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
                (Add, Just x, Just y) -> Number $ show (x + y)
                (Sub, Just x, Just y) -> Number $ show (x - y)
                (Mul, Just x, Just y) -> Number $ show (x * y)
                (Div, Just _, Just 0) -> Number "x"
                (Div, Just x, Just y) -> Number $ show (x `quot` y)
                (Mod, Just x, Just y) -> Number $ show (x `rem` y)
                (Pow, Just x, Just y) -> Number $ show (x ^ y)
                (Eq , Just x, Just y) -> bool $ x == y
                (Ne , Just x, Just y) -> bool $ x /= y
                (Gt , Just x, Just y) -> bool $ x >  y
                (Ge , Just x, Just y) -> bool $ x >= y
                (Lt , Just x, Just y) -> bool $ x <  y
                (Le , Just x, Just y) -> bool $ x <= y
                (ShiftAL, Just x, Just y) -> Number $ show $ shiftL x (toInt y)
                (ShiftAR, Just x, Just y) -> Number $ show $ shiftR x (toInt y)
                (ShiftL , Just x, Just y) -> Number $ show $ shiftL x (toInt y)
                (ShiftR , Just x, Just y) -> -- does not sign extend
                    Number $ show $ toInt32 $ shiftR (toWord32 x) (toInt y)
                _ -> BinOp op e1' e2'
            where
                toInt :: Int32 -> Int
                toInt = fromIntegral
                toWord32 :: Int32 -> Word32
                toWord32 = fromIntegral
                toInt32 :: Word32 -> Int32
                toInt32 = fromIntegral
        (Add, BinOp Add e (Number a), Number b) ->
            case (readNumber a, readNumber b) of
                (Just x, Just y) -> BinOp Add e $ Number $ show (x + y)
                _ -> BinOp op e1' e2'
        (Sub, e, Number "-1") -> BinOp Add e (Number "1")
        _ -> BinOp op e1' e2'
    where
        e1' = simplify e1
        e2' = simplify e2
        bool True = Number "1"
        bool False = Number "0"
simplify other = other

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
