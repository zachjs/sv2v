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
    , Args (..)
    , PartSelectMode (..)
    , showAssignment
    , showRanges
    , simplify
    , rangeSize
    , endianCondExpr
    , endianCondRange
    , sizedExpr
    , dimensionsSize
    ) where

import Data.List (intercalate)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Language.SystemVerilog.AST.Op
import Language.SystemVerilog.AST.ShowHelp
import {-# SOURCE #-} Language.SystemVerilog.AST.Type

type Range = (Expr, Expr)

type TypeOrExpr = Either Type Expr

data Expr
    = String  String
    | Number  String
    | Ident   Identifier
    | PSIdent Identifier Identifier
    | Range   Expr PartSelectMode Range
    | Bit     Expr Expr
    | Repeat  Expr [Expr]
    | Concat  [Expr]
    | Stream  StreamOp Expr [Expr]
    | Call    (Maybe Identifier) Identifier Args
    | UniOp   UniOp Expr
    | BinOp   BinOp Expr Expr
    | Mux     Expr Expr Expr
    | Cast    TypeOrExpr Expr
    | Bits    TypeOrExpr
    | Dot     Expr Identifier
    | Pattern [(Maybe Identifier, Expr)]
    | Nil
    deriving (Eq, Ord)

instance Show Expr where
    show (Nil          ) = ""
    show (Number  str  ) = str
    show (Ident   str  ) = str
    show (PSIdent x y  ) = printf "%s::%s" x y
    show (String  str  ) = printf "\"%s\"" str
    show (Bit     e b  ) = printf "%s[%s]"     (show e) (show b)
    show (Range   e m r) = printf "%s[%s%s%s]" (show e) (show $ fst r) (show m) (show $ snd r)
    show (Repeat  e l  ) = printf "{%s {%s}}"  (show e) (commas $ map show l)
    show (Concat  l    ) = printf "{%s}"                (commas $ map show l)
    show (Stream  o e l) = printf "{%s %s%s}"  (show o) (show e) (show $ Concat l)
    show (UniOp   a b  ) = printf "(%s %s)"    (show a) (show b)
    show (BinOp   o a b) = printf "(%s %s %s)" (show a) (show o) (show b)
    show (Dot     e n  ) = printf "%s.%s"      (show e) n
    show (Mux     c a b) = printf "(%s ? %s : %s)" (show c) (show a) (show b)
    show (Call   ps f l) = printf "%s%s(%s)" (maybe "" (++ "::") ps) f (show l)
    show (Cast tore e  ) = printf "%s'(%s)" (showEither tore) (show e)
    show (Bits tore    ) = printf "$bits(%s)" (showEither tore)
    show (Pattern l    ) =
        printf "'{\n%s\n}" (indent $ intercalate ",\n" $ map showPatternItem l)
        where
            showPatternItem :: (Maybe Identifier, Expr) -> String
            showPatternItem (Nothing, e) = show e
            showPatternItem (Just n , e) = printf "%s: %s" n (show e)

data Args
    = Args [Maybe Expr] [(Identifier, Maybe Expr)]
    deriving (Eq, Ord)

instance Show Args where
    show (Args pnArgs kwArgs) = commas strs
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

showAssignment :: Show a => Maybe a -> String
showAssignment Nothing = ""
showAssignment (Just val) = " = " ++ show val

showRanges :: [Range] -> String
showRanges [] = ""
showRanges l = " " ++ (concatMap showRange l)

showRange :: Range -> String
showRange (h, l) = printf "[%s:%s]" (show h) (show l)

clog2Help :: Int -> Int -> Int
clog2Help p n = if p >= n then 0 else 1 + clog2Help (p*2) n
clog2 :: Int -> Int
clog2 n = if n < 2 then 0 else clog2Help 1 n

readNumber :: String -> Maybe Int
readNumber n =
    readMaybe n' :: Maybe Int
    where
        n' = case n of
            '\'' : 'd' : rest -> rest
            _ -> n

-- basic expression simplfication utility to help us generate nicer code in the
-- common case of ranges like `[FOO-1:0]`
simplify :: Expr -> Expr
simplify (Repeat (Number "0") _) = Concat []
simplify (Concat [expr]) = expr
simplify (Concat exprs) =
    Concat $ filter (/= Concat []) exprs
simplify (orig @ (Call Nothing "$clog2" (Args [Just (Number n)] []))) =
    case readNumber n of
        Nothing -> orig
        Just x -> Number $ show $ clog2 x
simplify (Mux (Number "0") e _) = e
simplify (Mux (BinOp Ge c1 c2) e1 e2) =
    case (c1', c2') of
        (Number a, Number b) ->
            case (readNumber a, readNumber b) of
                (Just x, Just y) ->
                    if x >= y
                        then e1
                        else e2
                _ -> nochange
        _ -> nochange
    where
        c1' = simplify c1
        c2' = simplify c2
        e1' = simplify e1
        e2' = simplify e2
        nochange = Mux (BinOp Ge c1' c2') e1' e2'
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
                _ -> BinOp op e1' e2'
        (Add, BinOp Add e (Number a), Number b) ->
            case (readNumber a, readNumber b) of
                (Just x, Just y) -> BinOp Add e $ Number $ show (x + y)
                _ -> BinOp op e1' e2'
        _ -> BinOp op e1' e2'
    where
        e1' = simplify e1
        e2' = simplify e2
simplify other = other

rangeSize :: Range -> Expr
rangeSize (s, e) =
    endianCondExpr (s, e) a b
    where
        a = simplify $ BinOp Add (BinOp Sub s e) (Number "1")
        b = simplify $ BinOp Add (BinOp Sub e s) (Number "1")

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

-- attempts to make a number literal have an explicit size
sizedExpr :: Identifier -> Range -> Expr -> Expr
sizedExpr x r (Number n) =
    if size /= show resSize
        then error $ "literal " ++ show n ++ " for " ++ show x
                ++ " doesn't have size " ++ show size
        else Number res
    where
        Number size = simplify $ rangeSize r
        unticked = case n of
            '\'' : rest -> rest
            rest -> rest
        resSize = (read $ takeWhile (/= '\'') res) :: Int
        res = case readMaybe unticked :: Maybe Int of
            Nothing ->
                if unticked == n
                    then n
                    else size ++ n
            Just num -> size ++ "'d" ++ show num
sizedExpr _ _ e = e

dimensionsSize :: [Range] -> Expr
dimensionsSize ranges =
    simplify $
    foldl (BinOp Mul) (Number "1") $
    map rangeSize $
    ranges
