{-# LANGUAGE PatternSynonyms #-}
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
    , DimsFn (..)
    , DimFn (..)
    , showAssignment
    , showRange
    , showRanges
    , ParamBinding
    , showParams
    , pattern RawNum
    ) where

import Data.List (intercalate)
import Text.Printf (printf)

import Language.SystemVerilog.AST.Number (Number(..))
import Language.SystemVerilog.AST.Op
import Language.SystemVerilog.AST.ShowHelp
import {-# SOURCE #-} Language.SystemVerilog.AST.Type

type Range = (Expr, Expr)

type TypeOrExpr = Either Type Expr

pattern RawNum :: Integer -> Expr
pattern RawNum n = Number (Decimal (-32) True n)

data Expr
    = String  String
    | Real    String
    | Number  Number
    | Time    String
    | Ident   Identifier
    | PSIdent Identifier Identifier
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
    | Pattern [(TypeOrExpr, Expr)]
    | Inside  Expr [Expr]
    | MinTypMax Expr Expr Expr
    | ExprAsgn Expr Expr
    | Nil
    deriving Eq

instance Show Expr where
    show (Nil          ) = ""
    show (Time    str  ) = str
    show (Ident   str  ) = str
    show (Real    str  ) = str
    show (Number  n    ) = show n
    show (PSIdent x y  ) = printf "%s::%s" x y
    show (CSIdent x p y) = printf "%s#%s::%s" x (showParams p) y
    show (String  str  ) = printf "\"%s\"" str
    show (Bit     e b  ) = printf "%s[%s]"     (show e) (show b)
    show (Range   e m r) = printf "%s[%s%s%s]" (show e) (show $ fst r) (show m) (show $ snd r)
    show (Repeat  e l  ) = printf "{%s {%s}}"  (show e) (commas $ map show l)
    show (Concat  l    ) = printf "{%s}"                (commas $ map show l)
    show (Stream  o e l) = printf "{%s %s%s}"  (show o) (show e) (show $ Concat l)
    show (Cast tore e  ) = printf "%s'(%s)"    toreStr  (show e)
        where toreStr = either show (flip showBinOpPrec []) tore
    show (DimsFn  f v  ) = printf "%s(%s)"     (show f) (showEither v)
    show (DimFn   f v e) = printf "%s(%s, %s)" (show f) (showEither v) (show e)
    show (Inside  e l  ) = printf "(%s inside { %s })" (show e) (intercalate ", " $ map show l)
    show (Pattern l    ) =
        printf "'{\n%s\n}" (indent $ intercalate ",\n" $ map showPatternItem l)
        where
            showPatternItem :: (TypeOrExpr, Expr) -> String
            showPatternItem (Right Nil, v) = show v
            showPatternItem (Right e, v) = printf "%s: %s" (show e) (show v)
            showPatternItem (Left t, v) = printf "%s: %s" tStr (show v)
                where tStr = if null (show t) then "default" else show t
    show (MinTypMax a b c) = printf "(%s : %s : %s)" (show a) (show b) (show c)
    show (ExprAsgn l r) = printf "(%s = %s)" (show l) (show r)
    show e@UniOp{} = showsPrec 0 e ""
    show e@BinOp{} = showsPrec 0 e ""
    show e@Dot  {} = showsPrec 0 e ""
    show e@Mux  {} = showsPrec 0 e ""
    show e@Call {} = showsPrec 0 e ""

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
    deriving Eq

instance Show Args where
    show (Args pnArgs kwArgs) = '(' : commas strs ++ ")"
        where
            strs = (map show pnArgs) ++ (map showKwArg kwArgs)
            showKwArg (x, e) = printf ".%s(%s)" x (show e)

data PartSelectMode
    = NonIndexed
    | IndexedPlus
    | IndexedMinus
    deriving Eq

instance Show PartSelectMode where
    show NonIndexed   = ":"
    show IndexedPlus  = "+:"
    show IndexedMinus = "-:"

data DimsFn
    = FnBits
    | FnDimensions
    | FnUnpackedDimensions
    deriving Eq

data DimFn
    = FnLeft
    | FnRight
    | FnLow
    | FnHigh
    | FnIncrement
    | FnSize
    deriving Eq

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
showRanges l = ' ' : concatMap showRange l

showRange :: Range -> String
showRange (h, l) = '[' : show h ++ ':' : show l ++ "]"

showUniOpPrec :: Expr -> ShowS
showUniOpPrec e@UniOp{} = (showParen True . shows) e
showUniOpPrec e@BinOp{} = (showParen True . shows) e
showUniOpPrec e = shows e

showBinOpPrec :: Expr -> ShowS
showBinOpPrec e@BinOp{} = (showParen True . shows) e
showBinOpPrec e = shows e

type ParamBinding = (Identifier, TypeOrExpr)

showParams :: [ParamBinding] -> String
showParams params = indentedParenList $ map showParam params

showParam :: ParamBinding -> String
showParam ("", arg) = showEither arg
showParam (i, arg) = printf ".%s(%s)" i (showEither arg)
