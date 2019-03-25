{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog expressions
 -}

module Language.SystemVerilog.AST.Expr
    ( Expr (..)
    , Range
    , showAssignment
    , showRanges
    ) where

import Data.List (intercalate)
import Text.Printf (printf)

import Language.SystemVerilog.AST.Op
import Language.SystemVerilog.AST.ShowHelp
import {-# SOURCE #-} Language.SystemVerilog.AST.Type

type Range = (Expr, Expr)

data Expr
    = String  String
    | Number  String
    | Ident   Identifier
    | Range   Expr Range
    | Bit     Expr Expr
    | Repeat  Expr [Expr]
    | Concat  [Expr]
    | Call    Identifier [Maybe Expr]
    | UniOp   UniOp Expr
    | BinOp   BinOp Expr Expr
    | Mux     Expr Expr Expr
    | Cast    Type Expr
    | Dot     Expr Identifier
    | Pattern [(Maybe Identifier, Expr)]
    deriving (Eq, Ord)

instance Show Expr where
    show (Number  str  ) = str
    show (Ident   str  ) = str
    show (String  str  ) = printf "\"%s\"" str
    show (Bit     e b  ) = printf "%s[%s]"     (show e) (show b)
    show (Range   e r  ) = printf "%s%s"       (show e) (showRange r)
    show (Repeat  e l  ) = printf "{%s {%s}}"  (show e) (commas $ map show l)
    show (Concat  l    ) = printf "{%s}"                (commas $ map show l)
    show (UniOp   a b  ) = printf "(%s %s)"    (show a) (show b)
    show (BinOp   o a b) = printf "(%s %s %s)" (show a) (show o) (show b)
    show (Cast    t e  ) = printf "%s'(%s)"    (show t) (show e)
    show (Dot     e n  ) = printf "%s.%s"      (show e) n
    show (Mux     c a b) = printf "(%s ? %s : %s)" (show c) (show a) (show b)
    show (Call    f l  ) = printf "%s(%s)" f (commas $ map (maybe "" show) l)
    show (Pattern l    ) =
        printf "'{\n%s\n}" (indent $ intercalate ",\n" $ map showPatternItem l)
        where
            showPatternItem :: (Maybe Identifier, Expr) -> String
            showPatternItem (Nothing, e) = show e
            showPatternItem (Just n , e) = printf "%s: %s" n (show e)

showAssignment :: Maybe Expr -> String
showAssignment Nothing = ""
showAssignment (Just val) = " = " ++ show val

showRanges :: [Range] -> String
showRanges [] = ""
showRanges l = " " ++ (concatMap showRange l)

showRange :: Range -> String
showRange (h, l) = printf "[%s:%s]" (show h) (show l)
