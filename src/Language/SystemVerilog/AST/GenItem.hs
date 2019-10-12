{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog `generate` items
 -}

module Language.SystemVerilog.AST.GenItem
    ( GenItem (..)
    , GenCase
    ) where

import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp

import Language.SystemVerilog.AST.Expr (Expr)
import Language.SystemVerilog.AST.Op (AsgnOp)
import Language.SystemVerilog.AST.Type (Identifier)
import {-# SOURCE #-} Language.SystemVerilog.AST.ModuleItem (ModuleItem)

data GenItem
    = GenBlock Identifier [GenItem]
    | GenCase  Expr [GenCase] (Maybe GenItem)
    | GenFor   (Bool, Identifier, Expr) Expr (Identifier, AsgnOp, Expr) GenItem
    | GenIf    Expr GenItem GenItem
    | GenNull
    | GenModuleItem ModuleItem
    deriving Eq

instance Show GenItem where
    showList i _ = unlines' $ map show i
    show (GenBlock x i)  =
        printf "begin%s\n%s\nend"
            (if null x then "" else " : " ++ x)
            (indent $ unlines' $ map show i)
    show (GenCase e cs def) =
        printf "case (%s)\n%s%s\nendcase" (show e) bodyStr defStr
        where
            bodyStr = indent $ unlines' $ map showCase cs
            defStr = case def of
                Nothing -> ""
                Just c -> printf "\n\tdefault: %s" (show c)
    show (GenIf e a GenNull) = printf "if (%s) %s"          (show e) (show a)
    show (GenIf e a b      ) = printf "if (%s) %s\nelse %s" (show e) (show a) (show b)
    show (GenFor (new, x1, e1) c (x2, o2, e2) s) =
        printf "for (%s%s = %s; %s; %s %s %s) %s"
            (if new then "genvar " else "")
            x1 (show e1)
            (show c)
            x2 (show o2) (show e2)
            (show s)
    show (GenNull) = ";"
    show (GenModuleItem item) = show item

type GenCase = ([Expr], GenItem)

showCase :: (Show x, Show y) => ([x], y) -> String
showCase (a, b) = printf "%s: %s" (commas $ map show a) (show b)

