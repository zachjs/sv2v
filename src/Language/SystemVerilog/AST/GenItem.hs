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
    = GenBlock (Maybe Identifier) [GenItem]
    | GenCase  Expr [GenCase] (Maybe GenItem)
    | GenFor   (Identifier, Expr) Expr (Identifier, AsgnOp, Expr) (Maybe Identifier) [GenItem]
    | GenIf    Expr GenItem GenItem
    | GenNull
    | GenModuleItem ModuleItem
    deriving Eq

instance Show GenItem where
    showList i _ = unlines' $ map show i
    show (GenBlock mx i)  =
        printf "begin%s\n%s\nend"
            (maybe "" (" : " ++) mx)
            (indent $ unlines' $ map show i)
    show (GenCase e c md) =
        printf "case (%s)\n%s%s\nendcase" (show e)
            (indent $ unlines' $ map showCase c)
            (maybe "" (indent . indent . show) md)
    show (GenIf e a GenNull) = printf "if (%s) %s"          (show e) (show a)
    show (GenIf e a b      ) = printf "if (%s) %s\nelse %s" (show e) (show a) (show b)
    show (GenFor (x1, e1) c (x2, o2, e2) mx is) =
        printf "for (%s = %s; %s; %s %s %s) %s"
            x1 (show e1)
            (show c)
            x2 (show o2) (show e2)
            (show $ GenBlock mx is)
    show (GenNull) = ";"
    show (GenModuleItem item) = show item

type GenCase = ([Expr], GenItem)
