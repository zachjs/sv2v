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
import {-# SOURCE #-} Language.SystemVerilog.AST.ModuleItem (ModuleItem, showGenModuleItem)

data GenItem
    = GenBlock Identifier [GenItem]
    | GenCase  Expr [GenCase]
    | GenFor   (Identifier, Expr) Expr (Identifier, AsgnOp, Expr) GenItem
    | GenIf    Expr GenItem GenItem
    | GenNull
    | GenModuleItem ModuleItem
    deriving Eq

instance Show GenItem where
    showList i _ = unlines' $ map show i
    show (GenBlock x i) =
        "if (1) " ++ showBareBlock (GenBlock x i)
    show (GenCase e cs) =
        printf "case (%s)\n%s\nendcase" (show e) bodyStr
        where bodyStr = indent $ unlines' $ map showGenCase cs
    show (GenIf e a GenNull) = printf "if (%s) %s"          (show e) (showBareBlock a)
    -- showBlockedBranch avoids dangling else ambiguity
    show (GenIf e a b      ) = printf "if (%s) %s\nelse %s" (show e) (showBlockedBranch a) (showBareBlock b)
    show (GenFor (x1, e1) c (x2, o2, e2) s) =
        printf "for (%s = %s; %s; %s %s %s) %s"
            x1 (show e1)
            (show c)
            x2 (show o2) (show e2)
            (showBlockedBranch s) -- Verilog 2001 requires this to be a block
    show (GenNull) = ""
    show (GenModuleItem item) = showGenModuleItem item

showBareBlock :: GenItem -> String
showBareBlock (GenBlock x i) =
    printf "begin%s\n%s\nend"
        (if null x then "" else " : " ++ x)
        (indent $ show i)
showBareBlock (GenNull) = ";"
showBareBlock item = show item

showBlockedBranch :: GenItem -> String
showBlockedBranch genItem@GenBlock{} = showBareBlock genItem
showBlockedBranch genItem = showBareBlock $ GenBlock "" [genItem]

type GenCase = ([Expr], GenItem)

showGenCase :: GenCase -> String
showGenCase (a, b) = printf "%s: %s" exprStr (showBareBlock b)
    where exprStr = if null a then "default" else commas $ map show a
