{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog left-hand sides (aka lvals)
 -}

module Language.SystemVerilog.AST.LHS
    ( LHS (..)
    ) where

import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp (commas)
import Language.SystemVerilog.AST.Type (Identifier)
import Language.SystemVerilog.AST.Expr (Expr, PartSelectMode, Range)
import Language.SystemVerilog.AST.Op (StreamOp)

data LHS
    = LHSIdent  Identifier
    | LHSBit    LHS Expr
    | LHSRange  LHS PartSelectMode Range
    | LHSDot    LHS Identifier
    | LHSConcat [LHS]
    | LHSStream StreamOp Expr [LHS]
    deriving Eq

instance Show LHS where
    show (LHSIdent x         ) = x
    show (LHSBit   l e       ) = printf "%s[%s]"     (show l) (show e)
    show (LHSRange l m (a, b)) = printf "%s[%s%s%s]" (show l) (show a) (show m) (show b)
    show (LHSDot   l x       ) = printf "%s.%s"      (show l) x
    show (LHSConcat      lhss) = printf "{%s}"       (commas $ map show lhss)
    show (LHSStream  o e lhss) = printf "{%s %s%s}"  (show o) (show e) (show $ LHSConcat lhss)
