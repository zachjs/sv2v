{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog left-hand sides (aka lvals)
 -}

module Language.SystemVerilog.AST.Decl
    ( Decl (..)
    , Direction (..)
    ) where

import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp (showPad, unlines')
import Language.SystemVerilog.AST.Type (Type, Identifier)
import Language.SystemVerilog.AST.Expr (Expr, Range, showRanges, showAssignment)

data Decl
    = Parameter            Type Identifier Expr
    | Localparam           Type Identifier Expr
    | Variable   Direction Type Identifier [Range] (Maybe Expr)
    deriving Eq

instance Show Decl where
    showList l _ = unlines' $ map show l
    show (Parameter  t x e) = printf  "parameter %s%s = %s;" (showPad t) x (show e)
    show (Localparam t x e) = printf "localparam %s%s = %s;" (showPad t) x (show e)
    show (Variable d t x a me) = printf "%s%s%s%s%s;" (showPad d) (showPad t) x (showRanges a) (showAssignment me)

data Direction
    = Input
    | Output
    | Inout
    | Local
    deriving Eq

instance Show Direction where
    show Input  = "input"
    show Output = "output"
    show Inout  = "inout"
    show Local  = ""
