{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog left-hand sides (aka lvals)
 -
 - TODO: Normal parameters can be declared with no default valu.
 -}

module Language.SystemVerilog.AST.Decl
    ( Decl       (..)
    , Direction  (..)
    , ParamScope (..)
    ) where

import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp (showPad, unlines')
import Language.SystemVerilog.AST.Type (Type, Identifier)
import Language.SystemVerilog.AST.Expr (Expr, Range, showRanges, showAssignment)

data Decl
    = Param     ParamScope Type Identifier Expr
    | ParamType ParamScope Identifier (Maybe Type)
    | Variable   Direction Type Identifier [Range] (Maybe Expr)
    deriving Eq

instance Show Decl where
    showList l _ = unlines' $ map show l
    show (Param s t x e) = printf "%s %s%s = %s;" (show s) (showPad t) x (show e)
    show (ParamType s x mt) = printf "%s type %s%s;" (show s) x (showAssignment mt)
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

data ParamScope
    = Parameter
    | Localparam
    deriving Eq

instance Show ParamScope where
    show Parameter  = "parameter"
    show Localparam = "localparam"
