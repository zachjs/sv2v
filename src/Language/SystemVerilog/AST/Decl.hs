{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog data, net, and parameter declarations
 -}

module Language.SystemVerilog.AST.Decl
    ( Decl       (..)
    , Direction  (..)
    , ParamScope (..)
    ) where

import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp (showPad, showPadBefore, unlines')
import Language.SystemVerilog.AST.Type (Type(TypedefRef, UnpackedType), Identifier, pattern UnknownType, NetType, Strength)
import Language.SystemVerilog.AST.Expr (Expr, Range, showRanges, showAssignment)

data Decl
    = Param     ParamScope Type Identifier Expr
    | ParamType ParamScope Identifier Type
    | Variable   Direction Type Identifier [Range] Expr
    | Net Direction NetType Strength Type Identifier [Range] Expr
    | CommentDecl String
    deriving Eq

instance Show Decl where
    showList l _ = unlines' $ map show l
    show (Param s t x e) = printf "%s %s%s%s;" (show s) (showPad t) x (showAssignment e)
    show (ParamType Localparam x (TypedefRef e)) =
        printf "typedef %s %s;" (show e) x
    show (ParamType Localparam x (UnpackedType t rs)) =
        printf "typedef %s %s%s;" (show t) x (showRanges rs)
    show (ParamType s x t) = printf "%s type %s%s;" (show s) x tStr
        where tStr = if t == UnknownType then "" else  " = " ++ show t
    show (Variable d t x a e) = printf "%s%s%s%s%s;" (showPad d) (showPad t) x (showRanges a) (showAssignment e)
    show (Net  d n s t x a e) = printf "%s%s%s %s%s%s%s;" (showPad d) (show n) (showPadBefore s) (showPad t) x (showRanges a) (showAssignment e)
    show (CommentDecl c) = "// " ++ c

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
