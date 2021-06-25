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
    , showDecls
    ) where

import Data.List (intercalate)
import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp (showPad, unlines')
import Language.SystemVerilog.AST.Type (Type(TypedefRef, UnpackedType), Identifier, pattern UnknownType)
import Language.SystemVerilog.AST.Expr (Expr, Range, showRanges, showAssignment)

data Decl
    = Param     ParamScope Type Identifier Expr
    | ParamType ParamScope Identifier Type
    | Variable   Direction Type Identifier [Range] Expr
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
    show (CommentDecl c) =
        if elem '\n' c
            then "// " ++ show c
            else "// " ++ c

showDecls :: Char -> String -> [Decl] -> String
showDecls delim whitespace =
    dropDelim . intercalate whitespace . map showDecl
    where
        dropDelim :: String -> String
        dropDelim [] = []
        dropDelim [x] = if x == delim then [] else [x]
        dropDelim (x : xs) = x : dropDelim xs
        showDecl (CommentDecl c) =
            if whitespace == " "
                then "/* " ++ c ++ " */"
                else show $ CommentDecl c
        showDecl decl = (init $ show decl) ++ [delim]

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
