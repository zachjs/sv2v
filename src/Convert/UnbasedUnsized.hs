{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for unbased, unsized literals ('0, '1, 'z, 'x)
 -
 - The literals are given a binary base and are made signed to enable sign
 - extension. In self-determined contexts, the literals are additionally given
 - an explicit size of 1. This enables the desired implicit casting in
 - Verilog-2005.
 -}

module Convert.UnbasedUnsized (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $
    traverseDescriptions $ traverseModuleItems $
    traverseExprs $ traverseNestedExprs convertExpr

digits :: [Char]
digits = ['0', '1', 'x', 'z', 'X', 'Z']

literalFor :: String -> Char -> Expr
literalFor prefix ch =
    if elem ch digits
        then Number (prefix ++ [ch])
        else error $ "unexpected unbased-unsized digit: " ++ [ch]

sizedLiteralFor :: Char -> Expr
sizedLiteralFor = literalFor "1'sb"

unsizedLiteralFor :: Char -> Expr
unsizedLiteralFor = literalFor "'sb"

convertExpr :: Expr -> Expr
convertExpr (Mux cond left right) =
    Mux cond (convertExprUnsized left) (convertExprUnsized right)
convertExpr (Number ['\'', ch]) =
    sizedLiteralFor ch
convertExpr other = other

convertExprUnsized :: Expr -> Expr
convertExprUnsized (Number ['\'', ch]) =
    unsizedLiteralFor ch
convertExprUnsized other = other
