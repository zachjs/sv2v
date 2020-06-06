{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for unbased, unsized literals ('0, '1, 'z, 'x)
 -
 - The literals are given a binary base, a size of 1, and are made signed to
 - allow sign extension. This enables the desired implicit casting in
 - Verilog-2005.
 -
 - However, in a ternary expressions, these literals should take on the sign and
 - size of their counterpart. To work around this, we explicitly size cast these
 - literlas when they appear within a ternary expression.
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

literalFor :: Char -> Expr
literalFor ch =
    if elem ch digits
        then Number ("1'sb" ++ [ch])
        else error $ "unexpected unbased-unsized digit: " ++ [ch]

convertExpr :: Expr -> Expr
convertExpr (Mux cond left right) =
    Mux cond (convertExprCast left right) (convertExprCast right left)
convertExpr (Number ['\'', ch]) =
    literalFor ch
convertExpr other = other

convertExprCast :: Expr -> Expr -> Expr
convertExprCast (Number ['\'', ch]) other =
    Cast (Right size) (literalFor ch)
    where
        size = case other of
            Number ['\'', _] -> Number "32"
            _ -> DimsFn FnBits $ Right other
convertExprCast other _ = other
