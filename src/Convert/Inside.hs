{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `inside` expressions
 -
 - The expressions are compared to each candidate using the wildcard comparison
 - operator. Note that if expression has any Xs or Zs that are not wildcarded in
 - the candidate, the results is `1'bx`. As required by the specification, the
 - result of each comparison is combined using an OR reduction.
 -
 - TODO: Add support for array value ranges.
 -}

module Convert.Inside (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $
    traverseDescriptions $ traverseModuleItems $
    traverseExprs $ traverseNestedExprs convertExpr

convertExpr :: Expr -> Expr
convertExpr (Inside expr valueRanges) =
    if length checks == 1
        then head checks
        else UniOp RedOr $ Concat checks
    where
        checks = map toCheck valueRanges
        toCheck :: ExprOrRange -> Expr
        toCheck (Left e) =
            Mux
            (BinOp TNe rxr lxlxrxr)
            (Number "1'bx")
            (BinOp WEq expr e)
            where
                lxl = BinOp BitXor expr expr
                rxr = BinOp BitXor e e
                lxlxrxr = BinOp BitXor lxl rxr
        toCheck (Right (lo, hi)) =
            BinOp LogAnd
                (BinOp Le lo expr)
                (BinOp Ge hi expr)
convertExpr other = other
