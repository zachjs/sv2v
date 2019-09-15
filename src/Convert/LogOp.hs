{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for logical implication (->) and logical equality (<->) operators.
 -
 - We convert `a -> b` to `!a || b`, as per the definition of implication.
 -
 - We convert `a <-> b` to `!a = !b`. Note that we can't simply use `a = b` as
 - `1 != 2`, but `1 <-> 2`.
 -}

module Convert.LogOp (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $
    traverseDescriptions $ traverseModuleItems $
    traverseExprs $ traverseNestedExprs convertExpr

convertExpr :: Expr -> Expr
convertExpr (BinOp LogEq a b) =
    BinOp Eq (UniOp LogNot a) (UniOp LogNot b)
convertExpr (BinOp LogImp a b) =
    BinOp LogOr (UniOp LogNot a) b
convertExpr other = other
