{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Elaboration of `$bits`, where possible
 -}

module Convert.Bits (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert =
    traverseDescriptions $
    traverseModuleItems $
    traverseExprs $
    traverseNestedExprs $
    convertExpr

convertExpr :: Expr -> Expr
convertExpr (Bits (Left (IntegerVector _ _ [r]))) = rangeSize r
convertExpr (Bits (Left (Implicit        _ [r]))) = rangeSize r
convertExpr other = other
