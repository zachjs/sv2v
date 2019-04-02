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
convertExpr (Bits (Left (IntegerVector _ _ rs))) = size rs
convertExpr (Bits (Left (Implicit        _ rs))) = size rs
convertExpr other = other

size :: [Range] -> Expr
size ranges =
    simplify $
    foldl (BinOp Mul) (Number "1") $
    map rangeSize $
    ranges
