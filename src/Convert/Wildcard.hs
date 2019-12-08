{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `==?` and `!=?`
 -
 - `a ==? b` is defined as the bitwise comparison of `a` and `b`, where X and Z
 - values in `b` (but not those in `a`) are used as wildcards. We convert `a ==?
 - b` to `a ^ b === b ^ b`. This works because any value xor'ed with X or Z
 - becomes X.
 -
 - `!=?` is simply converted as the logical negation of `==?`, which is
 - converted as described above.
 -}

module Convert.Wildcard (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $
    traverseDescriptions $ traverseModuleItems $
    traverseExprs $ traverseNestedExprs convertExpr

convertExpr :: Expr -> Expr
convertExpr (BinOp WEq l r) =
    BinOp TEq
    (BinOp BitXor r r)
    (BinOp BitXor r l)
convertExpr (BinOp WNe l r) =
    UniOp LogNot $
    convertExpr $
    BinOp WEq l r
convertExpr other = other
