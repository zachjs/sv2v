{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `==?` and `!=?`
 -
 - `a ==? b` is defined as the bitwise comparison of `a` and `b`, where X and Z
 - values in `b` (but not those in `a`) are used as wildcards. This conversion
 - relies on the fact that works because any value xor'ed with X or Z becomes X.
 -
 - Procedure for `A ==? B`:
 - 1. If there is any bit in A that doesn't match a non-wildcarded bit in B,
 -    then the result is always `1'b0`.
 - 2. If there is any X or Z in A that is not wildcarded in B, then the result
 -    is `1'bx`.
 - 3. Otherwise, the result is `1'b1`.
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
    Mux noteq (Number "1'b0") $
    Mux badxs (Number "1'bx")
              (Number "1'b1")
    where
        lxl = BinOp BitXor l l
        rxr = BinOp BitXor r r
        -- Step #1: definitive mismatch
        noteq = BinOp TNe rxlxl lxrxr
        rxlxl = BinOp BitXor r lxl
        lxrxr = BinOp BitXor l rxr
        -- Step #2: extra X or Z
        badxs = BinOp TNe lxlxrxr rxr
        lxlxrxr = BinOp BitXor lxl rxr
convertExpr (BinOp WNe l r) =
    UniOp LogNot $
    convertExpr $
    BinOp WEq l r
convertExpr other = other
