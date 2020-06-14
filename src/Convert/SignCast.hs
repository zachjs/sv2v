{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `signed` and `unsigned` type casts.
 -
 - SystemVerilog has `signed'(foo)` and `unsigned'(foo)` as syntactic sugar for
 - the `$signed` and `$unsigned` system functions present in Verilog-2005. This
 - conversion elaborates these casts.
 -}

module Convert.SignCast (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $
    traverseDescriptions $
    traverseModuleItems $
    traverseExprs $
    traverseNestedExprs convertExpr

convertExpr :: Expr -> Expr
convertExpr (Cast (Left (Implicit Signed [])) e) =
    Call (Ident "$signed") (Args [e] [])
convertExpr (Cast (Left (Implicit Unsigned [])) e) =
    Call (Ident "$unsigned") (Args [e] [])
convertExpr other = other
