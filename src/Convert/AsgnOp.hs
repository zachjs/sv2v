{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for binary assignment operators, which appear in generate for
 - loops and as a special case of blocking assignment statements. We simply
 - elaborate them in the obvious manner.
 -}

module Convert.AsgnOp (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert =
    traverseDescriptions $ traverseModuleItems $
    ( traverseStmts    convertStmt
    . traverseGenItems convertGenItem
    )

convertGenItem :: GenItem -> GenItem
convertGenItem (GenFor a b (ident, AsgnOp op, expr) c d) =
    GenFor a b (ident, AsgnOpEq, BinOp op (Ident ident) expr) c d
convertGenItem other = other

convertStmt :: Stmt -> Stmt
convertStmt (AsgnBlk (AsgnOp op) lhs expr) =
    AsgnBlk AsgnOpEq lhs (BinOp op (lhsToExpr lhs) expr)
convertStmt other = other

lhsToExpr :: LHS -> Expr
lhsToExpr (LHSIdent   x) = Ident x
lhsToExpr (LHSBit   l e) = Bit   (lhsToExpr l) e
lhsToExpr (LHSRange l r) = Range (lhsToExpr l) r
lhsToExpr (LHSDot   l x) = Dot   (lhsToExpr l) x
lhsToExpr (LHSConcat ls) = Concat $ map lhsToExpr ls
