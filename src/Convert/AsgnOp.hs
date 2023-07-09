{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for binary assignment operators, which can appear in for loops and
 - as a special case of blocking assignment statements. We simply elaborate them
 - in the obvious manner: a += b -> a = a + b.
 -}

module Convert.AsgnOp (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $ traverseDescriptions $ traverseModuleItems $
    ( traverseStmts (traverseNestedStmts convertStmt)
    . traverseGenItems (traverseNestedGenItems convertGenItem)
    )

convertGenItem :: GenItem -> GenItem
convertGenItem (GenFor a b (ident, AsgnOp op, expr) c) =
    GenFor a b (ident, AsgnOpEq, elabBinOp op (Ident ident) expr) c
convertGenItem other = other

convertStmt :: Stmt -> Stmt
convertStmt (For inits cc asgns stmt) =
    For inits cc asgns' stmt
    where
        asgns' = map convertAsgn asgns
        convertAsgn :: (LHS, AsgnOp, Expr) -> (LHS, AsgnOp, Expr)
        convertAsgn (lhs, AsgnOp op, expr) =
            (lhs, AsgnOpEq, elabBinOp op (lhsToExpr lhs) expr)
        convertAsgn other = other
convertStmt (Asgn (AsgnOp op) mt lhs expr) =
    Asgn AsgnOpEq mt lhs (elabBinOp op (lhsToExpr lhs) expr)
convertStmt other = other

elabBinOp :: BinOp -> Expr -> Expr -> Expr
elabBinOp Add e1 (UniOp UniSub e2) = BinOp Sub e1 e2
elabBinOp Sub e1 (UniOp UniSub e2) = BinOp Add e1 e2
elabBinOp op e1 e2 = BinOp op e1 e2
