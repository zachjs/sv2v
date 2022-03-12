{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for assignments within expressions
 -
 - IEEE 1800-2017 Section 11.3.6 states that assignments within expressions can
 - only appear in procedural statements, though some tools support them in other
 - contexts. We do not currently raise an error for assignments within
 - expressions in unsupported contexts.
 -
 - Assignment expressions are replaced with the LHS, with the LHS being updated
 - in a preceding statement. For post-increment operations, a pre-increment is
 - performed and then the increment is reversed in the expression.
 -
 - This conversion occurs after the elaboration of `do` `while` loops and
 - `foreach` loops, but before the elaboration of jumps and extra `for` loop
 - initializations.
 -}

module Convert.ExprAsgn (convert) where

import Control.Monad.Writer.Strict
import Data.Bitraversable (bimapM)

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems $
    traverseStmts convertStmt

convertStmt :: Stmt -> Stmt

-- assignment expressions in a loop guard
convertStmt (While cond stmt) =
    if null add
        then While cond stmt'
        else block $ add ++ [While cond' $ injectLoopIter add stmt']
    where
        (cond', add) = runWriter $ convertExpr cond
        stmt' = convertStmt stmt

-- assignment expressions can appear in any part of the for loop
convertStmt (For inits cond incrs stmt) =
    if null initsAdd && null condAdd && null incrsAdd
        then For inits cond incrs stmt'
        else For initsCombined cond' incrs' $
            injectLoopIter (incrsAdd ++ condAdd) stmt'
    where
        (inits', initsAdd) = runWriter $ mapM convertInit inits
        (cond', condAdd) = runWriter $ convertExpr cond
        (incrs', incrsAdd) = runWriter $ mapM convertIncr incrs
        stmt' = convertStmt stmt
        initsCombined = map toInit initsAdd ++ inits' ++ map toInit condAdd

-- assignment expressions in other statements are added in front
convertStmt stmt =
    traverseSinglyNestedStmts convertStmt $
        if null add
            then stmt
            else block $ add ++ [stmt']
    where (stmt', add) = runWriter $ traverseStmtExprsM convertExpr stmt

-- helper for creating simple blocks
block :: [Stmt] -> Stmt
block = Block Seq "" []

-- add statements before the loop guard is checked
injectLoopIter :: [Stmt] -> Stmt -> Stmt
injectLoopIter add stmt = block $ beforeContinue add stmt : add

-- add statements before every `continue` in the loop
beforeContinue :: [Stmt] -> Stmt -> Stmt
beforeContinue _ stmt@While{} = stmt
beforeContinue _ stmt@For{} = stmt
beforeContinue add Continue =
    block $ add ++ [Continue]
beforeContinue add stmt =
    traverseSinglyNestedStmts (beforeContinue add) stmt

-- reversible pattern to unwrap in for loops
pattern AsgnStmt :: LHS -> Expr -> Stmt
pattern AsgnStmt lhs expr = Asgn AsgnOpEq Nothing lhs expr

toInit :: Stmt -> (LHS, Expr)
toInit stmt = (lhs, expr)
    where AsgnStmt lhs expr = stmt

-- functions which convert and collect assignment expressions
type Converter t = t -> Writer [Stmt] t

convertExpr :: Converter Expr
convertExpr (ExprAsgn l r) = do
    l' <- convertExpr l
    r' <- convertExpr r
    let Just lhs = exprToLHS l' -- checked by parser
    tell [AsgnStmt lhs r']
    return l'
convertExpr expr =
    traverseSinglyNestedExprsM convertExpr expr

convertLHS :: Converter LHS
convertLHS = traverseNestedLHSsM $ traverseLHSExprsM convertExpr

convertInit :: Converter (LHS, Expr)
convertInit = bimapM convertLHS convertExpr

convertIncr :: Converter (LHS, AsgnOp, Expr)
convertIncr (lhs, op, expr) = do
    lhs' <- convertLHS lhs
    expr' <- convertExpr expr
    return (lhs', op, expr')
