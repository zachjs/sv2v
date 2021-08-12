{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Verilog-2005 forbids block declarations with default values. We convert
 - these assignments to separate statements. If we handle static lifetimes in
 - the future, this conversion may have to change.
 -}

module Convert.BlockDecl (convert) where

import Data.Maybe (mapMaybe)

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $ traverseDescriptions $ traverseModuleItems
    (convertModuleItem . traverseStmts (traverseNestedStmts convertStmt))

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem (MIPackageItem (Function ml t f decls stmts)) =
    MIPackageItem $ Function ml t f decls' stmts'
    where
        Block Seq "" decls' stmts' = convertStmt $
            Block Seq "" decls stmts
convertModuleItem (MIPackageItem (Task ml f decls stmts)) =
    MIPackageItem $ Task ml f decls' stmts'
    where
        Block Seq "" decls' stmts' = convertStmt $
            Block Seq "" decls stmts
convertModuleItem other = other

convertStmt :: Stmt -> Stmt
convertStmt (Block Seq name decls stmts) =
    Block Seq name decls' stmts'
    where
        splitDecls = map splitDecl decls
        decls' = map fst splitDecls
        asgns = map asgnStmt $ mapMaybe snd splitDecls
        stmts' = asgns ++ stmts
convertStmt other = other

splitDecl :: Decl -> (Decl, Maybe (LHS, Expr))
splitDecl decl@(Variable _ _ _ _ Nil) =
    (decl, Nothing)
splitDecl (Variable d t ident a e) =
    (Variable d t ident a Nil, Just (LHSIdent ident, e))
splitDecl decl = (decl, Nothing)

asgnStmt :: (LHS, Expr) -> Stmt
asgnStmt = uncurry $ Asgn AsgnOpEq Nothing
