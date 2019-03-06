{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `return`
 -}

module Convert.Return (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert = traverseDescriptions $ traverseModuleItems convertFunction

convertFunction :: ModuleItem -> ModuleItem
convertFunction (Function ml t f decls stmts) =
    Function ml t f decls (map (traverseNestedStmts convertStmt) stmts)
    where
        convertStmt :: Stmt -> Stmt
        convertStmt (Return e) = AsgnBlk (LHSIdent f) e
        convertStmt other = other
convertFunction other = other
