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
convertFunction (MIPackageItem (Function ml t f decls stmts)) =
    MIPackageItem $ Function ml t f decls $
    map (traverseNestedStmts convertStmt) stmts
    where
        convertStmt :: Stmt -> Stmt
        convertStmt (Return e) = AsgnBlk AsgnOpEq (LHSIdent f) e
        convertStmt other = other
convertFunction other = other
