{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for tasks and functions to use only one statement, as required in
 - Verilog-2005.
 -}

module Convert.StmtBlock (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems convertModuleItem

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem (MIPackageItem packageItem) =
    MIPackageItem $ convertPackageItem packageItem
convertModuleItem other = other

convertPackageItem :: PackageItem -> PackageItem
convertPackageItem (Function ml t f decls stmts) =
    Function ml t f decls [stmtsToStmt stmts]
convertPackageItem (Task     ml   f decls stmts) =
    Task     ml   f decls [stmtsToStmt stmts]
convertPackageItem other = other

stmtsToStmt :: [Stmt] -> Stmt
stmtsToStmt [stmt] = stmt
stmtsToStmt stmts = Block Seq "" [] stmts
