{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion which simply removes assertions
 -}

module Convert.Assertion (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert = traverseDescriptions $ traverseModuleItems convertModuleItem

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem (AssertionItem _) =
    MIPackageItem $ Comment "removed an assertion item"
convertModuleItem other = traverseStmts convertStmt other

convertStmt :: Stmt -> Stmt
convertStmt (Assertion _) = Null
convertStmt other = other
