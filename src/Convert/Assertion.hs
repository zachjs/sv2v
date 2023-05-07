{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for removing assertions. Assertions items are "commented out."
 -}

module Convert.Assertion (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems convertModuleItem

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem item@AssertionItem{} =
    Generate $ map toItem comments
    where
        toItem = GenModuleItem . MIPackageItem . Decl . CommentDecl
        comments = "removed an assertion item" : (lines $ show item)
convertModuleItem other =
    traverseStmts (traverseNestedStmts convertStmt) other

convertStmt :: Stmt -> Stmt
convertStmt (Assertion _) = Null
convertStmt other = other
