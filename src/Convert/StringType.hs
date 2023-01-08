{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Drop explicit `string` data type from parameters and localparams
 -}

module Convert.StringType (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems convertModuleItem

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem = traverseNodes id traverseDecl id id traverseStmt

traverseStmt :: Stmt -> Stmt
traverseStmt = traverseNestedStmts $ traverseStmtDecls traverseDecl

traverseDecl :: Decl -> Decl
traverseDecl (Param s (NonInteger TString) x e) = Param s UnknownType x e
traverseDecl other = other
