{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for removing any comments
 -}

module Convert.RemoveComments (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map convertFile

convertFile :: AST -> AST
convertFile =
    traverseDescriptions (traverseModuleItems convertModuleItem) .
    filter (not . isTopLevelComment)

isTopLevelComment :: Description -> Bool
isTopLevelComment (PackageItem (Decl CommentDecl{})) = True
isTopLevelComment _ = False

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem (MIPackageItem (Decl CommentDecl{})) = Generate []
convertModuleItem (MIPackageItem item) =
    MIPackageItem $ convertPackageItem item
convertModuleItem other =
    traverseStmts (traverseNestedStmts convertStmt) other

convertPackageItem :: PackageItem -> PackageItem
convertPackageItem (Function l t x decls stmts) =
    Function l t x decls' stmts'
    where
        decls' = convertDecls decls
        stmts' = convertStmts stmts
convertPackageItem (Task     l   x decls stmts) =
    Task     l   x decls' stmts'
    where
        decls' = convertDecls decls
        stmts' = convertStmts stmts
convertPackageItem (DPIImport spec prop alias typ name decls) =
    DPIImport spec prop alias typ name decls'
    where decls' = convertDecls decls
convertPackageItem other = other

convertStmt :: Stmt -> Stmt
convertStmt (CommentStmt _) = Null
convertStmt (Block kw name decls stmts) =
    Block kw name decls' stmts'
    where
        decls' = convertDecls decls
        stmts' = filter (/= Null) stmts
convertStmt other = other

convertDecls :: [Decl] -> [Decl]
convertDecls = filter (not . isCommentDecl)
    where
        isCommentDecl :: Decl -> Bool
        isCommentDecl CommentDecl{} = True
        isCommentDecl _ = False

convertStmts :: [Stmt] -> [Stmt]
convertStmts = map $ traverseNestedStmts convertStmt
