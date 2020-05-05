{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for tasks and functions to contain only one top-level statement,
 - as required in Verilog-2005. This conversion also hoists data declarations to
 - the task or function level for greater portability.
 -}

module Convert.TFBlock (convert) where

import Data.List (isPrefixOf)

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
    Function ml t f decls' stmts'
    where (decls', stmts') = convertTFBlock decls stmts
convertPackageItem (Task     ml   f decls stmts) =
    Task     ml   f decls' stmts'
    where (decls', stmts') = convertTFBlock decls stmts
convertPackageItem other = other

convertTFBlock :: [Decl] -> [Stmt] -> ([Decl], [Stmt])
convertTFBlock decls stmts =
    (decls', [stmtsToStmt stmts'])
    where (decls', stmts') = flattenOuterBlocks $ Block Seq "" decls stmts

stmtsToStmt :: [Stmt] -> Stmt
stmtsToStmt [stmt] = stmt
stmtsToStmt stmts = Block Seq "" [] stmts

flattenOuterBlocks :: Stmt -> ([Decl], [Stmt])
flattenOuterBlocks (Block Seq "" declsA [stmt]) =
    (declsA ++ declsB, stmtsB)
    where (declsB, stmtsB) = flattenOuterBlocks stmt
flattenOuterBlocks (Block Seq "" declsA (Block Seq name declsB stmtsA : stmtsB)) =
    flattenOuterBlocks $ Block Seq name (declsA ++ declsB) (stmtsA ++ stmtsB)
flattenOuterBlocks (Block Seq name decls stmts)
    | notscope name = (decls, stmts)
    | otherwise = ([], [Block Seq name decls stmts])
flattenOuterBlocks stmt = ([], [stmt])

notscope :: Identifier -> Bool
notscope "" = True
notscope name = "sv2v_autoblock_" `isPrefixOf` name
