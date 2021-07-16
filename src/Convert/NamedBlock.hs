{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for unnamed blocks with contain data declarations
 -
 - SystemVerilog allows data declarations to appear in all blocks, but Verilog
 - allows them to appear only in blocks that are named. This conversion gives
 - such blocks a unique name to placate strict Verilog frontends.
 -}

module Convert.NamedBlock (convert) where

import Control.Monad.State.Strict

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription description =
    evalState (traverseModuleItemsM traverseModuleItem description) 1
    where
        traverseModuleItem = traverseStmtsM $ traverseNestedStmtsM traverseStmtM

traverseStmtM :: Stmt -> State Int Stmt
traverseStmtM (Block kw "" [] stmts) =
    return $ Block kw "" [] stmts
traverseStmtM (Block kw "" decls stmts) = do
    x <- uniqueBlockName
    return $ Block kw x decls stmts
traverseStmtM other = return other

uniqueBlockName :: State Int String
uniqueBlockName = do
    cnt <- get
    put $ cnt + 1
    return $ "sv2v_autoblock_" ++ show cnt
