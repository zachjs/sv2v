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
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type Idents = Set.Set Identifier

convert :: [AST] -> [AST]
convert asts =
    -- we collect all the existing blocks in the first pass to make sure we
    -- don't generate conflicting names on repeated passes of this conversion
    evalState (runner collectStmtM asts >>= runner traverseStmtM) Set.empty
    where runner = mapM . traverseDescriptionsM . traverseModuleItemsM . traverseStmtsM

collectStmtM :: Stmt -> State Idents Stmt
collectStmtM (Block kw x decls stmts) = do
    modify $ Set.insert x
    return $ Block kw x decls stmts
collectStmtM other = return other

traverseStmtM :: Stmt -> State Idents Stmt
traverseStmtM (Block kw "" [] stmts) =
    return $ Block kw "" [] stmts
traverseStmtM (Block kw "" decls stmts) = do
    names <- get
    let x = uniqueBlockName names
    modify $ Set.insert x
    return $ Block kw x decls stmts
traverseStmtM other = return other

uniqueBlockName :: Idents -> Identifier
uniqueBlockName names =
    step ("sv2v_autoblock_" ++ (show $ Set.size names)) 0
    where
        step :: Identifier -> Int -> Identifier
        step base n =
            if Set.member name names
                then step base (n + 1)
                else name
            where
                name = if n == 0
                    then base
                    else base ++ "_" ++ show n
