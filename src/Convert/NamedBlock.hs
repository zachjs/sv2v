{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for unnamed blocks with contain data declarations
 -}

module Convert.NamedBlock (convert) where

import Control.Monad.State
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type Idents = Set.Set Identifier

convert :: AST -> AST
convert ast =
    -- we collect all the existing blocks in the first pass to make sure we
    -- don't generate conflicting names on repeated passes of this conversion
    evalState (runner collectStmtM ast >>= runner traverseStmtM) Set.empty
    where runner = traverseDescriptionsM . traverseModuleItemsM . traverseStmtsM

collectStmtM :: Stmt -> State Idents Stmt
collectStmtM (Block (Just x) decls stmts) = do
    modify $ Set.insert x
    return $ Block (Just x) decls stmts
collectStmtM other = return other

traverseStmtM :: Stmt -> State Idents Stmt
traverseStmtM (Block Nothing [] stmts) =
    return $ Block Nothing [] stmts
traverseStmtM (Block Nothing decls stmts) = do
    names <- get
    let x = uniqueBlockName names
    modify $ Set.insert x
    return $ Block (Just x) decls stmts
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
