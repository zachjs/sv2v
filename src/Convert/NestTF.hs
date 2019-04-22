{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for moving top-level tasks and functions into modules
 -}

module Convert.NestTF (convert) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type TFs = Map.Map Identifier PackageItem
type Idents = Set.Set Identifier

convert :: AST -> AST
convert ast =
    filter (not . isTF) $ nest $ ast
    where
        nest :: AST -> AST
        nest curr =
            if next == curr
                then curr
                else nest next
            where
                next = evalState (traverseM curr) Map.empty
                traverseM = traverseDescriptionsM traverseDescriptionM
        isTF :: Description -> Bool
        isTF (PackageItem (Function _ _ _ _ _)) = True
        isTF (PackageItem (Task     _   _ _ _)) = True
        isTF _ = False

-- collects and nests in tasks and functions missing from modules
traverseDescriptionM :: Description -> State TFs Description
traverseDescriptionM (PackageItem item) = do
    () <- case item of
        Function _ _ ident _ _ -> modify $ Map.insert ident item
        Task     _   ident _ _ -> modify $ Map.insert ident item
        _ -> return ()
    return $ PackageItem item
traverseDescriptionM (orig @ (Part extern kw lifetime name ports items)) = do
    tfs <- get
    let newItems = map MIPackageItem $ Map.elems $
            Map.restrictKeys tfs neededTFs
    return $ Part extern kw lifetime name ports (items ++ newItems)
    where
        existingTFs = execWriter $ collectModuleItemsM collectTFsM orig
        usedTFs = Set.union
            (execWriter $ collectModuleItemsM (collectStmtsM collectSubroutinesM) orig)
            (execWriter $ collectModuleItemsM (collectExprsM $ collectNestedExprsM collectCallsM) orig)
        neededTFs = Set.difference usedTFs existingTFs
traverseDescriptionM other = return other

-- writes down the names of tasks and functions
collectTFsM :: ModuleItem -> Writer Idents ()
collectTFsM (MIPackageItem item) =
    case item of
        Function _ _ ident _ _ -> tell $ Set.singleton ident
        Task     _   ident _ _ -> tell $ Set.singleton ident
        _ -> return ()
collectTFsM _ = return ()

-- writes down the names of subroutine invocations
collectSubroutinesM :: Stmt -> Writer Idents ()
collectSubroutinesM (Subroutine f _) = tell $ Set.singleton f
collectSubroutinesM _ = return ()

-- writes down the names of function calls
collectCallsM :: Expr -> Writer Idents ()
collectCallsM (Call f _) = tell $ Set.singleton f
collectCallsM _ = return ()
