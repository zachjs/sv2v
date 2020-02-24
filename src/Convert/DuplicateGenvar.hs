{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion to remove duplicate genvar declarations
 -}

module Convert.DuplicateGenvar (convert) where

import Control.Monad.State
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type IdentSet = Set.Set Identifier

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions traverseDescription

traverseDescription :: Description -> Description
traverseDescription (part @ Part{}) =
    Part attrs extern kw lifetime name ports items'
    where
        Part attrs extern kw lifetime name ports items = part
        -- intentionally only looks at top level module items
        items' = evalState (mapM traverseModuleItemM items) Set.empty
traverseDescription other = other

traverseModuleItemM :: ModuleItem -> State IdentSet ModuleItem
traverseModuleItemM (Genvar x) = do
    alreadyExists <- gets $ Set.member x
    if alreadyExists
        then return $ Generate []
        else do
            modify $ Set.insert x
            return $ Genvar x
traverseModuleItemM (Generate items) = do
    s <- get
    items' <- traverseItems items
    s' <- get
    items'' <- traverseItems items'
    let genvarDecls = map (GenModuleItem . Genvar) $
            Set.toList $ Set.difference s' s
    return $ Generate (genvarDecls ++ items'')
    where traverseItems = mapM $ traverseNestedGenItemsM traverseGenItemM
traverseModuleItemM other = return other

traverseGenItemM :: GenItem -> State IdentSet GenItem
traverseGenItemM (GenModuleItem item) = do
    item' <- traverseModuleItemM item
    return $ GenModuleItem item'
traverseGenItemM other = return other
