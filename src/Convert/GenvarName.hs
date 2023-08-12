{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Assign unique names to `genvar`s to avoid conflicts within explicitly-scoped
 - variables when inlining interface arrays.
 -}

module Convert.GenvarName (convert) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Scoper (replaceInExpr)
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert files = evalState
    (mapM (traverseDescriptionsM traverseDescription) files)
    (collectFiles files, mempty)

type IdentSet = Set.Set Identifier
type IdentMap = Map.Map Identifier Identifier
type SC = State (IdentSet, IdentMap)

-- get all of the seemingly sv2v-generated genvar names already present anywhere
-- in the sources so we can avoid generating new ones that conflict with them
collectFiles :: [AST] -> IdentSet
collectFiles = execWriter . mapM (collectDescriptionsM collectDescription)

collectDescription :: Description -> Writer IdentSet ()
collectDescription = collectModuleItemsM collectModuleItem

collectModuleItem :: ModuleItem -> Writer IdentSet ()
collectModuleItem (Genvar ident) =
    when (isGeneratedName ident) $ tell (Set.singleton ident)
collectModuleItem _ = return ()

traverseDescription :: Description -> SC Description
traverseDescription (Part att ext kw lif name ports items) =
    mapM traverseModuleItem items <&> Part att ext kw lif name ports
traverseDescription description = return description

traverseModuleItem :: ModuleItem -> SC ModuleItem
traverseModuleItem (Genvar ident) =
    renameGenvar ident <&> Genvar
traverseModuleItem (Generate genItems) =
    mapM traverseGenItem genItems <&> Generate
traverseModuleItem (MIAttr attr item) =
    traverseModuleItem item <&> MIAttr attr
traverseModuleItem item = return item

traverseGenItem :: GenItem -> SC GenItem
traverseGenItem (GenFor start@(index, _) cond incr item)
    | not (isGeneratedName index) = do
    index' <- gets $ (Map.! index) . snd
    item' <- traverseGenItem item
    return $ if index == index'
        then GenFor start cond incr item'
        else renameInLoop start cond incr index' item'
traverseGenItem (GenBlock blk items) = do
    priorMapping <- gets snd
    items' <- mapM traverseGenItem items
    -- keep all assigned names, but prefer names from the outer scope
    modify' $ (, priorMapping) . fst
    return $ GenBlock blk items'
traverseGenItem (GenModuleItem item) =
    traverseModuleItem item <&> GenModuleItem
traverseGenItem item =
    traverseSinglyNestedGenItemsM traverseGenItem item

-- rename all usages of the genvar in the initialization, guard, and
-- incrementation of a generate for loop
renameInLoop :: (Identifier, Expr) -> Expr -> (Identifier, AsgnOp, Expr)
    -> Identifier -> GenItem -> GenItem
renameInLoop (index, start) cond (dest, op, next) index' =
    GenFor (index', start') cond' (dest', op, next') . prependGenItem decl
    where
        expr = Ident index'
        replacements = Map.singleton index expr
        start' = replaceInExpr replacements start
        cond' = replaceInExpr replacements cond
        next' = replaceInExpr replacements next
        dest' = if dest == index then index' else dest
        decl = GenModuleItem $ MIPackageItem $ Decl $
                Param Localparam UnknownType index expr

-- add an item to the beginning of the given generate block
prependGenItem :: GenItem -> GenItem -> GenItem
prependGenItem item block = GenBlock blk $ item : items
    where GenBlock blk items = block

prefixIntf :: Identifier
prefixIntf = "_arr_"
prefixUniq :: Identifier
prefixUniq = "_gv_"

isGeneratedName :: Identifier -> Bool
isGeneratedName ident =
    isPrefixOf prefixIntf ident ||
    isPrefixOf prefixUniq ident

-- generate and record a unique name for the given genvar
renameGenvar :: Identifier -> SC Identifier
renameGenvar ident | isGeneratedName ident = return ident
renameGenvar ident = do
    idents <- gets fst
    let ident' = uniqueGenvarName idents prefix 1
    modify' $ (<>) (Set.singleton ident', Map.singleton ident ident')
    return ident'
    where prefix = prefixUniq ++ ident ++ "_"

-- increment the counter until it produces a unique identifier
uniqueGenvarName :: IdentSet -> Identifier -> Int -> Identifier
uniqueGenvarName idents prefix = step
    where
        step :: Int -> Identifier
        step counter =
            if Set.member candidate idents
                then step $ counter + 1
                else candidate
            where candidate = prefix ++ show counter
