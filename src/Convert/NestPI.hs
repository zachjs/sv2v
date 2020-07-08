{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for moving top-level package items into modules
 -}

module Convert.NestPI (convert) where

import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type PIs = Map.Map Identifier PackageItem
type Idents = Set.Set Identifier

convert :: [AST] -> [AST]
convert =
    map (filter (not . isPI)) . nest
    where
        nest :: [AST] -> [AST]
        nest = traverseFiles
            (collectDescriptionsM collectDescriptionM)
            (traverseDescriptions . convertDescription)
        isPI :: Description -> Bool
        isPI (PackageItem Import{}) = False
        isPI (PackageItem item) = piName item /= ""
        isPI _ = False

-- collects packages items missing
collectDescriptionM :: Description -> Writer PIs ()
collectDescriptionM (PackageItem item) = do
    case piName item of
        "" -> return ()
        ident -> tell $ Map.singleton ident item
collectDescriptionM _ = return ()

-- nests packages items missing from modules
convertDescription :: PIs -> Description -> Description
convertDescription pis (orig @ Part{}) =
    if Map.null pis
        then orig
        else Part attrs extern kw lifetime name ports items'
    where
        Part attrs extern kw lifetime name ports items = orig
        items' = addItems pis Set.empty items
convertDescription _ other = other

-- iteratively inserts missing package items exactly where they are needed
addItems :: PIs -> Idents -> [ModuleItem] -> [ModuleItem]
addItems pis existingPIs (item : items) =
    if not $ Set.disjoint existingPIs thisPI then
        -- this item was re-imported earlier in the module
        addItems pis existingPIs items
    else if null itemsToAdd then
        -- this item has no additional dependencies
        item : addItems pis (Set.union existingPIs thisPI) items
    else
        -- this item has at least one un-met dependency
        addItems pis existingPIs (head itemsToAdd : item : items)
    where
        thisPI = execWriter $ collectPIsM item
        runner f = execWriter $ collectNestedModuleItemsM f item
        usedPIs = Set.unions $ map runner
            [ collectStmtsM collectSubroutinesM
            , collectTypesM $ collectNestedTypesM collectTypenamesM
            , collectExprsM $ collectNestedExprsM collectIdentsM
            ]
        neededPIs = Set.difference (Set.difference usedPIs existingPIs) thisPI
        itemsToAdd = map MIPackageItem $ Map.elems $
            Map.restrictKeys pis neededPIs
addItems _ _ [] = []

-- writes down the names of package items
collectPIsM :: ModuleItem -> Writer Idents ()
collectPIsM (MIPackageItem item) =
    case piName item of
        "" -> return ()
        ident -> tell $ Set.singleton ident
collectPIsM _ = return ()

-- writes down the names of subroutine invocations
collectSubroutinesM :: Stmt -> Writer Idents ()
collectSubroutinesM (Subroutine (Ident f) _) = tell $ Set.singleton f
collectSubroutinesM _ = return ()

-- writes down the names of function calls and identifiers
collectIdentsM :: Expr -> Writer Idents ()
collectIdentsM (Call (Ident x) _) = tell $ Set.singleton x
collectIdentsM (Ident x)          = tell $ Set.singleton x
collectIdentsM _ = return ()

-- writes down aliased typenames
collectTypenamesM :: Type -> Writer Idents ()
collectTypenamesM (CSAlias _ _ x _) = tell $ Set.singleton x
collectTypenamesM _ = return ()

-- returns the "name" of a package item, if it has one
piName :: PackageItem -> Identifier
piName (Function _ _ ident _ _) = ident
piName (Task     _   ident _ _) = ident
piName (Typedef    _ ident    ) = ident
piName (Decl (Variable _ _ ident _ _)) = ident
piName (Decl (Param    _ _ ident   _)) = ident
piName (Decl (ParamType  _ ident   _)) = ident
piName (Decl (CommentDecl          _)) = ""
piName (Import x y) = show $ Import x y
piName (Export    _) = ""
piName (Directive _) = ""
