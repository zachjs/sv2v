{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for moving top-level package items into modules
 -}

module Convert.NestPI (convert, reorder) where

import Control.Monad.Writer.Strict
import Data.Maybe (mapMaybe)
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

reorder :: [AST] -> [AST]
reorder = map $ traverseDescriptions reorderDescription

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
        items' = addItems pis Set.empty (map addUsedPIs items)
convertDescription _ other = other

-- attempt to fix simple declaration order issues
reorderDescription :: Description -> Description
reorderDescription (Part attrs extern kw lifetime name ports items) =
    Part attrs extern kw lifetime name ports items'
    where
        items' = addItems localPIs Set.empty (map addUsedPIs items)
        localPIs = Map.fromList $ mapMaybe toPIElem items
        toPIElem :: ModuleItem -> Maybe (Identifier, PackageItem)
        toPIElem (MIPackageItem item) = Just (piName item, item)
        toPIElem _ = Nothing
reorderDescription other = other

-- iteratively inserts missing package items exactly where they are needed
addItems :: PIs -> Idents -> [(ModuleItem, Idents)] -> [ModuleItem]
addItems pis existingPIs ((item, usedPIs) : items) =
    if not $ Set.disjoint existingPIs thisPI then
        -- this item was re-imported earlier in the module
        addItems pis existingPIs items
    else if null itemsToAdd then
        -- this item has no additional dependencies
        item : addItems pis (Set.union existingPIs thisPI) items
    else
        -- this item has at least one un-met dependency
        addItems pis existingPIs (addUsedPIs chosen : (item, usedPIs) : items)
    where
        thisPI = execWriter $ collectPIsM item
        neededPIs = Set.difference (Set.difference usedPIs existingPIs) thisPI
        itemsToAdd = map MIPackageItem $ Map.elems $
            Map.restrictKeys pis neededPIs
        chosen = head itemsToAdd
addItems _ _ [] = []

-- augment a module item with the set of identifiers it uses
addUsedPIs :: ModuleItem -> (ModuleItem, Idents)
addUsedPIs item =
    (item, usedPIs)
    where
        usedPIs = execWriter $
            traverseNestedModuleItemsM (traverseIdentsM writeIdent) item
        writeIdent :: Identifier -> Writer Idents Identifier
        writeIdent x = tell (Set.singleton x) >> return x

-- writes down the names of package items
collectPIsM :: ModuleItem -> Writer Idents ()
collectPIsM (MIPackageItem item) =
    case piName item of
        "" -> return ()
        ident -> tell $ Set.singleton ident
collectPIsM _ = return ()

-- visits all identifiers in a module item
traverseIdentsM :: Monad m => MapperM m Identifier -> MapperM m ModuleItem
traverseIdentsM identMapper = traverseNodesM
    (traverseExprIdentsM identMapper)
    (traverseDeclIdentsM identMapper)
    (traverseTypeIdentsM identMapper)
    (traverseLHSIdentsM  identMapper)
    (traverseStmtIdentsM identMapper)

-- visits all identifiers in an expression
traverseExprIdentsM :: Monad m => MapperM m Identifier -> MapperM m Expr
traverseExprIdentsM identMapper = fullMapper
    where
        fullMapper = exprMapper >=> traverseSinglyNestedExprsM fullMapper
        exprMapper (Call (Ident x) args) =
            identMapper x >>= \x' -> return $ Call (Ident x') args
        exprMapper (Ident x) = identMapper x >>= return . Ident
        exprMapper other = return other

-- visits all identifiers in a type
traverseTypeIdentsM :: Monad m => MapperM m Identifier -> MapperM m Type
traverseTypeIdentsM identMapper = fullMapper
    where
        fullMapper = typeMapper
            >=> traverseTypeExprsM (traverseExprIdentsM identMapper)
            >=> traverseSinglyNestedTypesM fullMapper
        typeMapper (Alias       x t) = aliasHelper (Alias      ) x t
        typeMapper (PSAlias p   x t) = aliasHelper (PSAlias p  ) x t
        typeMapper (CSAlias c p x t) = aliasHelper (CSAlias c p) x t
        typeMapper other = return other
        aliasHelper constructor x t =
            identMapper x >>= \x' -> return $ constructor x' t

-- visits all identifiers in an LHS
traverseLHSIdentsM :: Monad m => MapperM m Identifier -> MapperM m LHS
traverseLHSIdentsM identMapper = fullMapper
    where
        fullMapper = lhsMapper
            >=> traverseLHSExprsM (traverseExprIdentsM identMapper)
            >=> traverseSinglyNestedLHSsM fullMapper
        lhsMapper (LHSIdent x) = identMapper x >>= return . LHSIdent
        lhsMapper other = return other

-- visits all identifiers in a statement
traverseStmtIdentsM :: Monad m => MapperM m Identifier -> MapperM m Stmt
traverseStmtIdentsM identMapper = fullMapper
    where
        fullMapper = stmtMapper
            >=> traverseStmtExprsM (traverseExprIdentsM identMapper)
            >=> traverseStmtLHSsM  (traverseLHSIdentsM  identMapper)
            >=> traverseSinglyNestedStmtsM fullMapper
        stmtMapper (Subroutine (Ident x) args) =
            identMapper x >>= \x' -> return $ Subroutine (Ident x') args
        stmtMapper other = return other

-- visits all identifiers in a declaration
traverseDeclIdentsM :: Monad m => MapperM m Identifier -> MapperM m Decl
traverseDeclIdentsM identMapper =
    traverseDeclExprsM (traverseExprIdentsM identMapper) >=>
    traverseDeclTypesM (traverseTypeIdentsM identMapper)

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
