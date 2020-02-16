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
        nest curr =
            if next == curr
                then curr
                else nest next
            where
                next = traverseFiles
                    (collectDescriptionsM collectDescriptionM)
                    (traverseDescriptions . convertDescription)
                    curr
        isPI :: Description -> Bool
        isPI (PackageItem Import{}) = False
        isPI (PackageItem item) = piName item /= Nothing
        isPI _ = False

-- collects packages items missing
collectDescriptionM :: Description -> Writer PIs ()
collectDescriptionM (PackageItem item) = do
    case piName item of
        Nothing -> return ()
        Just ident -> tell $ Map.singleton ident item
collectDescriptionM _ = return ()

-- nests packages items missing from modules
convertDescription :: PIs -> Description -> Description
convertDescription pis (orig @ Part{}) =
    Part attrs extern kw lifetime name ports items'
    where
        Part attrs extern kw lifetime name ports items = orig
        existingPIs = execWriter $ collectModuleItemsM collectPIsM orig
        runner f = execWriter $ collectModuleItemsM f orig
        usedPIs = Set.unions $ map runner $
            [ collectStmtsM collectSubroutinesM
            , collectTypesM $ collectNestedTypesM collectTypenamesM
            , collectExprsM $ collectNestedExprsM collectIdentsM
            ]
        neededPIs = Set.difference usedPIs existingPIs
        newItems = map MIPackageItem $ Map.elems $
            Map.restrictKeys pis neededPIs
        -- place data declarations at the beginning to obey declaration
        -- ordering; everything else can go at the end
        newItemsBefore = filter isDecl newItems
        newItemsAfter = filter (not . isDecl) newItems
        items' = newItemsBefore ++ items ++ newItemsAfter
        isDecl (MIPackageItem (Decl{})) = True
        isDecl _ = False
convertDescription _ other = other

-- writes down the names of package items
collectPIsM :: ModuleItem -> Writer Idents ()
collectPIsM (MIPackageItem item) =
    case piName item of
        Nothing -> return ()
        Just ident -> tell $ Set.singleton ident
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
collectTypenamesM (Alias _ x _) = tell $ Set.singleton x
collectTypenamesM _ = return ()

-- returns the "name" of a package item, if it has one
piName :: PackageItem -> Maybe Identifier
piName (Function _ _ ident _ _) = Just ident
piName (Task     _   ident _ _) = Just ident
piName (Typedef    _ ident    ) = Just ident
piName (Decl (Variable _ _ ident _ _)) = Just ident
piName (Decl (Param    _ _ ident   _)) = Just ident
piName (Decl (ParamType  _ ident   _)) = Just ident
piName (Decl (CommentDecl          _)) = Nothing
piName (Import x y) = Just $ show $ Import x y
piName (Export    _) = Nothing
piName (Directive _) = Nothing
