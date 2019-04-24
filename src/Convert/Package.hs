{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for packages and imports
 -
 - TODO FIXME: This package conversion does not yet handle package-scoped
 - identifiers for task/function names or type names, as the AST and parser
 - doesn't support them yet. This won't be too difficult.
 -}

module Convert.Package (convert) where

import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type Packages = Map.Map Identifier PackageItems
type PackageItems = Map.Map Identifier PackageItem
type Idents = Set.Set Identifier

convert :: [AST] -> [AST]
convert asts =
    step asts
    where
        step :: [AST] -> [AST]
        step curr =
            if next == curr
                then curr
                else step next
            where
                packages = execWriter $
                    collectDescriptionsM collectDescriptionM $ concat curr
                globalItems = map PackageItem $
                    concatMap (uncurry globalPackageItems) $ Map.toList packages
                next = map ((++) globalItems) $ map (filter shouldntRemove) $ map
                    (traverseDescriptions $ traverseDescription packages) curr
                shouldntRemove :: Description -> Bool
                shouldntRemove (Package _ name _) = Map.notMember name packages
                shouldntRemove _ = True

globalPackageItems :: Identifier -> PackageItems -> [PackageItem]
globalPackageItems name items =
    map (prefixPackageItem name (Map.keysSet items)) (Map.elems items)

prefixPackageItem :: Identifier -> Idents -> PackageItem -> PackageItem
prefixPackageItem packageName idents item =
    item''
    where
        prefix :: Identifier -> Identifier
        prefix x =
            if Set.member x idents
                then packageName ++ "_" ++ x
                else x
        item' = case item of
            Function       a b x c d  -> Function       a b (prefix x) c d
            Task           a   x c d  -> Task           a   (prefix x) c d
            Typedef          a x      -> Typedef          a (prefix x)
            Decl (Variable a b x c d) -> Decl (Variable a b (prefix x) c d)
            Decl (Parameter  a x   b) -> Decl (Parameter  a (prefix x)   b)
            Decl (Localparam a x   b) -> Decl (Localparam a (prefix x)   b)
            other -> other
        convertExpr (Ident x) = Ident $ prefix x
        convertExpr other = other
        converter =
            (traverseExprs $ traverseNestedExprs convertExpr)
        MIPackageItem item'' = converter $ MIPackageItem item'

collectDescriptionM :: Description -> Writer Packages ()
collectDescriptionM (Package _ name items) =
    if any isImport items
        then return ()
        else tell $ Map.singleton name itemMap
    where
        itemMap = Map.unions $ map toMap items
        toMap :: PackageItem -> PackageItems
        toMap item =
            case piName item of
                Nothing -> Map.empty
                Just x -> Map.singleton x item
        isImport :: PackageItem -> Bool
        isImport (Import _ _) = True
        isImport _ = False
collectDescriptionM _ = return ()

traverseDescription :: Packages -> Description -> Description
traverseDescription packages description =
    traverseModuleItems (traverseModuleItem packages) description

traverseModuleItem :: Packages -> ModuleItem -> ModuleItem
traverseModuleItem packages (MIPackageItem (Import x y)) =
    if Map.member x packages
        then Generate $ map (GenModuleItem . MIPackageItem) items
        else MIPackageItem $ Import x y
    where
        packageItems = packages Map.! x
        filterer = case y of
                Nothing -> \_ -> True
                Just ident -> (==) ident
        items = map snd $ filter (filterer . fst) $ Map.toList packageItems
traverseModuleItem _ item =
    (traverseExprs $ traverseNestedExprs traverseExpr) $
    item

traverseExpr :: Expr -> Expr
traverseExpr (PSIdent x y) = Ident $ x ++ "_" ++ y
traverseExpr other = other

-- returns the "name" of a package item, if it has one
piName :: PackageItem -> Maybe Identifier
piName (Function _ _ ident _ _) = Just ident
piName (Task     _   ident _ _) = Just ident
piName (Typedef    _ ident    ) = Just ident
piName (Decl (Variable _ _ ident _ _)) = Just ident
piName (Decl (Parameter  _ ident   _)) = Just ident
piName (Decl (Localparam _ ident   _)) = Just ident
piName (Import _ _) = Nothing
piName (Comment  _) = Nothing
