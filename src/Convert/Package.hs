{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for packages, exports, and imports
 -
 - TODO: We do not yet handle exports.
 - TODO: The scoping rules are not being entirely followed yet.
 - TODO: Explicit imports may introduce name conflicts because of carried items.
 -
 - The SystemVerilog scoping rules for exports and imports are not entirely
 - trivial. We do not explicitly handle the "error" scenarios detailed Table
 - 26-1 of Section 26-3 of IEEE 1800-2017. Users generally shouldn't be relying
 - on this tool to catch and report such wild naming conflicts that are outlined
 - there.
 -
 - Summary:
 - * In scopes which have a local declaration of an identifier, that identifier
 -   refers to that local declaration.
 - * If there is no local declaration, the identifier refers to the imported
 -   declaration.
 - * If there is an explicit import of that identifier, the identifier refers to
 -   the imported declaration.
 - * Usages of conflicting wildcard imports are not allowed.
 -}

module Convert.Package (convert) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type Packages = Map.Map Identifier PackageItems
type PackageItems = [(Identifier, PackageItem)]
type Idents = Set.Set Identifier

convert :: [AST] -> [AST]
convert = step
    where
        step :: [AST] -> [AST]
        step curr =
            if next == curr
                then curr
                else step next
            where
                next = traverseFiles
                    (collectDescriptionsM collectDescriptionM)
                    convertFile curr

convertFile :: Packages -> AST -> AST
convertFile packages ast =
    (++) globalItems $
    filter (not . isCollected) $
    concatMap (traverseDescription packages) $
    ast
    where
        globalItems = map PackageItem $
             concatMap (uncurry globalPackageItems) $ Map.toList packages
        isCollected :: Description -> Bool
        isCollected (Package _ name _) = Map.member name packages
        isCollected _ = False

globalPackageItems :: Identifier -> PackageItems -> [PackageItem]
globalPackageItems name items =
    prefixPackageItems name (packageItemIdents items) (map snd items)

packageItemIdents :: PackageItems -> Idents
packageItemIdents items =
    Set.union
        (Set.fromList $ map fst items)
        (Set.unions $ map (packageItemSubIdents . snd) items)
    where
        packageItemSubIdents :: PackageItem -> Idents
        packageItemSubIdents (Typedef (Enum _ enumItems _) _) =
            Set.fromList $ map fst enumItems
        packageItemSubIdents _ = Set.empty

prefixPackageItems :: Identifier -> Idents -> [PackageItem] -> [PackageItem]
prefixPackageItems packageName idents items =
    map unwrap $ evalScoper
    traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM
    packageName $ map (wrap . initialPrefix) items
    where
        wrap :: PackageItem -> ModuleItem
        wrap = MIPackageItem
        unwrap :: ModuleItem -> PackageItem
        unwrap (MIPackageItem item) = item
        unwrap _ = error "unwrap invariant violated"

        initialPrefix :: PackageItem -> PackageItem
        initialPrefix item =
            case item of
                Function       a b x c d  -> Function       a b (prefix x) c d
                Task           a   x c d  -> Task           a   (prefix x) c d
                Typedef          a x      -> Typedef          a (prefix x)
                Decl (Variable a b x c d) -> Decl (Variable a b (prefix x) c d)
                Decl (Param    a b x c  ) -> Decl (Param    a b (prefix x) c  )
                Decl (ParamType  a x b  ) -> Decl (ParamType  a (prefix x) b  )
                other -> other

        prefix :: Identifier -> Identifier
        prefix x =
            if Set.member x idents
                then packageName ++ '_' : x
                else x
        prefixM :: Identifier -> Scoper () Identifier
        prefixM x = do
            details <- lookupElemM x
            if details == Nothing
                then return $ prefix x
                else return x

        traverseDeclM :: Decl -> Scoper () Decl
        traverseDeclM decl = do
            case decl of
                Variable _ _ x _ _ -> insertElem x ()
                Param _ _ x _      -> insertElem x ()
                ParamType _ x _    -> insertElem x ()
                CommentDecl{} -> return ()
            traverseDeclTypesM traverseTypeM decl >>=
                traverseDeclExprsM traverseExprM

        traverseTypeM :: Type -> Scoper () Type
        traverseTypeM (Alias x rs) =
            prefixM x >>= \x' -> return $ Alias x' rs
        traverseTypeM (Enum t enumItems rs) = do
            enumItems' <- mapM prefixEnumItem enumItems
            return $ Enum t enumItems' rs
            where  prefixEnumItem (x, e) = prefixM x >>= \x' -> return (x', e)
        traverseTypeM other = traverseSinglyNestedTypesM traverseTypeM other

        traverseExprM (Ident x) = prefixM x >>= return . Ident
        traverseExprM other = traverseSinglyNestedExprsM traverseExprM other
        traverseLHSM (LHSIdent x) = prefixM x >>= return . LHSIdent
        traverseLHSM other = traverseSinglyNestedLHSsM traverseLHSM other

        traverseGenItemM = error "not possible"
        traverseModuleItemM =
            traverseTypesM traverseTypeM >=>
            traverseExprsM traverseExprM >=>
            traverseLHSsM  traverseLHSM
        traverseStmtM =
            traverseStmtExprsM traverseExprM >=>
            traverseStmtLHSsM  traverseLHSM

collectDescriptionM :: Description -> Writer Packages ()
collectDescriptionM (Package _ name items) =
    if any isImport items
        then return ()
        else tell $ Map.singleton name itemList
    where
        itemList = concatMap toPackageItems items
        toPackageItems :: PackageItem -> PackageItems
        toPackageItems item =
            case piName item of
                "" -> []
                x -> [(x, item)]
        isImport :: PackageItem -> Bool
        isImport (Import _ _) = True
        isImport _ = False
collectDescriptionM _ = return ()

traverseDescription :: Packages -> Description -> [Description]
traverseDescription packages (PackageItem (Import x y)) =
    map (\(MIPackageItem item) -> PackageItem item) items
    where
        orig = Part [] False Module Inherit "DNE" []
            [MIPackageItem $ Import x y]
        [orig'] = traverseDescription packages orig
        Part [] False Module Inherit "DNE" [] items = orig'
traverseDescription packages description =
    [description']
    where
        description' = traverseModuleItems
            (traverseModuleItem existingItemNames packages)
            description
        existingItemNames = execWriter $
            collectModuleItemsM writePIName description
        writePIName :: ModuleItem -> Writer Idents ()
        writePIName (MIPackageItem (Import _ (Just x))) =
            tell $ Set.singleton x
        writePIName (MIPackageItem item) =
            case piName item of
                "" -> return ()
                x -> tell $ Set.singleton x
        writePIName _ = return ()

traverseModuleItem :: Idents -> Packages -> ModuleItem -> ModuleItem
traverseModuleItem existingItemNames packages (MIPackageItem (Import x y)) =
    if Map.member x packages
        then Generate $ map (GenModuleItem . MIPackageItem) itemsRenamed
        else MIPackageItem $ Import x y
    where
        packageItems = packages Map.! x
        namesToAvoid = case y of
            Nothing -> existingItemNames
            Just ident -> Set.delete ident existingItemNames
        itemsRenamed =
            prefixPackageItems x namesToAvoid
            (map snd packageItems)
traverseModuleItem _ _ item =
    (traverseExprs $ traverseNestedExprs traverseExpr) $
    (traverseTypes $ traverseNestedTypes traverseType) $
    item
    where

        traverseExpr :: Expr -> Expr
        traverseExpr (PSIdent x y) = Ident $ x ++ "_" ++ y
        traverseExpr other = other

        traverseType :: Type -> Type
        traverseType (PSAlias ps xx rs) = Alias (ps ++ "_" ++ xx) rs
        traverseType other = other

-- returns the "name" of a package item, if it has one
piName :: PackageItem -> Identifier
piName (Function _ _ ident _ _) = ident
piName (Task     _   ident _ _) = ident
piName (Typedef    _ ident    ) = ident
piName (Decl (Variable _ _ ident _ _)) = ident
piName (Decl (Param    _ _ ident   _)) = ident
piName (Decl (ParamType  _ ident   _)) = ident
piName (Decl (CommentDecl          _)) = ""
piName (Import  _ _) = ""
piName (Export    _) = ""
piName (Directive _) = ""
