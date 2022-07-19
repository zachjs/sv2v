{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for packages and global declarations
 -
 - This conversion first makes a best-effort pass at resolving any simple
 - declaration ordering issues in the input. Many conversions require that
 - declarations precede their first usage.
 -
 - The main phase elaborates packages and resolves imported identifiers. An
 - identifier (perhaps implicitly) referring to `P::X` is rewritten to `P_X`.
 - This conversion assumes such renaming will not cause conflicts. The full
 - semantics of imports and exports are followed.
 -
 - Finally, because Verilog doesn't allow declarations to exist outside of
 - modules, declarations within packages and in the global scope are injected
 - into modules and interfaces as needed.
 -}

module Convert.Package
    ( convert
    , inject
    , prefixItems
    ) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.List (insert, intercalate, isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.ResolveBindings (resolveBindings)
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type Packages = Map.Map Identifier Package
type Classes = Map.Map Identifier Class
type Package = (IdentStateMap, [PackageItem])
type Class = ([Decl], [PackageItem])
type Idents = Set.Set Identifier
type PIs = Map.Map Identifier PackageItem

convert :: [AST] -> [AST]
convert files =
    map (traverseDescriptions $ convertDescription pis) files'
    where
        (files', packages') = convertPackages files
        pis = Map.fromList $
            concatMap (concatMap (toPackageItems . makeLocal) . snd) $
            filter (not . Map.null . fst) $
            Map.elems packages'
        toPackageItems :: PackageItem -> [(Identifier, PackageItem)]
        toPackageItems item = map (, item) (piNames item)

-- convert a parameter to a localparam
makeLocal :: PackageItem -> PackageItem
makeLocal (Decl (Param _ t x e)) = Decl $ Param Localparam t x e
makeLocal (Decl (ParamType _ x t)) = Decl $ ParamType Localparam x t
makeLocal other = other

-- utility for inserting package items into a set of module items as needed
inject :: [PackageItem] -> [ModuleItem] -> [ModuleItem]
inject packageItems items =
    addItems localPIs Set.empty (map addUsedPIs items)
    where
        localPIs = Map.fromList $ concatMap toPIElem packageItems
        toPIElem :: PackageItem -> [(Identifier, PackageItem)]
        toPIElem item = map (, item) (piNames item)

-- utility for appling a prefix to all of the top level items in a module
prefixItems :: Identifier -> [ModuleItem] -> [ModuleItem]
prefixItems prefix items =
    snd $ evalState (processItems "" prefix items) initialState
    where initialState = PK [] Map.empty Map.empty Set.empty

-- collect packages and global package items
collectPackageM :: Description -> Writer (Packages, Classes, [PackageItem]) ()
collectPackageM (PackageItem item) =
    when (not $ null $ piNames item) $
        tell (Map.empty, Map.empty, [item])
collectPackageM (Package _ name items) =
    tell (Map.singleton name (Map.empty, items), Map.empty, [])
collectPackageM (Class _ name decls items) =
    tell (Map.empty, Map.singleton name (decls, map unpackClassItem items), [])
    where
        unpackClassItem :: ClassItem -> PackageItem
        unpackClassItem item@(_, Task{}) = checkTF item
        unpackClassItem item@(_, Function{}) = checkTF item
        unpackClassItem item = checkNonTF item
        checkTF :: ClassItem -> PackageItem
        checkTF (QStatic, item) = item
        checkTF (_, item) =
            error $ "unsupported declaration of non-static method " ++ itemName
            where [itemName] = piNames item
        checkNonTF :: ClassItem -> PackageItem
        checkNonTF (QNone, item) = item
        checkNonTF (qualifier, _) =
            error $ "unexpected qualifier " ++ show qualifier
                ++ " on a class item which is not a task or a function"
collectPackageM _ = return ()

-- elaborate all packages and their usages
convertPackages :: [AST] -> ([AST], Packages)
convertPackages files =
    (files', packages')
    where
        (files', PK [] packages' _ _) = runState op initialState
        initialState = PK [] packages classes conflicts
        op = mapM (traverseDescriptionsM traverseDescriptionM) files
        packages = Map.insert "" (Map.empty, globalItems) realPackages
        (realPackages, classes, globalItems) =
            execWriter $ mapM (collectDescriptionsM collectPackageM) files
        prefixes = Set.union (Map.keysSet classes) (Map.keysSet realPackages)
        conflicts =
            if Set.null prefixes
                then Set.empty
                else execWriter $ mapM (collectIdentConflicts prefixes) files

-- write down identifiers that might conflict with the generated names for
-- injected package items
collectIdentConflicts :: Idents -> AST -> Writer Idents ()
collectIdentConflicts prefixes =
    mapM_ $ collectModuleItemsM collectModuleItem
    where
        collectModuleItem =
            evalScoperT . scoper >=>
            collectify traverseIdentsM ident
        scoper = scopeModuleItem collectDecl return return return
        collectDecl decl = do
            case decl of
                Variable _ _ x _ _ -> lift $ ident x
                Net  _ _ _ _ x _ _ -> lift $ ident x
                Param    _ _ x   _ -> lift $ ident x
                ParamType  _ x   _ -> lift $ ident x
                CommentDecl{} -> return ()
            return decl
        ident = collectIdent prefixes

-- write down identifiers that have a package name as a prefix
collectIdent :: Idents -> Identifier -> Writer Idents ()
collectIdent prefixes ident =
    case Set.lookupLE ident prefixes of
        Just prefix -> when (prefix `isPrefixOf` ident) found
            where found = tell $ Set.singleton ident
        Nothing -> return ()

data PK = PK
    { pkStack :: [Identifier]
    , pkPackages :: Packages
    , pkClasses :: Classes
    , pkConflicts :: Idents
    }

type PackagesState = State PK

traverseDescriptionM :: Description -> PackagesState Description
traverseDescriptionM (PackageItem item) = do
    return $ PackageItem $ case piNames item of
        [] -> item
        idents -> Decl $ CommentDecl $ "removed " ++ show idents
traverseDescriptionM (Package _ name _) =
    return $ PackageItem $ Decl $ CommentDecl $ "removed package " ++ show name
traverseDescriptionM (Class _ name _ _) =
    return $ PackageItem $ Decl $ CommentDecl $ "removed class " ++ show name
traverseDescriptionM (Part attrs extern kw liftetime name ports items) = do
    (_, items') <- processItems name "" items
    return $ Part attrs extern kw liftetime name ports items'

data IdentState
    = Available [Identifier]
    | Imported Identifier
    | Declared
    deriving Eq

isImported :: IdentState -> Bool
isImported Imported{} = True
isImported _ = False

isDeclared :: IdentState -> Bool
isDeclared Declared{} = True
isDeclared _ = False

type IdentStateMap = Map.Map Identifier IdentState
type Scope = ScoperT IdentState PackagesState

-- produce the partial mapping for a particular export and ensure its validity
resolveExport
    :: IdentStateMap -> Identifier -> Identifier -> PackagesState IdentStateMap
resolveExport mapping "" "" =
    return $ Map.filter isImported mapping
resolveExport mapping pkg "" =
    fmap (Map.mapMaybeWithKey checkExport . fst) (findPackage pkg)
    where
        checkExport :: Identifier -> IdentState -> Maybe IdentState
        checkExport ident exportedIdentState =
            if localIdentState == expectedIdentState
                then localIdentState
                else Nothing
            where
                localIdentState = Map.lookup ident mapping
                expectedIdentState = Just $ Imported $
                    toRootPackage pkg exportedIdentState
resolveExport mapping pkg ident =
    case Map.lookup ident mapping of
        Just (Imported importedPkg) -> do
            exportedPkg <- resolveRootPackage pkg ident
            if importedPkg == exportedPkg
                then return $ Map.singleton ident $ Imported importedPkg
                else error $ "export of " ++ pkg ++ "::" ++ ident
                        ++ " differs from import of " ++ importedPkg
                        ++ "::" ++ ident
        _ -> error $ "export of " ++ pkg ++ "::" ++ ident
                ++ ", but " ++ ident ++ " was never imported"

-- lookup the state of the identifier only within the current scope
lookupLocalIdentState :: Identifier -> Scope (Maybe IdentState)
lookupLocalIdentState =
    fmap (fmap thd3) . lookupLocalIdentM
    where thd3 (_, _, c) = c

-- make a particular identifier within a package available for import
wildcardImport :: Identifier -> Identifier -> Scope ()
wildcardImport pkg ident = do
    rootPkg <- lift $ resolveRootPackage pkg ident
    maybeIdentState <- lookupLocalIdentState ident
    insertElem ident $
        case maybeIdentState of
            Nothing -> Available [rootPkg]
            Just Declared -> Declared
            Just (Imported existingRootPkg) -> Imported existingRootPkg
            Just (Available rootPkgs) ->
                if elem rootPkg rootPkgs
                    then Available rootPkgs
                    else Available $ insert rootPkg rootPkgs

-- make all exported identifiers within a package available for import
wildcardImports :: Identifier -> Scope ()
wildcardImports pkg = do
    (exports, _) <- lift $ findPackage pkg
    _ <- mapM (wildcardImport pkg) (Map.keys exports)
    return ()

-- resolve and store an explicit (non-wildcard) import
explicitImport :: Identifier -> Identifier -> Scope ()
explicitImport pkg ident = do
    rootPkg <- lift $ resolveRootPackage pkg ident
    maybeIdentState <- lookupLocalIdentState ident
    insertElem ident $
        case maybeIdentState of
            Nothing -> Imported rootPkg
            Just Declared ->
                error $ "import of " ++ pkg ++ "::" ++ ident
                    ++ " conflicts with prior declaration of " ++ ident
            Just Available{} -> Imported rootPkg
            Just (Imported otherPkg) ->
                if otherPkg == rootPkg
                    then Imported rootPkg
                    else error $ "import of " ++ pkg ++ "::" ++ ident
                            ++ " conflicts with prior import of "
                            ++ otherPkg ++ "::" ++ ident

-- main logic responsible for translating packages, resolving imports and
-- exports, and rewriting identifiers referring to package declarations
processItems :: Identifier -> Identifier -> [ModuleItem]
    -> PackagesState (IdentStateMap, [ModuleItem])
processItems topName packageName moduleItems = do
    (moduleItems', scopes) <- runScoperT $ scopeModuleItems scoper
        topName (reorderItems moduleItems)
    let rawIdents = extractMapping scopes
    externalIdentMaps <- mapM (resolveExportMI rawIdents) moduleItems
    let externalIdents = Map.unions externalIdentMaps
    let declaredIdents = Map.filter isDeclared rawIdents
    let exports = Map.union declaredIdents externalIdents
    let exports' = if null packageName
                    then rawIdents
                    else exports
    seq exports return (exports', moduleItems')
    where
        scoper = scopeModuleItem
            traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

        -- produces partial mappings of exported identifiers, while also
        -- checking the validity of the exports
        resolveExportMI :: IdentStateMap -> ModuleItem -> PackagesState IdentStateMap
        resolveExportMI mapping (MIPackageItem item@(Export pkg ident)) =
            if null packageName
                then error $ "invalid " ++ (init $ show item)
                        ++ " outside of package"
                else resolveExport mapping pkg ident
        resolveExportMI _ _ = return Map.empty

        -- declare an identifier, prefixing it if within a package
        prefixIdent :: Identifier -> Scope Identifier
        prefixIdent x = do
            inProcedure <- withinProcedureM
            maybeIdentState <- lookupLocalIdentState x
            case maybeIdentState of
                Just (Imported rootPkg) -> error $ "declaration of " ++ x
                    ++ " conflicts with prior import of " ++ rootPkg
                    ++ "::" ++ x
                _ -> do
                    insertElem x Declared
                    if inProcedure || null packageName
                        then return x
                        else lift $ makeIdent packageName x

        -- check the global scope for declarations or imports
        resolveGlobalIdent :: Identifier -> Scope Identifier
        resolveGlobalIdent x = do
            (exports, _) <- lift $ findPackage ""
            case Map.lookup x exports of
                Nothing -> return x
                Just identState -> do
                    -- inject the exact state outside of the module scope,
                    -- allowing wildcard imports to be handled correctly
                    insertElem [Access x Nil] identState
                    resolveIdent x

        -- remap an identifier if needed based on declarations, explicit
        -- imports, or names available for import
        resolveIdent :: Identifier -> Scope Identifier
        resolveIdent x = do
            details <- lookupElemM x
            case details of
                Nothing ->
                    -- only missing identifiers within parts should be looked up
                    -- in the global scope
                    if null packageName && not (null topName)
                        then resolveGlobalIdent x
                        else return x
                Just ([_, _], _, Declared) ->
                    if null packageName
                        then return x
                        else lift $ makeIdent packageName x
                Just (_, _, Declared) -> return x
                Just (_, _, Imported rootPkg) ->
                    lift $ makeIdent rootPkg x
                Just (accesses, _, Available [rootPkg]) -> do
                    insertElem accesses $ Imported rootPkg
                    lift $ makeIdent rootPkg x
                Just (_, _, Available rootPkgs) ->
                    error $ "identifier " ++ show x
                        ++ " ambiguously refers to the definitions in any of "
                        ++ intercalate ", " rootPkgs

        traversePackageItemM :: PackageItem -> Scope PackageItem
        traversePackageItemM orig@(Import pkg ident) = do
            if null ident
                then wildcardImports pkg
                else explicitImport pkg ident
            return $ Decl $ CommentDecl $ "removed " ++ show orig
        traversePackageItemM orig@(Export pkg ident) = do
            () <- when (not (null pkg || null ident)) $ do
                localName <- resolveIdent ident
                rootPkg <- lift $ resolveRootPackage pkg ident
                localName `seq` rootPkg `seq` return ()
            return $ Decl $ CommentDecl $ "removed " ++ show orig
        traversePackageItemM other = return other

        traverseDeclM :: Decl -> Scope Decl
        traverseDeclM decl = do
            decl' <- traverseDeclNodesM traverseTypeM traverseExprM decl
            case decl' of
                Variable d t x a e -> declHelp x $ \x' -> Variable d t x' a e
                Net  d n s t x a e -> declHelp x $ \x' -> Net  d n s t x' a e
                Param    p t x   e -> declHelp x $ \x' -> Param    p t x'   e
                ParamType  p x   t -> declHelp x $ \x' -> ParamType  p x'   t
                CommentDecl c -> return $ CommentDecl c
            where declHelp x f = prefixIdent x >>= return . f

        traverseRangeM :: Range -> Scope Range
        traverseRangeM = mapBothM traverseExprM

        traverseEnumItemM :: (Identifier, Expr) -> Scope (Identifier, Expr)
        traverseEnumItemM (x, e) = do
            x' <- prefixIdent x
            e' <- traverseExprM e
            return (x', e')

        traverseTypeM :: Type -> Scope Type
        traverseTypeM (CSAlias p b x rs) = do
            x' <- resolveCSIdent' p b x
            rs' <- mapM traverseRangeM rs
            return $ Alias x' rs'
        traverseTypeM (PSAlias p x rs) = do
            x' <- resolvePSIdent' p x
            rs' <- mapM traverseRangeM rs
            return $ Alias x' rs'
        traverseTypeM (Alias x rs) = do
            rs' <- mapM traverseRangeM rs
            x' <- resolveIdent x
            return $ Alias x' rs'
        traverseTypeM (Enum t enumItems rs) = do
            t' <- traverseTypeM t
            enumItems' <- mapM traverseEnumItemM enumItems
            rs' <- mapM traverseRangeM rs
            return $ Enum t' enumItems' rs'
        traverseTypeM other =
            traverseSinglyNestedTypesM traverseTypeM other
            >>= traverseTypeExprsM traverseExprM

        traverseExprM :: Expr -> Scope Expr
        traverseExprM (CSIdent p b x) = do
            x' <- resolveCSIdent' p b x
            return $ Ident x'
        traverseExprM (PSIdent p x) = do
            x' <- resolvePSIdent' p x
            return $ Ident x'
        traverseExprM (Ident x) = resolveIdent x >>= return . Ident
        traverseExprM other =
            traverseSinglyNestedExprsM traverseExprM other
            >>= traverseExprTypesM traverseTypeM

        traverseLHSM :: LHS -> Scope LHS
        traverseLHSM (LHSIdent x) = resolveIdent x >>= return . LHSIdent
        traverseLHSM other = traverseSinglyNestedLHSsM traverseLHSM other

        traverseGenItemM = traverseGenItemExprsM traverseExprM
        traverseModuleItemM (MIPackageItem item) = do
            item' <- traversePackageItemM item
            return $ MIPackageItem item'
        traverseModuleItemM other =
            traverseModuleItemM' other
        traverseModuleItemM' =
            traverseTypesM traverseTypeM >=>
            traverseExprsM traverseExprM >=>
            traverseLHSsM  traverseLHSM
        traverseStmtM =
            traverseStmtExprsM traverseExprM >=>
            traverseStmtLHSsM  traverseLHSM

        traverseParamBindingM :: ParamBinding -> Scope ParamBinding
        traverseParamBindingM (x, Left t) =
            traverseTypeM t >>= return . (x, ) . Left
        traverseParamBindingM (x, Right e) =
            traverseExprM e >>= return . (x, ) . Right

        -- wrapper allowing explicit reference to local package items
        resolvePSIdent' :: Identifier -> Identifier -> Scope Identifier
        resolvePSIdent' p x = do
            if p /= packageName then
                lift $ resolvePSIdent p x
            else do
                details <- lookupElemM $ Dot (Ident p) x
                case details of
                    Just ([_, _], _, Declared) ->
                        lift $ makeIdent p x
                    Just ([_, _], _, Imported rootPkg) ->
                        lift $ makeIdent rootPkg x
                    _ -> error $ "package " ++ show p ++ " references"
                            ++ " undeclared local \"" ++ p ++ "::" ++ x ++ "\""

        -- wrapper resolving parameters and locally injecting the necessary
        -- class items into modules and interfaces
        resolveCSIdent'
            :: Identifier -> [ParamBinding] -> Identifier -> Scope Identifier
        resolveCSIdent' p b x = do
            scopeKeys <- bindingsScopeKeys b
            b' <- mapM traverseParamBindingM b
            x' <- lift $ resolveCSIdent p b' scopeKeys x
            let rootPkg = take (length x' - length x - 1) x'
            when (null packageName) (classScopeInject rootPkg x')
            return x'

        -- inject the given class item and its dependencies into the local scope
        classScopeInject :: Identifier -> Identifier -> Scope ()
        classScopeInject rootPkg fullName = do
            packages <- lift $ gets pkPackages
            let (_, packageItems) = packages Map.! rootPkg
            let localPIs = Map.fromList $ concatMap toPIElem packageItems
            mapM_ injectIfMissing $
                addItems localPIs Set.empty
                [(Generate [], Set.singleton fullName)]
            where
                injectIfMissing :: ModuleItem -> Scope ()
                injectIfMissing (Generate []) = return ()
                injectIfMissing moduleItem = do
                    let MIPackageItem packageItem = moduleItem
                    let itemName : _ = piNames packageItem
                    details <- lookupElemM itemName
                    when (details == Nothing) $ do
                        accesses <- procedureLocM
                        let accesses' = accesses ++ [Access itemName Nil]
                        if null accesses
                            then insertElem itemName Declared
                            else insertElem accesses' Declared
                        injectItem moduleItem
                toPIElem :: PackageItem -> [(Identifier, PackageItem)]
                toPIElem item = map (, makeLocal item) (piNames item)

-- locate a package by name, processing its contents if necessary
findPackage :: Identifier -> PackagesState Package
findPackage packageName = do
    PK { pkStack = stack, pkPackages = packages } <- get
    let maybePackage = Map.lookup packageName packages
    assertMsg (maybePackage /= Nothing) $
        "could not find package " ++ show packageName
    -- because this conversion doesn't enforce declaration ordering of packages,
    -- it must check for dependency loops to avoid infinite recursion
    let first : rest = reverse $ packageName : stack
    assertMsg (not $ elem packageName stack) $
        "package dependency loop: " ++ show first ++ " depends on "
            ++ intercalate ", which depends on " (map show rest)
    let Just package@(exports, _) = maybePackage
    if Map.null exports
        then do
            -- process and resolve this package
            modify' $ \pk -> pk { pkStack = packageName : pkStack pk }
            package' <- processPackage packageName $ snd package
            pk <- get
            let stack' = tail $ pkStack pk
            let packages' = Map.insert packageName package' $ pkPackages pk
            put $ pk { pkStack = stack', pkPackages = packages' }
            return package'
        else return package

-- helper for elaborating a package when it is first referenced
processPackage :: Identifier -> [PackageItem] -> PackagesState Package
processPackage packageName packageItems = do
    (exports, moduleItems') <- processItems packageName packageName wrapped
    let packageItems' = map unwrap moduleItems'
    let package' = (exports, packageItems')
    return package'
    where
        wrapped = map wrap packageItems
        wrap :: PackageItem -> ModuleItem
        wrap = MIPackageItem
        unwrap :: ModuleItem -> PackageItem
        unwrap packageItem = item
            where MIPackageItem item = packageItem

-- resolve a package scoped identifier to its unique global name
resolvePSIdent :: Identifier -> Identifier -> PackagesState Identifier
resolvePSIdent packageName itemName = do
    classes <- gets pkClasses
    case Map.lookup packageName classes of
        Nothing -> do
            rootPkg <- resolveRootPackage packageName itemName
            makeIdent rootPkg itemName
        Just ([], _) -> resolveCSIdent packageName [] Set.empty itemName
        Just _ -> error $ "reference to " ++ show itemName
            ++ " in parameterized class " ++ show packageName
            ++ " requires explicit #()"

-- determines the root package contained the given package scoped identifier
resolveRootPackage :: Identifier -> Identifier -> PackagesState Identifier
resolveRootPackage packageName itemName = do
    (exports, _) <- findPackage packageName
    let maybeIdentState = Map.lookup itemName exports
    assertMsg (maybeIdentState /= Nothing) $
        "could not find " ++ show itemName ++ " in package " ++ show packageName
    let Just identState = maybeIdentState
    return $ toRootPackage packageName identState

-- collect hashes of accessed resolved scopes in class parameters
bindingsScopeKeys :: [ParamBinding] -> Scope Idents
bindingsScopeKeys =
    execWriterT . mapM (traverseTypeOrExprIdentsM identMapper) . map snd
    where
        identMapper :: Identifier -> WriterT Idents Scope Identifier
        identMapper x = do
            details <- lift $ lookupElemM x
            case details of
                Nothing -> return ()
                Just (accesses, _, _) ->
                    tell $ Set.singleton $ shortHash accesses
            return x
        traverseTypeOrExprIdentsM mapper (Left  t) =
            traverseTypeIdentsM mapper t >>= return . Left
        traverseTypeOrExprIdentsM mapper (Right e) =
            traverseExprIdentsM mapper e >>= return . Right

-- resolve a class scoped identifier to its unique global name
resolveCSIdent :: Identifier -> [ParamBinding] -> Idents -> Identifier -> PackagesState Identifier
resolveCSIdent className paramBindings scopeKeys itemName = do
    -- find the specified class
    classes <- gets pkClasses
    let maybeClass = Map.lookup className classes
    assertMsg (maybeClass /= Nothing) $
        "could not find class " ++ show className
    let Just (classParams, classItems) = maybeClass
    -- resolve the provided parameters
    let resolveMsg = "parameters in class specialization of " ++ show className
    let paramNames = mapMaybe extractParameterName classParams
    let paramBindings' = resolveBindings resolveMsg paramNames paramBindings
    -- generate a unique name for this synthetic package
    let packageName = className ++ '_' : shortHash (scopeKeys, paramBindings')
    -- process the synthetic package and inject the given parameters
    (exports, classItems') <- processPackage packageName $
        map Decl classParams ++ classItems
    let overrider = overrideParam packageName paramBindings'
    let classItems'' = map overrider classItems'
    -- add the synthetic package to the state
    let package = (exports, classItems'')
    packages' <- gets $ Map.insert packageName package . pkPackages
    modify' $ \pk -> pk { pkPackages = packages' }
    -- ensure the item actually exists
    let maybeIdentState = Map.lookup itemName exports
    assertMsg (maybeIdentState /= Nothing) $
        "could not find " ++ show itemName ++ " in class " ++ show className
    makeIdent packageName itemName
    where
        extractParameterName :: Decl -> Maybe Identifier
        extractParameterName (Param Parameter _ x _) = Just x
        extractParameterName (ParamType Parameter x _) = Just x
        extractParameterName _ = Nothing

        -- replace default parameter values with the given overrides
        overrideParam :: Identifier -> [ParamBinding] -> PackageItem -> PackageItem
        overrideParam packageName bindings (Decl (Param Parameter t x e)) =
            Decl $ Param Parameter t x $
                case lookup x' bindings of
                    Just (Right e') -> e'
                    Just (Left t') ->
                        error $ "cannot override parameter " ++ show x'
                            ++ " in class " ++ show className
                            ++ " with type " ++ show t'
                    Nothing ->
                        if e == Nil
                            then error $ "required parameter " ++ show x'
                                    ++ " in class " ++ show className
                                    ++ " has not been provided"
                            else e
            where x' = drop (1 + length packageName) x
        overrideParam packageName bindings (Decl (ParamType Parameter x t)) =
            Decl $ ParamType Parameter x $
                case lookup x' bindings of
                    Just (Left t') -> t'
                    Just (Right e') ->
                        case exprToType e' of
                            Just t' -> t'
                            Nothing ->
                                error $ "cannot override type parameter "
                                    ++ show x' ++ " in class " ++ show className
                                    ++ " with expression " ++ show e'
                    Nothing ->
                        if t == UnknownType
                            then error $ "required type parameter " ++ show x'
                                    ++ " in class " ++ show className
                                    ++ " has not been provided"
                            else t
            where x' = drop (1 + length packageName) x
        overrideParam _ _ other = other

-- construct a new identifier for a package scoped identifier
makeIdent :: Identifier -> Identifier -> PackagesState Identifier
makeIdent x y = do
    conflicts <- gets pkConflicts
    return $ uniqueIdent conflicts $ x ++ '_' : y

-- prepend underscores until the name is unique
uniqueIdent :: Idents -> Identifier -> Identifier
uniqueIdent conflicts ident =
    if Set.member ident conflicts
        then uniqueIdent conflicts $ '_' : ident
        else ident

-- errors with the given message when the check is false
assertMsg :: Monad m => Bool -> String -> m ()
assertMsg check msg = when (not check) $ error msg

-- helper for taking an ident which is either declared or exported form a
-- package and determine its true root source package
toRootPackage :: Identifier -> IdentState -> Identifier
toRootPackage sourcePackage identState =
    if identState == Declared
        then sourcePackage
        else rootPackage
    where Imported rootPackage = identState

-- nests packages items missing from modules
convertDescription :: PIs -> Description -> Description
convertDescription pis orig@Part{} =
    if Map.null pis
        then orig
        else Part attrs extern kw lifetime name ports items'
    where
        Part attrs extern kw lifetime name ports items = orig
        items' = addItems pis Set.empty $ map addUsedPIs $
            foldr breakGenerate [] items -- ensure decls are visible
convertDescription _ other = other

-- attempt to fix simple declaration order issues
reorderItems :: [ModuleItem] -> [ModuleItem]
reorderItems items =
    addItems localPIs Set.empty $ map addUsedPIs $
        map (traverseGenItems $ traverseNestedGenItems reorderGenItem) items
    where
        localPIs = Map.fromList $ concat $ mapMaybe toPIElem items
        toPIElem :: ModuleItem -> Maybe [(Identifier, PackageItem)]
        toPIElem (MIPackageItem item) = Just $ map (, item) (piNames item)
        toPIElem _ = Nothing

-- attempt to declaration order issues within generate blocks
reorderGenItem :: GenItem -> GenItem
reorderGenItem (GenBlock name genItems) =
    GenBlock name $ map unwrap $ reorderItems $ map wrap genItems
    where
        wrap :: GenItem -> ModuleItem
        wrap (GenModuleItem item) = item
        wrap item = Generate [item]
        unwrap :: ModuleItem -> GenItem
        unwrap (Generate [item]) = item
        unwrap item = GenModuleItem item
reorderGenItem item = item

-- iteratively inserts missing package items exactly where they are needed
addItems :: PIs -> Idents -> [(ModuleItem, Idents)] -> [ModuleItem]
addItems pis existingPIs ((item, usedPIs) : items) =
    if not $ forceKeep || Set.disjoint existingPIs thisPI then
        -- this item was re-imported earlier in the module
        addItems pis existingPIs items
    else if Map.null itemsToAdd then
        -- this item has no additional dependencies
        item : addItems pis (Set.union existingPIs thisPI) items
    else
        -- this item has at least one un-met dependency
        addItems remainingPIs existingPIs
            (addUsedPIs chosenItem : (item, usedPIs) : items)
    where
        thisPI = case item of
            MIPackageItem packageItem ->
                Set.fromList $ piNames packageItem
            Instance _ _ x _ _ -> Set.singleton x
            _ -> Set.empty
        forceKeep = case item of
            Instance{} -> True
            _ -> False
        neededPIs = Set.difference (Set.difference usedPIs existingPIs) thisPI
        itemsToAdd = Map.restrictKeys pis neededPIs
        (chosenName, chosenPI) = Map.findMin itemsToAdd
        chosenItem = MIPackageItem chosenPI
        remainingPIs = Map.delete chosenName pis
addItems _ _ [] = []

-- augment a module item with the set of identifiers it uses
addUsedPIs :: ModuleItem -> (ModuleItem, Idents)
addUsedPIs item =
    (item, usedPIs)
    where
        usedPIs = execWriter $ evalScoperT $ scoper item
        scoper = scopeModuleItem writeDeclIdents writeModuleItemIdents
            writeGenItemIdents writeStmtIdents

type IdentWriter = ScoperT () (Writer Idents)

writeDeclIdents :: Decl -> IdentWriter Decl
writeDeclIdents decl = do
    case decl of
        Variable _ _ x _ _ -> insertElem x ()
        Net  _ _ _ _ x _ _ -> insertElem x ()
        Param    _ _ x   _ -> insertElem x ()
        ParamType  _ x   _ -> insertElem x ()
        CommentDecl{} -> return ()
    traverseDeclIdentsM writeIdent decl

writeModuleItemIdents :: ModuleItem -> IdentWriter ModuleItem
writeModuleItemIdents = traverseIdentsM writeIdent

writeGenItemIdents :: GenItem -> IdentWriter GenItem
writeGenItemIdents =
    traverseGenItemExprsM $ traverseExprIdentsM writeIdent

writeStmtIdents :: Stmt -> IdentWriter Stmt
writeStmtIdents = traverseStmtIdentsM writeIdent

writeIdent :: Identifier -> IdentWriter Identifier
writeIdent x = do
    details <- lookupElemM x
    when (details == Nothing) $ tell (Set.singleton x)
    return x

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
        typeMapper (Alias x t) = identMapper x >>= return . flip Alias t
        typeMapper other = return other

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
        stmtMapper (Subroutine (Ident x) args) =
            identMapper x >>= \x' -> return $ Subroutine (Ident x') args
        stmtMapper other = return other

-- visits all identifiers in a declaration
traverseDeclIdentsM :: Monad m => MapperM m Identifier -> MapperM m Decl
traverseDeclIdentsM identMapper = traverseDeclNodesM
    (traverseTypeIdentsM identMapper)
    (traverseExprIdentsM identMapper)

-- returns any names defined by a package item
piNames :: PackageItem -> [Identifier]
piNames (Decl (ParamType _ ident (Enum _ enumItems _))) =
    ident : map fst enumItems
piNames (Function _ _ ident _ _) = [ident]
piNames (Task     _   ident _ _) = [ident]
piNames (Decl (Variable _ _ ident _ _)) = [ident]
piNames (Decl (Net  _ _ _ _ ident _ _)) = [ident]
piNames (Decl (Param    _ _ ident   _)) = [ident]
piNames (Decl (ParamType  _ ident   _)) = [ident]
piNames (Decl (CommentDecl          _)) = []
piNames item@DPIImport{} = [show item]
piNames item@DPIExport{} = [show item]
piNames item@Import{} = [show item]
piNames item@Export{} = [show item]
piNames (Directive _) = []
