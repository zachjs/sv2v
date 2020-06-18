{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for interfaces
 -}

module Convert.Interface (convert) where

import Data.Maybe (fromJust, mapMaybe)
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type Instances = Map.Map Identifier Identifier
type Interface = ([Identifier], [ModuleItem])
type Interfaces = Map.Map Identifier Interface
type Modports = Map.Map Identifier (Identifier, [ModportDecl])
type Modules = Map.Map Identifier ([Identifier], [(Identifier, Type)])

convert :: [AST] -> [AST]
convert =
    map (filter $ not . isInterface) .
    repeatedConverter
    where
        repeatedConverter :: [AST] -> [AST]
        repeatedConverter files =
            if files == files'
                then files
                else repeatedConverter files'
            where
                files' =
                    traverseFiles (collectDescriptionsM collectDesc)
                    (map . uncurry convertDescription)
                    files
        -- we can only collect/map non-extern interfaces
        collectDesc :: Description -> Writer (Interfaces, Modules) ()
        collectDesc (orig @ (Part _ False kw _ name ports items)) = do
            if kw == Interface
                then tell (Map.singleton name (ports, items), Map.empty)
                else tell (Map.empty, Map.singleton name (params, decls))
            where
                params = map fst $ parameters items
                decls = execWriter $
                    collectModuleItemsM (collectDeclsM collectDecl) orig
                collectDecl :: Decl -> Writer [(Identifier, Type)] ()
                collectDecl (Variable _ t ident _ _) =
                    tell [(ident, t)]
                collectDecl _ = return ()
        collectDesc _ = return ()
        isInterface :: Description -> Bool
        isInterface (Part _ False Interface _ _ _ _) = True
        isInterface _ = False

convertDescription :: Interfaces -> Modules -> Description -> Description
convertDescription interfaces modules (Part attrs extern Module lifetime name ports items) =
    Part attrs extern Module lifetime name ports' items'
    where
        items' =
            map (traverseNestedModuleItems $ traverseExprs' ExcludeTFs (traverseNestedExprs $ convertExpr instances modports)) $
            map (traverseNestedModuleItems $ traverseLHSs'  ExcludeTFs (traverseNestedLHSs  $ convertLHS  instances modports)) $
            map (traverseNestedModuleItems mapInterface) $
            items
        ports' = concatMap convertPort ports

        -- collect the interface type of all interface instances in this module
        (instances, modports) = execWriter $ mapM
            (collectNestedModuleItemsM collectInterface) items
        collectInterface :: ModuleItem -> Writer (Instances, Modports)  ()
        collectInterface (MIPackageItem (Decl (Variable _ t ident _ _))) =
            case t of
                InterfaceT interfaceName (Just modportName) [] ->
                    tell (Map.empty, Map.singleton ident (interfaceName, modportDecls))
                    where Just modportDecls = lookupModport interfaceName modportName
                Alias Nothing interfaceName [] ->
                    case impliedModport interfaceName of
                        Just modportDecls ->
                            tell (Map.empty, Map.singleton ident modport)
                            where modport = (interfaceName, modportDecls)
                        Nothing -> return ()
                _ -> return ()
        collectInterface (Instance part _ ident [] _) =
            if Map.member part interfaces
                then tell (Map.singleton ident part, Map.empty)
                else return ()
        collectInterface _ = return ()

        mapInterface :: ModuleItem -> ModuleItem
        mapInterface (orig @ (MIPackageItem (Decl (Variable _ t ident _ _)))) =
            -- expand instantiation of a modport
            case Map.lookup ident modports of
                Just (_, modportDecls) -> Generate $
                    map (GenModuleItem . MIPackageItem . Decl)
                    (parameterDecls ++ map mapper modportDecls)
                Nothing -> orig
            where
                interfaceName = case t of
                    InterfaceT x (Just _) [] -> x
                    Alias Nothing x [] -> x
                    _ -> error $ "unexpected modport type " ++ show t
                interfaceItems =
                    case Map.lookup interfaceName interfaces of
                        Just res -> snd res
                        Nothing -> error $ "could not find interface " ++ show interfaceName
                parameterDecls = map snd $ parameters interfaceItems
                mapper :: ModportDecl -> Decl
                mapper (dir, port, expr) =
                    Variable dir mpt (ident ++ "_" ++ port) mprs Nil
                    where (mpt, mprs) = lookupType interfaceItems expr
        mapInterface (Instance part params ident [] instancePorts) =
            -- expand modport port bindings
            case Map.lookup part interfaces of
                Just interface ->
                    -- inline instantiation of an interface
                    Generate $ map GenModuleItem $
                        inlineInterface interface (ident, params, instancePorts)
                Nothing ->
                    if Map.member part modules
                        then Instance part params' ident [] expandedPorts
                        else Instance part params  ident [] instancePorts
            where
                expandedBindings = map (uncurry $ expandPortBinding part) (zip instancePorts [0..])
                expandedPorts = concatMap snd expandedBindings
                Just (moduleParamNames, _) = Map.lookup part modules
                addedParams = concatMap fst expandedBindings
                paramsNamed = resolveParams moduleParamNames params
                params' =
                    if null addedParams
                        then params
                        else paramsNamed ++ addedParams
        mapInterface (orig @ (MIPackageItem (Function _ _ _ decls _))) =
            convertTF decls orig
        mapInterface (orig @ (MIPackageItem (Task _ _ decls _))) =
            convertTF decls orig
        mapInterface other = other

        convertTF :: [Decl] -> ModuleItem -> ModuleItem
        convertTF decls =
            traverseExprs (traverseNestedExprs $ convertExpr its mps) .
            traverseLHSs  (traverseNestedLHSs  $ convertLHS  its mps)
            where
                locals = Set.fromList $ map declVarIdent decls
                its = Map.withoutKeys instances locals
                mps = Map.withoutKeys modports  locals
                declVarIdent :: Decl -> Identifier
                declVarIdent (Variable _ _ x _ _) = x
                declVarIdent _ = ""

        expandPortBinding :: Identifier -> PortBinding -> Int -> ([ParamBinding], [PortBinding])
        expandPortBinding _ (origBinding @ (portName, Dot (Ident instanceName) modportName)) _ =
            -- expand instance modport bound to a modport
            if Map.member instanceName instances && modportDecls /= Nothing
                then expandPortBinding' interfaceName portName instanceName
                        (fromJust modportDecls)
                else ([], [origBinding])
            where
                interfaceName = instances Map.! instanceName
                modportDecls = lookupModport interfaceName modportName
        expandPortBinding moduleName (origBinding @ (portName, Ident ident)) idx =
            case (instances Map.!? ident, modports Map.!? ident) of
                (Nothing, Nothing) -> ([], [origBinding])
                (Just interfaceName, _) ->
                    -- given entire interface, but just bound to a modport
                    if Map.notMember moduleName modules then
                        error $ "could not find module " ++ show moduleName
                    else if modportDecls == Nothing then
                        ([], [origBinding])
                    else
                        expandPortBinding' interfaceName portName ident
                            (fromJust modportDecls)
                    where
                        Just (_, decls) = Map.lookup moduleName modules
                        portType =
                            if null portName
                            then if idx < length decls
                                then snd $ decls !! idx
                                else error $ "could not infer port "
                                    ++ show origBinding ++ " in module "
                                    ++ show moduleName
                            else case lookup portName decls of
                                Nothing -> error $ "could not find port "
                                    ++ show portName ++ " in module "
                                    ++ show moduleName
                                Just t -> t
                        modportDecls =
                            case portType of
                                InterfaceT _ (Just modportName) [] ->
                                    lookupModport interfaceName modportName
                                Alias Nothing _ [] ->
                                    impliedModport interfaceName
                                _ -> Nothing
                (_, Just (interfaceName, modportDecls)) ->
                    -- modport directly bound to a modport
                    expandPortBinding' interfaceName portName ident
                        (map redirect modportDecls)
                    where redirect (d, x, _) = (d, x, Ident x)
        expandPortBinding _ other _ = ([], [other])

        expandPortBinding' :: Identifier -> Identifier -> Identifier ->
            [ModportDecl] -> ([ParamBinding], [PortBinding])
        expandPortBinding' interfaceName portName instanceName modportDecls =
            (paramBindings, portBindings)
            where
                paramBindings = map toParamBinding interfaceParamNames
                interfaceItems = snd $ interfaces Map.! interfaceName
                interfaceParamNames = map fst $ parameters interfaceItems
                toParamBinding x = (x, Right $ Ident $ instanceName ++ '_' : x)
                portBindings = map toPortBinding modportDecls
                toPortBinding (_, x, e) = (x', e')
                    where
                        x' = if null portName then "" else portName ++ '_' : x
                        e' = traverseNestedExprs prefixExpr e
                prefixExpr :: Expr -> Expr
                prefixExpr (Ident x) = Ident (instanceName ++ '_' : x)
                prefixExpr other = other

        lookupModport :: Identifier -> Identifier -> Maybe [ModportDecl]
        lookupModport interfaceName =
            if Map.member interfaceName interfaces
                then (Map.!?) modportMap
                else error $ "could not find interface " ++ show interfaceName
            where
                interfaceItems = snd $ interfaces Map.! interfaceName
                modportMap = execWriter $
                    mapM (collectNestedModuleItemsM collectModport) $
                    interfaceItems
                collectModport :: ModuleItem -> Writer (Map.Map Identifier [ModportDecl]) ()
                collectModport (Modport ident l) = tell $ Map.singleton ident l
                collectModport _ = return ()

        impliedModport :: Identifier -> Maybe [ModportDecl]
        impliedModport interfaceName =
            if Map.member interfaceName interfaces
                then Just modport
                else Nothing
            where
                interfaceItems = snd $ interfaces Map.! interfaceName
                modport = execWriter $
                    mapM (collectNestedModuleItemsM collectModportDecls) $
                    interfaceItems
                collectModportDecls :: ModuleItem -> Writer [ModportDecl] ()
                collectModportDecls (MIPackageItem (Decl (Variable d _ x _ _))) =
                    tell [(d', x, Ident x)]
                    where d' = if d == Local then Inout else d
                collectModportDecls _ = return ()

        convertExpr :: Instances -> Modports -> Expr -> Expr
        convertExpr its mps (orig @ (Dot (Ident x) y)) =
            if Map.member x mps || Map.member x its
                then Ident (x ++ "_" ++ y)
                else orig
        convertExpr _ _ other = other
        convertLHS :: Instances -> Modports -> LHS -> LHS
        convertLHS its mps (orig @ (LHSDot (LHSIdent x) y)) =
            if Map.member x mps || Map.member x its
                then LHSIdent (x ++ "_" ++ y)
                else orig
        convertLHS _ _ other = other
        convertPort :: Identifier -> [Identifier]
        convertPort ident =
            case Map.lookup ident modports of
                Nothing -> [ident]
                Just (_, decls) -> map (\(_, x, _) -> ident ++ "_" ++ x) decls

convertDescription _ _ other = other


-- add a prefix to all standard identifiers in a module item
prefixModuleItems :: (Identifier -> Identifier) -> ModuleItem -> ModuleItem
prefixModuleItems prefix =
    prefixOtherItem .
    traverseDecls prefixDecl .
    traverseExprs (traverseNestedExprs prefixExpr) .
    traverseLHSs  (traverseNestedLHSs  prefixLHS )
    where
        prefixDecl :: Decl -> Decl
        prefixDecl (Variable d t x a e) = Variable d t (prefix x) a e
        prefixDecl (Param    s t x   e) = Param    s t (prefix x)   e
        prefixDecl (ParamType  s x  mt) = ParamType  s (prefix x)  mt
        prefixDecl (CommentDecl      c) = CommentDecl               c
        prefixExpr :: Expr -> Expr
        prefixExpr (Ident x) = Ident (prefix x)
        prefixExpr other = other
        prefixLHS :: LHS -> LHS
        prefixLHS (LHSIdent x) = LHSIdent (prefix x)
        prefixLHS other = other
        prefixOtherItem :: ModuleItem -> ModuleItem
        prefixOtherItem (MIPackageItem item) =
            MIPackageItem $ prefixPackageItem prefix item
        prefixOtherItem (Instance m params name rs ports) =
            Instance m params (prefix name) rs ports
        prefixOtherItem (Genvar x) = Genvar $ prefix x
        prefixOtherItem other = other

-- add a prefix to all standard identifiers in a package item
prefixPackageItem :: (Identifier -> Identifier) -> PackageItem -> PackageItem
prefixPackageItem prefix (Function lifetime t x decls stmts) =
    Function lifetime t x' decls stmts
    where x' = prefix x
prefixPackageItem prefix (Task     lifetime   x decls stmts) =
    Task     lifetime   x' decls stmts
    where x' = prefix x
prefixPackageItem _ other = other

-- collect all identifiers defined within a module item
collectIdentsM :: ModuleItem -> Writer (Set.Set Identifier) ()
collectIdentsM (MIPackageItem (Function _ _ x _ _)) = tell $ Set.singleton x
collectIdentsM (MIPackageItem (Task     _   x _ _)) = tell $ Set.singleton x
collectIdentsM (Instance _ _ x _ _) = tell $ Set.singleton x
collectIdentsM (Genvar x) = tell $ Set.singleton x
collectIdentsM item = collectDeclsM collectDecl item
    where
        collectDecl :: Decl -> Writer (Set.Set Identifier) ()
        collectDecl (Variable _ _ x _ _) = tell $ Set.singleton x
        collectDecl (Param    _ _ x   _) = tell $ Set.singleton x
        collectDecl (ParamType  _ x   _) = tell $ Set.singleton x
        collectDecl (CommentDecl      _) = return ()

lookupType :: [ModuleItem] -> Expr -> (Type, [Range])
lookupType items (Ident ident) =
    case mapMaybe findType items of
        [] -> error $ "unable to locate type of " ++ ident
        ts -> head ts
    where
        findType :: ModuleItem -> Maybe (Type, [Range])
        findType (MIPackageItem (Decl (Variable _ t x rs _))) =
            if x == ident then Just (t, rs) else Nothing
        findType _ = Nothing
lookupType _ expr =
    -- TODO: Add support for non-Ident modport expressions.
    error $ "interface conversion does not support modport expressions that "
        ++ " are not identifiers: " ++ show expr

-- convert an interface instantiation into a series of equivalent module items
inlineInterface :: Interface -> (Identifier, [ParamBinding], [PortBinding]) -> [ModuleItem]
inlineInterface (ports, items) (instanceName, instanceParams, instancePorts) =
    (:) comment $
    flip (++) portBindings $
    map (traverseNestedModuleItems removeModport) $
    map (traverseNestedModuleItems removeDeclDir) $
    itemsPrefixed
    where
        comment = MIPackageItem $ Decl $ CommentDecl $
            "expanded instance: " ++ instanceName
        prefix = instanceName ++ "_"
        idents = execWriter $
            mapM (collectNestedModuleItemsM collectIdentsM) items
        prefixIfNecessary :: Identifier -> Identifier
        prefixIfNecessary x =
            if Set.member x idents
                then prefix ++ x
                else x
        itemsPrefixed =
            map (traverseNestedModuleItems $ prefixModuleItems prefixIfNecessary) $
            map (traverseDecls overrideParam) $
            items
        origInstancePortNames = map fst instancePorts
        instancePortExprs = map snd instancePorts
        instancePortNames =
            map (prefix ++) $
            if all ("" ==) origInstancePortNames
                then ports
                else origInstancePortNames
        portBindings =
            mapMaybe portBindingItem $
            zip instancePortNames instancePortExprs

        removeDeclDir :: ModuleItem -> ModuleItem
        removeDeclDir (MIPackageItem (Decl (Variable _ t x a e))) =
            MIPackageItem $ Decl $ Variable Local t x a e
        removeDeclDir other = other
        removeModport :: ModuleItem -> ModuleItem
        removeModport (Modport x _) =
            MIPackageItem $ Decl $ CommentDecl $ "removed modport " ++ x
        removeModport other = other

        interfaceParamNames = map fst $ parameters items
        instanceParamMap =
            Map.fromList $ resolveParams interfaceParamNames instanceParams
        overrideParam :: Decl -> Decl
        overrideParam (Param Parameter t x e) =
            case Map.lookup x instanceParamMap of
                Nothing -> Param Parameter t x e
                Just (Right e') -> Param Parameter t x e'
                Just (Left t') ->
                    error $ "interface param expected expression, found type: "
                        ++ show t'
        overrideParam (ParamType Parameter x mt) =
            case Map.lookup x instanceParamMap of
                Nothing -> ParamType Parameter x mt
                Just (Left t') -> ParamType Parameter x (Just t')
                Just (Right e') ->
                    error $ "interface param expected type, found expression: "
                        ++ show e'
        overrideParam other = other

        portBindingItem :: PortBinding -> Maybe ModuleItem
        portBindingItem (_, Nil) = Nothing
        portBindingItem (ident, expr) =
            Just $ if declDirs Map.! ident == Input
                then Assign AssignOptionNone (LHSIdent ident) expr
                else Assign AssignOptionNone (toLHS expr) (Ident ident)

        declDirs = execWriter $
            mapM (collectDeclsM collectDeclDir) itemsPrefixed
        collectDeclDir :: Decl -> Writer (Map.Map Identifier Direction) ()
        collectDeclDir (Variable dir _ ident _ _) =
            if dir /= Local
                then tell $ Map.singleton ident dir
                else return ()
        collectDeclDir _ = return ()

        toLHS :: Expr -> LHS
        toLHS expr =
            case exprToLHS expr of
                Just lhs -> lhs
                Nothing  -> error $ "trying to bind an interface output to " ++
                                show expr ++ " but that can't be an LHS"

-- give a set of param bindings explicit names
resolveParams :: [Identifier] -> [ParamBinding] -> [ParamBinding]
resolveParams available bindings =
    map (uncurry resolveParam) $ zip bindings [0..]
    where
        resolveParam :: ParamBinding -> Int -> ParamBinding
        resolveParam ("", e) idx =
            if idx < length available
                then (available !! idx, e)
                else error $ "interface param binding " ++ (show e)
                        ++ " is out of range"
        resolveParam other _ = other

-- given a list of module items, produces the parameters in order
parameters :: [ModuleItem] -> [(Identifier, Decl)]
parameters =
    execWriter . mapM (collectNestedModuleItemsM $ collectDeclsM collectDeclM)
    where
        collectDeclM :: Decl -> Writer [(Identifier, Decl)] ()
        collectDeclM (decl @ (Param Parameter _ x _)) = tell [(x, decl)]
        collectDeclM (decl @ (ParamType Parameter x _)) = tell [(x, decl)]
        collectDeclM _ = return ()
