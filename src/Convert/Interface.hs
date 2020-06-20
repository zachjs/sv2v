{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for interfaces
 -}

module Convert.Interface (convert) where

import Data.Maybe (mapMaybe)
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type Interface = ([Identifier], [ModuleItem])
type Interfaces = Map.Map Identifier Interface
type Module = ([Identifier], [(Identifier, Type)])
type Modules = Map.Map Identifier Module
type Instances = Map.Map Identifier Identifier
type Modports = Map.Map Identifier (Identifier, Identifier)

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
            if kw == Interface then
                if all fullyResolved items
                    then tell (Map.singleton name (ports, items), Map.empty)
                    else return ()
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
        isInterface (Part _ False Interface _ _ _ items) =
            all fullyResolved items
        isInterface _ = False
        -- returns whether a ModuleItem still contains TypeOf
        fullyResolved :: ModuleItem -> Bool
        fullyResolved =
            not . any isTypeOf . execWriter .
                collectNestedModuleItemsM (collectTypesM collectType)
            where
                collectType :: Type -> Writer [Type] ()
                collectType t = tell [t]
                isTypeOf TypeOf{} = True
                isTypeOf _ = False

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
            (collectNestedModuleItemsM collectInstanceM) items
        collectInstanceM :: ModuleItem -> Writer (Instances, Modports) ()
        collectInstanceM (MIPackageItem (Decl (Variable _ t ident _ _))) =
            case t of
                InterfaceT interfaceName (Just modportName) [] ->
                    if Map.member interfaceName interfaces
                        then writeModport interfaceName modportName
                        else return ()
                Alias Nothing interfaceName [] ->
                    if Map.member interfaceName interfaces
                        then writeModport interfaceName ""
                        else return ()
                _ -> return ()
            where
                writeModport :: Identifier -> Identifier ->
                    Writer (Instances, Modports) ()
                writeModport interfaceName modportName =
                    tell (Map.empty, Map.singleton ident modport)
                    where modport = (interfaceName, modportName)
        collectInstanceM (Instance part _ ident [] _) =
            if Map.member part interfaces
                then tell (Map.singleton ident part, Map.empty)
                else return ()
        collectInstanceM _ = return ()

        mapInterface :: ModuleItem -> ModuleItem
        mapInterface (orig @ (MIPackageItem (Decl (Variable _ _ ident _ _)))) =
            -- expand instantiation of a modport
            if Map.member ident modports
                then Generate $ map GenModuleItem $
                    filter shouldKeep interfaceItems ++ map makePortDecl
                    modportDecls
                else orig
            where
                Just (interfaceName, modportName) = Map.lookup ident modports
                interfaceItems = prefixInterface ident $
                    snd $ lookupInterface interfaceName
                modportDecls = lookupModport interfaceItems modportName
                shouldKeep (MIPackageItem (Decl Param{})) = True
                shouldKeep (MIPackageItem Task{}) = True
                shouldKeep (MIPackageItem Function{}) = True
                shouldKeep _ = False
                makePortDecl :: ModportDecl -> ModuleItem
                makePortDecl (dir, port, typ, _) =
                    MIPackageItem $ Decl $ Variable dir typ port' [] Nil
                    where port' = if null modportName
                                    then port
                                    else ident ++ '_' : port
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
        expandPortBinding moduleName ("", binding) idx =
            case Map.lookup moduleName modules of
                Nothing -> error $ "could not find module: " ++ moduleName
                Just (_, decls) ->
                    if idx < length decls
                        then expandPortBinding moduleName
                            (fst $ decls !! idx, binding) idx
                        else error $ "could not infer port for "
                            ++ show binding ++ " in module " ++ show moduleName
        expandPortBinding _ (origBinding @ (portName, Dot (Ident instanceName) modportName)) _ =
            -- expand instance modport bound to a modport
            if Map.member instanceName instances
                then expandPortBinding' interfaceName portName instanceName
                        modportDecls
                else ([], [origBinding])
            where
                interfaceName = instances Map.! instanceName
                interfaceItems = snd $ lookupInterface interfaceName
                modportDecls = lookupModport interfaceItems modportName
        expandPortBinding moduleName (origBinding @ (portName, Ident ident)) _ =
            case (instances Map.!? ident, modports Map.!? ident) of
                (Nothing, Nothing) -> ([], [origBinding])
                (Just interfaceName, _) ->
                    -- given entire interface, but just bound to a modport
                    if Map.notMember moduleName modules then
                        error $ "could not find module " ++ show moduleName
                    else
                        expandPortBinding' interfaceName portName ident
                            modportDecls
                    where
                        Just (_, decls) = Map.lookup moduleName modules
                        portType =
                            case lookup portName decls of
                                Nothing -> error $ "could not find port "
                                    ++ show portName ++ " in module "
                                    ++ show moduleName
                                Just t -> t
                        interfaceItems = snd $ lookupInterface interfaceName
                        modportDecls = lookupModport interfaceItems modportName
                        modportName = case portType of
                            InterfaceT _ (Just x) [] -> x
                            Alias Nothing _ [] -> ""
                            _ -> error $ "can't deduce modport for interface "
                                    ++ interfaceName ++ " bound to port "
                                    ++ portName ++ " of module " ++ moduleName
                (_, Just (interfaceName, modportName)) ->
                    -- modport directly bound to a modport
                    expandPortBinding' interfaceName portName ident
                        (map redirect modportDecls)
                    where
                        interfaceItems = snd $ lookupInterface interfaceName
                        modportDecls = lookupModport interfaceItems modportName
                        redirect (d, x, t, _) = (d, x, t, Ident x)
        expandPortBinding _ other _ = ([], [other])

        expandPortBinding' :: Identifier -> Identifier -> Identifier ->
            [ModportDecl] -> ([ParamBinding], [PortBinding])
        expandPortBinding' interfaceName portName instanceName modportDecls =
            (paramBindings, portBindings)
            where
                paramBindings = map toParamBinding interfaceParamNames
                interfaceItems = snd $ lookupInterface interfaceName
                interfaceParamNames = map fst $ parameters interfaceItems
                toParamBinding x = (portName ++ '_' : x, Right $ Ident $ instanceName ++ '_' : x)
                portBindings = map toPortBinding modportDecls
                toPortBinding (_, x, _, e) = (x', e')
                    where
                        x' = portName ++ '_' : x
                        e' = traverseNestedExprs prefixExpr e
                prefixExpr :: Expr -> Expr
                prefixExpr (Ident x) = Ident (instanceName ++ '_' : x)
                prefixExpr other = other

        lookupInterface :: Identifier -> Interface
        lookupInterface interfaceName =
            case Map.lookup interfaceName interfaces of
                Just res -> res
                Nothing -> error $ "could not find interface " ++ show interfaceName

        lookupModport :: [ModuleItem] -> Identifier -> [ModportDecl]
        lookupModport interfaceItems "" = impliedModport interfaceItems
        lookupModport interfaceItems modportName =
            case Map.lookup modportName modportMap of
                Just modportDecls -> modportDecls
                Nothing -> error $ "could not find modport " ++ show modportName
            where
                modportMap = execWriter $
                    mapM (collectNestedModuleItemsM collectModport) $
                    interfaceItems
                collectModport :: ModuleItem -> Writer (Map.Map Identifier [ModportDecl]) ()
                collectModport (Modport ident l) = tell $ Map.singleton ident l
                collectModport _ = return ()

        impliedModport :: [ModuleItem] -> [ModportDecl]
        impliedModport =
            execWriter . mapM (collectNestedModuleItemsM collectModportDecls)
            where
                collectModportDecls :: ModuleItem -> Writer [ModportDecl] ()
                collectModportDecls (MIPackageItem (Decl (Variable d t x _ _))) =
                    tell [(d', x, t, Ident x)]
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
                Just (interfaceName, modportName) ->
                    map (\(_, x, _, _) ->
                    ident ++ "_" ++ x) modportDecls
                    where
                    interfaceItems = snd $ lookupInterface interfaceName
                    modportDecls = lookupModport interfaceItems modportName

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
        itemsPrefixed =
            map (traverseDecls overrideParam) $
            prefixInterface instanceName items
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
            MIPackageItem $ Decl $ Variable Local t' x a e
            where t' = case t of
                    Implicit Unspecified rs ->
                        IntegerVector TLogic Unspecified rs
                    _ -> t
        removeDeclDir other = other
        removeModport :: ModuleItem -> ModuleItem
        removeModport (Modport x _) =
            MIPackageItem $ Decl $ CommentDecl $ "removed modport " ++ x
        removeModport other = other

        interfaceParamNames = map fst $ parameters items
        instanceParamMap = Map.mapKeys (prefix ++) $
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

-- convert an interface instantiation into a series of equivalent module items
prefixInterface :: Identifier -> [ModuleItem] -> [ModuleItem]
prefixInterface instanceName items =
    map prefixItem items
    where
        prefix = instanceName ++ "_"
        idents = execWriter $
            mapM (collectNestedModuleItemsM collectIdentsM) items
        prefixIfNecessary :: Identifier -> Identifier
        prefixIfNecessary x =
            if Set.member x idents
                then prefix ++ x
                else x
        prefixItem = traverseNestedModuleItems $
            prefixModuleItems prefixIfNecessary

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
