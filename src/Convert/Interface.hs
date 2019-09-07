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
type Modports = Map.Map Identifier [ModportDecl]
type Modules = Map.Map (Identifier, Identifier) Type

convert :: [AST] -> [AST]
convert =
    traverseFiles (collectDescriptionsM collectDesc) converter
    where
        converter (interfaces, modules) =
            filter (not . isInterface) .
            map (convertDescription interfaces modules)
        -- we can only collect/map non-extern interfaces
        collectDesc :: Description -> Writer (Interfaces, Modules) ()
        collectDesc (orig @ (Part False kw _ name ports items)) = do
            if kw == Interface
                then tell (Map.singleton name (ports, items), Map.empty)
                else collectModuleItemsM (collectDeclsM $ collectDecl name) orig
        collectDesc _ = return ()
        collectDecl :: Identifier -> Decl -> Writer (Interfaces, Modules) ()
        collectDecl name (Variable _ t ident _ _) = do
            tell (Map.empty, Map.singleton (name, ident) t)
        collectDecl _ _ = return ()
        isInterface :: Description -> Bool
        isInterface (Part False Interface _ _ _ _) = True
        isInterface _ = False

convertDescription :: Interfaces -> Modules -> Description -> Description
convertDescription interfaces modules (Part extern Module lifetime name ports items) =
    Part extern Module lifetime name ports' items'
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
                    tell (Map.empty, Map.singleton ident modportDecls)
                    where Just modportDecls = lookupModport Nothing interfaceName modportName
                _ -> return ()
        collectInterface (Instance part _ ident Nothing _) =
            if Map.member part interfaces
                then tell (Map.singleton ident part, Map.empty)
                else return ()
        collectInterface _ = return ()

        mapInterface :: ModuleItem -> ModuleItem
        mapInterface (orig @ (MIPackageItem (Decl (Variable Local t ident _ _)))) =
            case Map.lookup ident modports of
                Just modportDecls -> Generate $
                    map (GenModuleItem . MIPackageItem . Decl . mapper)
                    modportDecls
                Nothing -> orig
            where
                InterfaceT interfaceName (Just _) [] = t
                interfaceItems = snd $ interfaces Map.! interfaceName
                mapper (dir, port, expr) =
                    Variable dir mpt (ident ++ "_" ++ port) mprs Nothing
                    where (mpt, mprs) = lookupType interfaceItems (fromJust expr)
        mapInterface (Instance part params ident Nothing instancePorts) =
            case Map.lookup part interfaces of
                Just interface ->
                    -- TODO: Add support for interfaces with parameter bindings.
                    if not $ null params
                    then error $ "interface instantiations with parameter "
                            ++ "bindings are not yet supported: "
                            ++ show (part, params, ident)
                    else Generate $ map GenModuleItem $
                            inlineInterface interface (ident, expandedPorts)
                Nothing -> Instance part params ident Nothing expandedPorts
            where expandedPorts = concatMap (expandPortBinding part) instancePorts
        mapInterface (orig @ (MIPackageItem (Function _ _ _ decls _))) =
            convertTF decls orig
        mapInterface (orig @ (MIPackageItem (Task _ _ decls _))) =
            convertTF decls orig
        mapInterface other = other

        convertTF :: [Decl] -> ModuleItem -> ModuleItem
        convertTF decls orig =
            traverseExprs (traverseNestedExprs $ convertExpr its mps) $
            traverseLHSs  (traverseNestedLHSs  $ convertLHS  its mps) $
            orig
            where
                locals = Set.fromList $ mapMaybe declVarIdent decls
                its = Map.withoutKeys instances locals
                mps = Map.withoutKeys modports  locals
        declVarIdent :: Decl -> Maybe Identifier
        declVarIdent (Variable _ _ x _ _) = Just x
        declVarIdent _ = Nothing

        expandPortBinding :: Identifier -> PortBinding -> [PortBinding]
        expandPortBinding _ (origBinding @ (portName, Just (Dot (Ident instanceName) modportName))) =
            case Map.lookup instanceName instances of
                Nothing ->
                    case Map.lookup instanceName modports of
                        Nothing -> [origBinding]
                        Just _ -> [(portName, Just $ Ident $ instanceName ++ "_" ++ modportName)]
                Just interfaceName ->
                    case modportDecls of
                        Nothing -> [(portName, Just $ Ident $ instanceName ++ "_" ++ modportName)]
                        Just decls -> map mapper decls
                    where
                        modportDecls = lookupModport (Just instanceName) interfaceName modportName
                        mapper (_, x, me) = (portName ++ "_" ++ x, me)
        expandPortBinding moduleName (origBinding @ (portName, Just (Ident instanceName))) =
            case (instances Map.!? instanceName, modports Map.!? instanceName) of
                (Nothing, Nothing) -> [origBinding]
                (Just _, _) ->
                    map mapper modportDecls
                    where
                        InterfaceT interfaceName (Just modportName) [] =
                            modules Map.! (moduleName, portName)
                        Just modportDecls = lookupModport (Just instanceName) interfaceName modportName
                        mapper (_, x, me) = (portName ++ "_" ++ x, me)
                (_, Just decls) ->
                    map mapper decls
                    where mapper (_, x, _) =
                            ( portName ++ "_" ++ x
                            , Just $ Ident $ instanceName ++ "_" ++ x )
        expandPortBinding _ other = [other]

        lookupModport :: Maybe Identifier -> Identifier -> Identifier -> Maybe [ModportDecl]
        lookupModport instanceName interfaceName = (Map.!?) modportMap
            where
                prefix = maybe "" (++ "_") instanceName
                interfaceItems =
                    map (prefixModuleItems prefix) $
                    snd $ interfaces Map.! interfaceName
                modportMap = execWriter $
                    mapM (collectNestedModuleItemsM collectModport) $
                    interfaceItems
                collectModport :: ModuleItem -> Writer Modports ()
                collectModport (Modport ident l) = tell $ Map.singleton ident l
                collectModport _ = return ()

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
        convertLHS its mps (LHSBit   l e) =
            LHSBit l (traverseNestedExprs (convertExpr its mps) e)
        convertLHS its mps (LHSRange l m (e1, e2)) =
            LHSRange l m (traverseNestedExprs (convertExpr its mps) e1, traverseNestedExprs (convertExpr its mps) e2)
        convertLHS _ _ other = other
        convertPort :: Identifier -> [Identifier]
        convertPort ident =
            case Map.lookup ident modports of
                Nothing -> [ident]
                Just decls -> map (\(_, x, _) -> ident ++ "_" ++ x) decls

convertDescription _ _ other = other


-- add a prefix to all standard identifiers in a module item
prefixModuleItems :: Identifier -> ModuleItem -> ModuleItem
prefixModuleItems prefix =
    traverseDecls prefixDecl .
    traverseExprs (traverseNestedExprs prefixExpr) .
    traverseLHSs  (traverseNestedLHSs  prefixLHS )
    where
        prefixDecl :: Decl -> Decl
        prefixDecl (Variable d t x a me) = Variable d t (prefix ++ x) a me
        prefixDecl (Param    s t x    e) = Param    s t (prefix ++ x)    e
        prefixDecl (ParamType  s x   mt) = ParamType  s (prefix ++ x)   mt
        prefixExpr :: Expr -> Expr
        prefixExpr (Ident x) = Ident (prefix ++ x)
        prefixExpr other = other
        prefixLHS :: LHS -> LHS
        prefixLHS (LHSIdent x) = LHSIdent (prefix ++ x)
        prefixLHS other = other

lookupType :: [ModuleItem] -> Expr -> (Type, [Range])
lookupType items (Ident ident) =
    case mapMaybe findType items of
        [] -> error $ "unable to locate type of " ++ ident
        ts -> head ts
    where
        findType :: ModuleItem -> Maybe (Type, [Range])
        findType (MIPackageItem (Decl (Variable _ t x rs Nothing))) =
            if x == ident then Just (t, rs) else Nothing
        findType _ = Nothing
lookupType _ expr =
    -- TODO: Add support for non-Ident modport expressions.
    error $ "interface conversion does not support modport expressions that "
        ++ " are not identifiers: " ++ show expr

-- convert an interface instantiation into a series of equivalent module items
inlineInterface :: Interface -> (Identifier, [PortBinding]) -> [ModuleItem]
inlineInterface (ports, items) (instanceName, instancePorts) =
    (:) (MIPackageItem $ Comment $ "expanded instance: " ++ instanceName) $
    flip (++) portBindings $
    map (traverseNestedModuleItems removeModport) $
    map (traverseNestedModuleItems removeDeclDir) $
    itemsPrefixed
    where
        prefix = instanceName ++ "_"
        itemsPrefixed = map (prefixModuleItems prefix) $ items
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
        removeDeclDir (MIPackageItem (Decl (Variable _ t x a me))) =
            MIPackageItem $ Decl $ Variable Local t x a me
        removeDeclDir other = other
        removeModport :: ModuleItem -> ModuleItem
        removeModport (Modport x _) =
            MIPackageItem $ Comment $ "removed modport " ++ x
        removeModport other = other

        portBindingItem :: PortBinding -> Maybe ModuleItem
        portBindingItem (ident, Just expr) =
            Just $ if declDirs Map.! ident == Input
                then Assign Nothing (LHSIdent ident) expr
                else Assign Nothing (toLHS expr) (Ident ident)
        portBindingItem (_, Nothing) = Nothing

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
