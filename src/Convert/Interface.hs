{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for interfaces
 -}

module Convert.Interface (convert) where

import Data.Maybe (isJust, mapMaybe)
import Control.Monad.Writer
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Instances = Map.Map Identifier Identifier
type Interface = ([Identifier], [ModuleItem])
type Interfaces = Map.Map Identifier Interface
type Modports = Map.Map Identifier [ModportDecl]

convert :: AST -> AST
convert descriptions =
    filter (not . isInterface) $
    traverseDescriptions (convertDescription interfaces) $
    descriptions
    where
        interfaces = execWriter $ collectDescriptionsM collectDesc descriptions
        -- we can only collect/map non-extern interfaces
        collectDesc :: Description -> Writer Interfaces ()
        collectDesc (Part False Interface _ name ports items) =
            tell $ Map.singleton name (ports, items)
        collectDesc _ = return ()
        isInterface :: Description -> Bool
        isInterface (Part False Interface _ _ _ _) = True
        isInterface _ = False

convertDescription :: Interfaces -> Description -> Description
convertDescription interfaces (Part extern Module lifetime name ports items) =
    Part extern Module lifetime name ports' items'
    where
        items' =
            map (traverseNestedModuleItems $ traverseExprs (traverseNestedExprs convertExpr)) $
            map (traverseNestedModuleItems $ traverseLHSs  (traverseNestedLHSs  convertLHS)) $
            map (traverseNestedModuleItems mapInterface) $
            items
        ports' = concatMap convertPort ports

        -- collect the interface type of all interface instances in this module
        (instances, modports) = execWriter $ mapM
            (collectNestedModuleItemsM collectInterface) items
        collectInterface :: ModuleItem -> Writer (Instances, Modports)  ()
        collectInterface (MIDecl (Variable Local t ident _ _)) =
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

        -- TODO: We don't yet handle interfaces with parameter bindings.
        mapInterface :: ModuleItem -> ModuleItem
        mapInterface (orig @ (MIDecl (Variable Local t ident _ _))) =
            case Map.lookup ident modports of
                Just modportDecls -> Generate $
                    map (GenModuleItem . MIDecl . mapper) modportDecls
                Nothing -> orig
            where
                InterfaceT interfaceName (Just _) [] = t
                interfaceItems = snd $ interfaces Map.! interfaceName
                mapper = \(dir, port, Just expr) ->
                    Variable dir (lookupType interfaceItems expr)
                    (ident ++ "_" ++ port) [] Nothing
        mapInterface (Instance part params ident Nothing instancePorts) =
            case Map.lookup part interfaces of
                Just interface ->
                    Generate $ map GenModuleItem $
                    inlineInterface interface (ident, expandedPorts)
                Nothing -> Instance part params ident Nothing expandedPorts
            where expandedPorts = concatMap expandPortBinding instancePorts
        mapInterface other = other

        expandPortBinding :: PortBinding -> [PortBinding]
        expandPortBinding (origBinding @ (portName, Just (Dot (Ident instanceName) modportName))) =
            case Map.lookup instanceName instances of
                Nothing -> [origBinding]
                Just interfaceName ->
                    case modportDecls of
                        Nothing -> [(portName, Just $ Ident $ instanceName ++ "_" ++ modportName)]
                        Just decls -> map mapper decls
                    where
                        modportDecls = lookupModport (Just instanceName) interfaceName modportName
                        mapper (_, x, me) = (portName ++ "_" ++ x, me)
        expandPortBinding other = [other]

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

        convertExpr :: Expr -> Expr
        convertExpr (orig @ (Dot (Ident x) y)) =
            if Map.member x modports || Map.member x instances
                then Ident (x ++ "_" ++ y)
                else orig
        convertExpr other = other
        convertLHS :: LHS -> LHS
        convertLHS (orig @ (LHSDot (LHSIdent x) y)) =
            if Map.member x modports || Map.member x instances
                then LHSIdent (x ++ "_" ++ y)
                else orig
        convertLHS other = other
        convertPort :: Identifier -> [Identifier]
        convertPort ident =
            case Map.lookup ident modports of
                Nothing -> [ident]
                Just decls -> map (\(_, x, _) -> ident ++ "_" ++ x) decls

convertDescription _ other = other


-- add a prefix to all standard identifiers in a module item
prefixModuleItems :: Identifier -> ModuleItem -> ModuleItem
prefixModuleItems prefix =
    traverseDecls prefixDecl .
    traverseExprs (traverseNestedExprs prefixExpr) .
    traverseLHSs  (traverseNestedLHSs  prefixLHS )
    where
        prefixDecl :: Decl -> Decl
        prefixDecl (Variable d t x a me) = Variable d t (prefix ++ x) a me
        prefixDecl (Parameter  t x e) = Parameter  t (prefix ++ x) e
        prefixDecl (Localparam t x e) = Localparam t (prefix ++ x) e
        prefixExpr :: Expr -> Expr
        prefixExpr (Ident x) = Ident (prefix ++ x)
        prefixExpr other = other
        prefixLHS :: LHS -> LHS
        prefixLHS (LHSIdent x) = LHSIdent (prefix ++ x)
        prefixLHS other = other

-- TODO: this is an incomplete attempt at looking up the type of an expression;
-- there is definitely some overlap here with the Struct conversion
lookupType :: [ModuleItem] -> Expr -> Type
lookupType items (Ident ident) =
    head $ mapMaybe findType items
    where
        findType :: ModuleItem -> Maybe Type
        findType (MIDecl (Variable _ t x [] Nothing)) =
            if x == ident then Just t else Nothing
        findType _ = Nothing
lookupType _ expr = error $ "lookupType on fancy expr: " ++ show expr

-- convert an interface instantiation into a series of equivalent module items
inlineInterface :: Interface -> (Identifier, [PortBinding]) -> [ModuleItem]
inlineInterface (ports, items) (instanceName, instancePorts) =
    (:) (MIPackageItem $ Comment $ "expanded instance: " ++ instanceName) $
    flip (++) portBindings $
    map (traverseNestedModuleItems removeModport) $
    map (traverseNestedModuleItems removeMIDeclDir) $
    map (prefixModuleItems prefix) $
    items
    where
        prefix = instanceName ++ "_"
        origInstancePortNames = map fst instancePorts
        instancePortExprs = map snd instancePorts
        instancePortNames =
            map (prefix ++) $
            if all ("" ==) origInstancePortNames
                then ports
                else origInstancePortNames
        portBindings =
            map (\(ident, Just expr) -> Assign Nothing (LHSIdent ident) expr) $
            filter (isJust . snd) $
            zip instancePortNames instancePortExprs

        removeMIDeclDir :: ModuleItem -> ModuleItem
        removeMIDeclDir (MIDecl (Variable _ t x a me)) =
            MIDecl $ Variable Local t x a me
        removeMIDeclDir other = other
        removeModport :: ModuleItem -> ModuleItem
        removeModport (Modport x _) =
            MIPackageItem $ Comment $ "removed modport " ++ x
        removeModport other = other
