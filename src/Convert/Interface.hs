{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for interfaces
 -}

module Convert.Interface (convert) where

import Data.Maybe (isJust)
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
        collectDesc :: Description -> Writer Interfaces ()
        collectDesc (Part Interface name ports items) =
            tell $ Map.singleton name (ports, items)
        collectDesc _ = return ()
        isInterface :: Description -> Bool
        isInterface (Part Interface _ _ _) = True
        isInterface _ = False

-- TODO FIXME XXX: We should probably extract out/flatten the needless generate
-- blocks we make during covnersion...

convertDescription :: Interfaces -> Description -> Description
convertDescription interfaces (orig @ (Part Module name _ _)) =
    Part Module name ports' items'
    where
        Part Module _ ports items = traverseModuleItems mapInstance orig
        ports' = ports
        items' = items

        -- collect the interface type of all interface instances in this module
        instances = execWriter $ collectModuleItemsM collectInstance orig
        collectInstance :: ModuleItem -> Writer Instances ()
        collectInstance (Instance part _ ident _) =
            if Map.member part interfaces
                then tell $ Map.singleton ident part
                else return ()
        collectInstance _ = return ()

        -- TODO: We don't yet handle interfaces with parameter bindings.
        mapInstance :: ModuleItem -> ModuleItem
        mapInstance (Instance part params ident (Just instancePorts)) =
            case Map.lookup part interfaces of
                Just interface ->
                    Generate $ map GenModuleItem $
                    inlineInterface interface (ident, expandedPorts)
                Nothing -> Instance part params ident (Just expandedPorts)
            where expandedPorts = concatMap expandPortBinding instancePorts
        mapInstance other = other

        expandPortBinding :: PortBinding -> [PortBinding]
        expandPortBinding (origBinding @ (portName, Just (Access (Ident instanceName) modportName))) =
            case Map.lookup instanceName instances of
                Nothing -> [origBinding]
                Just interfaceName ->
                    map mapper modportDecls
                    where
                        modportDecls = lookupModport instanceName interfaceName modportName
                        mapper (_, x, me) = (portName ++ "_" ++ x, me)
        expandPortBinding other = [other]

        lookupModport :: Identifier -> Identifier -> Identifier -> [ModportDecl]
        lookupModport instanceName interfaceName = (Map.!) modportMap
            where
                interfaceItems =
                    map (prefixModuleItems $ instanceName ++ "_") $
                    snd $ interfaces Map.! interfaceName
                modportMap = execWriter $
                    mapM (collectNestedModuleItemsM collectModport) $
                    interfaceItems
                collectModport :: ModuleItem -> Writer Modports ()
                collectModport (Modport x l) = tell $ Map.singleton x l
                collectModport _ = return ()

convertDescription _ other = other


-- add a prefix to all standard identifiers in a module item
prefixModuleItems :: Identifier -> ModuleItem -> ModuleItem
prefixModuleItems prefix =
    traverseDecls prefixDecl .
    traverseExprs prefixExpr .
    traverseLHSs  prefixLHS
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

-- convert an interface instantiation into a series of equivalent module items
inlineInterface :: Interface -> (Identifier, [PortBinding]) -> [ModuleItem]
inlineInterface (ports, items) (instanceName, instancePorts) =
    (:) (Comment $ "expanded instance: " ++ instanceName) $
    (++) portBindings $
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
            map (\(ident, Just expr) -> Assign (LHSIdent ident) expr) $
            filter (isJust . snd) $
            zip instancePortNames instancePortExprs

        removeMIDeclDir :: ModuleItem -> ModuleItem
        removeMIDeclDir (MIDecl (Variable _ t x a me)) =
            MIDecl $ Variable Local t x a me
        removeMIDeclDir other = other
        removeModport :: ModuleItem -> ModuleItem
        removeModport (Modport x _) = Comment $ "removed modport " ++ x
        removeModport other = other
