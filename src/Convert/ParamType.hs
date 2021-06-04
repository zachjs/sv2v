{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `parameter type` in module instantiations
 -}

module Convert.ParamType (convert) where

import Control.Monad.Writer.Strict
import Data.Either (isLeft)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type MaybeTypeMap = Map.Map Identifier (Maybe Type)
type Info = Map.Map Identifier MaybeTypeMap

type Instance = Map.Map Identifier (Type, IdentSet)
type Instances = Set.Set (Identifier, Instance)

type IdentSet = Set.Set Identifier
type UsageMap = [(Identifier, Set.Set Identifier)]

convert :: [AST] -> [AST]
convert files =
    files'''
    where
        info = execWriter $
            mapM (collectDescriptionsM collectDescriptionM) files
        (files', instancesRaw) = runWriter $ mapM
            (mapM $ traverseModuleItemsM $ convertModuleItemM info) files
        instances = Set.toList instancesRaw

        -- add type parameter instantiations
        files'' = map (concatMap explodeDescription) files'
        explodeDescription :: Description -> [Description]
        explodeDescription (part @ (Part _ _ _ _ name _ _)) =
            if null theseInstances then
                [part]
            else
                (:) part $
                filter (not . alreadyExists) $
                map (rewriteModule part) theseInstances
            where
                theseInstances = map snd $ filter ((== name) . fst) instances
                alreadyExists = (flip Map.member info) . moduleName
                moduleName :: Description -> Identifier
                moduleName = \(Part _ _ _ _ x _ _) -> x
        explodeDescription other = [other]

        -- remove source modules that are no longer needed
        files''' = map (filter keepDescription) files''
        (usageMapRaw, usedTypedModulesRaw) =
            execWriter $ mapM (mapM collectUsageInfoM) files''
        usageMap = Map.unionsWith Set.union $ map (uncurry Map.singleton)
            usageMapRaw
        usedTypedModules = Map.unionsWith Set.union $ map (uncurry
            Map.singleton) usedTypedModulesRaw
        collectUsageInfoM :: Description -> Writer (UsageMap, UsageMap) ()
        collectUsageInfoM (part @ (Part _ _ _ _ name _ _)) =
            tell (makeList used, makeList usedTyped)
            where
                makeList s = zip (Set.toList s) (repeat $ Set.singleton name)
                (usedUntyped, usedTyped) =
                    execWriter $ (collectModuleItemsM collectModuleItemM) part
                used = Set.union usedUntyped usedTyped
        collectUsageInfoM _ = return ()
        collectModuleItemM :: ModuleItem -> Writer (IdentSet, IdentSet) ()
        collectModuleItemM (Instance m bindings _ _ _) = do
            case Map.lookup m info of
                Nothing -> tell (Set.singleton m, Set.empty)
                Just maybeTypeMap ->
                    if any (flip Map.member maybeTypeMap) $ map fst bindings
                        then tell (Set.empty, Set.singleton m)
                        else tell (Set.singleton m, Set.empty)
        collectModuleItemM _ = return ()
        keepDescription :: Description -> Bool
        keepDescription (Part _ _ _ _ name _ _) =
            Map.notMember name info ||
            Map.null maybeTypeMap ||
            (Map.member name usedTypedModules && isUsed name)
            where maybeTypeMap = info Map.! name
        keepDescription _ = True

        isUsed :: Identifier -> Bool
        isUsed name =
            any (flip Map.notMember usedTypedModules) used
            where
                used = usageSet $ expandSet name
                expandSet :: Identifier -> IdentSet
                expandSet ident =
                    case ( Map.lookup ident usedTypedModules
                         , Map.lookup name usageMap) of
                        (Just x, _) -> x
                        (Nothing, Just x) -> x
                        _ -> Set.empty
                usageSet :: IdentSet -> IdentSet
                usageSet names =
                    if names' == names
                        then names
                        else usageSet names'
                    where names' =
                            Set.union names $
                            Set.unions $
                            Set.map expandSet names

        -- substitute in a particular instance's parameter types
        rewriteModule :: Description -> Instance -> Description
        rewriteModule part typeMap =
            Part attrs extern kw ml m' p (additionalParamItems ++ items')
            where
                Part attrs extern kw ml m p items = part
                m' = moduleInstanceName m typeMap
                items' = map rewriteModuleItem items
                rewriteModuleItem = traverseNestedModuleItems $ traverseNodes
                    rewriteExpr rewriteDecl rewriteType rewriteLHS rewriteStmt
                rewriteDecl :: Decl -> Decl
                rewriteDecl (ParamType Parameter x t) =
                    ParamType Localparam x $ rewriteType $
                    case Map.lookup x typeMap of
                        Nothing -> t
                        Just (t', _) -> t'
                rewriteDecl other =
                    traverseDeclTypes rewriteType $
                    traverseDeclExprs rewriteExpr other
                additionalParamItems = concatMap makeAddedParams $
                    Map.toList $ Map.map snd typeMap
                rewriteExpr :: Expr -> Expr
                rewriteExpr (orig @ (Dot (Ident x) y)) =
                    if x == m
                        then Dot (Ident m') y
                        else orig
                rewriteExpr other =
                    traverseExprTypes rewriteType $
                    traverseSinglyNestedExprs rewriteExpr other
                rewriteLHS :: LHS -> LHS
                rewriteLHS (orig @ (LHSDot (LHSIdent x) y)) =
                    if x == m
                        then LHSDot (LHSIdent m') y
                        else orig
                rewriteLHS other =
                    traverseLHSExprs rewriteExpr $
                    traverseSinglyNestedLHSs rewriteLHS other
                rewriteType :: Type -> Type
                rewriteType =
                    traverseTypeExprs rewriteExpr .
                    traverseSinglyNestedTypes rewriteType
                rewriteStmt :: Stmt -> Stmt
                rewriteStmt =
                    traverseStmtLHSs rewriteLHS .
                    traverseStmtExprs rewriteExpr .
                    traverseSinglyNestedStmts rewriteStmt

        makeAddedParams :: (Identifier, IdentSet) -> [ModuleItem]
        makeAddedParams (paramName, identSet) =
            map (MIPackageItem . Decl) $
            map toTypeParam idents ++ map toParam idents
            where
                idents = Set.toList identSet
                toParam :: Identifier -> Decl
                toParam ident =
                    Param Parameter typ name (RawNum 0)
                    where
                        typ = Alias (addedParamTypeName paramName ident) []
                        name = addedParamName paramName ident
                toTypeParam :: Identifier -> Decl
                toTypeParam ident = ParamType Parameter name UnknownType
                    where name = addedParamTypeName paramName ident

-- write down module parameter names and type parameters
collectDescriptionM :: Description -> Writer Info ()
collectDescriptionM (part @ (Part _ _ _ _ name _ _)) =
    tell $ Map.singleton name maybeTypeMap
    where
        params = execWriter $
            collectModuleItemsM (collectDeclsM collectDeclM) part
        maybeTypeMap = Map.fromList $
            map (\(x, y) -> (x, fromJust y)) $
            filter (isJust . snd) params
        collectDeclM :: Decl -> Writer [(Identifier, Maybe (Maybe Type))] ()
        collectDeclM (Param   Parameter _ x _) = tell [(x, Nothing)]
        collectDeclM (ParamType Parameter x v) =
            if v == UnknownType
                then tell [(x, Just Nothing)]
                else tell [(x, Just $ Just v)]
        collectDeclM _ = return ()
collectDescriptionM _ = return ()

-- generate a "unique" name for a particular module type instance
moduleInstanceName :: Identifier -> Instance -> Identifier
moduleInstanceName m inst = m ++ "_" ++ shortHash (m, inst)

-- checks where a type is sufficiently resolved to be substituted
isSimpleType :: Type -> Bool
isSimpleType typ =
    (not $ typeIsUnresolved typ) &&
    case typ of
        IntegerVector{} -> True
        IntegerAtom  {} -> True
        NonInteger   {} -> True
        Net          {} -> True
        Implicit     {} -> True
        Struct   _ fields _ -> all (isSimpleType . fst) fields
        Union    _ fields _ -> all (isSimpleType . fst) fields
        _ -> False

-- returns whether a top-level type contains any dimension queries or
-- hierarchical references
typeIsUnresolved :: Type -> Bool
typeIsUnresolved =
    getAny . execWriter . collectTypeExprsM
    (collectNestedExprsM collectUnresolvedExprM)
    where
        collectUnresolvedExprM :: Expr -> Writer Any ()
        collectUnresolvedExprM PSIdent{} = tell $ Any True
        collectUnresolvedExprM CSIdent{} = tell $ Any True
        collectUnresolvedExprM DimsFn {} = tell $ Any True
        collectUnresolvedExprM DimFn  {} = tell $ Any True
        collectUnresolvedExprM Dot    {} = tell $ Any True
        collectUnresolvedExprM _ = return ()

prepareTypeExprs :: Identifier -> Identifier -> Type -> (Type, (IdentSet, [Decl]))
prepareTypeExprs instanceName paramName =
    runWriter . traverseNestedTypesM
        (traverseTypeExprsM $ traverseNestedExprsM prepareExpr)
    where
        prepareExpr :: Expr -> Writer (IdentSet, [Decl]) Expr
        prepareExpr (e @ Call{}) = do
            tell (Set.empty, [decl])
            prepareExpr $ Ident x
            where
                decl = Param Localparam (TypeOf e) x e
                x = instanceName ++ "_sv2v_pfunc_" ++ shortHash e
        prepareExpr (Ident x) = do
            tell (Set.singleton x, [])
            return $ Ident $ paramName ++ '_' : x
        prepareExpr other = return other

addedParamName :: Identifier -> Identifier -> Identifier
addedParamName paramName var = paramName ++ '_' : var

addedParamTypeName :: Identifier -> Identifier -> Identifier
addedParamTypeName paramName var = paramName ++ '_' : var ++ "_type"

-- attempt to rewrite instantiations with type parameters
convertModuleItemM :: Info -> ModuleItem -> Writer Instances ModuleItem
convertModuleItemM info (orig @ (Instance m bindings x r p)) =
    if Map.notMember m info then
        return orig
    else if Map.null maybeTypeMap then
        return orig
    else if any (isLeft . snd) bindings' then
        error $ "param type resolution left type params: " ++ show orig
            ++ " converted to: " ++ show bindings'
    else if any (not . isSimpleType . fst) resolvedTypes then
        return orig
    else do
        tell $ Set.singleton (m, resolvedTypes)
        let m' = moduleInstanceName m resolvedTypes
        return $ Generate $ map GenModuleItem $
            map (MIPackageItem . Decl) addedDecls ++
            [Instance m' (additionalBindings ++ bindings') x r p]
    where
        maybeTypeMap = info Map.! m
        -- determine the types corresponding to each type parameter
        bindingsMap = Map.fromList bindings
        resolvedTypesWithDecls = Map.mapMaybeWithKey resolveType bindingsMap
        resolvedTypes = Map.map (\(a, (b, _)) -> (a, b)) resolvedTypesWithDecls
        addedDecls = concatMap (snd . snd . snd) $
            Map.toList resolvedTypesWithDecls
        resolveType :: Identifier -> TypeOrExpr -> Maybe (Type, (IdentSet, [Decl]))
        resolveType _ Right{} = Nothing
        resolveType paramName (Left t) =
            Just $ prepareTypeExprs x paramName t

        -- leave only the normal expression params behind
        isParamType = flip Map.member maybeTypeMap
        bindings' = filter (not . isParamType . fst) bindings

        -- create additional parameters needed to specify existing type params
        additionalBindings = concatMap makeAddedParams $
            Map.toList $ Map.map snd resolvedTypes
        makeAddedParams :: (Identifier, IdentSet) -> [ParamBinding]
        makeAddedParams (paramName, identSet) =
            map toTypeParam idents ++ map toParam idents
            where
                idents = Set.toList identSet
                toParam :: Identifier -> ParamBinding
                toParam ident =
                    (addedParamName paramName ident, Right $ Ident ident)
                toTypeParam :: Identifier -> ParamBinding
                toTypeParam ident =
                    (addedParamTypeName paramName ident, Left $ TypeOf $ Ident ident)

convertModuleItemM _ other = return other
