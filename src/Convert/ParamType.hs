{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `parameter type` in module instantiations
 -}

module Convert.ParamType (convert) where

import Control.Monad.Writer.Strict
import Data.Either (isLeft)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.ExprUtils
import Convert.Traverse
import Language.SystemVerilog.AST

type MaybeTypeMap = Map.Map Identifier (Maybe Type)
type Info = Map.Map Identifier ([Identifier], MaybeTypeMap)

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
                filter isNonDefault $
                map (rewriteModule part) theseInstances
            where
                theseInstances = map snd $ filter ((== name) . fst) instances
                isNonDefault = (name /=) . moduleName
                alreadyExists = (flip Map.member info) . moduleName
                moduleName :: Description -> Identifier
                moduleName (Part _ _ _ _ x _ _) = x
                moduleName _ = error "not possible"
        explodeDescription other = [other]

        -- remove or rewrite source modules that are no longer needed
        files''' = map (\a -> concatMap (replaceDefault a) a) files''
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
                Just (_, maybeTypeMap) ->
                    if any (flip Map.member maybeTypeMap) $ map fst bindings
                        then tell (Set.empty, Set.singleton m)
                        else tell (Set.singleton m, Set.empty)
        collectModuleItemM _ = return ()
        replaceDefault :: [Description] -> Description -> [Description]
        replaceDefault existing (part @ (Part _ _ _ _ name _ _)) =
            if Map.notMember name info then
                [part]
            else if Map.null maybeTypeMap then
                [part]
            else if Map.member name usedTypedModules && isUsed name then
                [part]
            else if all isNothing maybeTypeMap then
                []
            else
                filter (not . alreadyExists) $
                (:) (removeDefaultTypeParams part) $
                if isNothing typeMap
                    then []
                    else [rewriteModule part $ fromJust typeMap]
            where
                maybeTypeMap = snd $ info Map.! name
                typeMap = defaultInstance maybeTypeMap
                existingNames = map moduleName existing
                alreadyExists = (flip elem existingNames) . moduleName
                moduleName :: Description -> Identifier
                moduleName (Part _ _ _ _ x _ _) = x
                moduleName _ = ""
        replaceDefault _ other = [other]

        removeDefaultTypeParams :: Description -> Description
        removeDefaultTypeParams (part @ Part{}) =
            Part attrs extern kw ml (moduleDefaultName name) p items
            where
                Part attrs extern kw ml name p items =
                    traverseModuleItems (traverseDecls rewriteDecl) part
                rewriteDecl :: Decl -> Decl
                rewriteDecl (ParamType Parameter x _) =
                    ParamType Parameter x Nothing
                rewriteDecl other = other
        removeDefaultTypeParams _ = error "not possible"

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
                items' = map (traverseDecls rewriteDecl) items
                rewriteDecl :: Decl -> Decl
                rewriteDecl (ParamType Parameter x _) =
                    ParamType Localparam x (Just $ fst $ typeMap Map.! x)
                rewriteDecl other = other
                additionalParamItems = concatMap makeAddedParams $
                    Map.toList $ Map.map snd typeMap

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
                toTypeParam ident = ParamType Parameter name Nothing
                    where name = addedParamTypeName paramName ident

-- write down module parameter names and type parameters
collectDescriptionM :: Description -> Writer Info ()
collectDescriptionM (part @ (Part _ _ _ _ name _ _)) =
    tell $ Map.singleton name (paramNames, maybeTypeMap)
    where
        params = execWriter $
            collectModuleItemsM (collectDeclsM collectDeclM) part
        paramNames = map fst params
        maybeTypeMap = Map.fromList $
            map (\(x, y) -> (x, fromJust y)) $
            filter (isJust . snd) params
        collectDeclM :: Decl -> Writer [(Identifier, Maybe (Maybe Type))] ()
        collectDeclM (Param   Parameter _ x _) = tell [(x, Nothing)]
        collectDeclM (ParamType Parameter x v) = tell [(x, Just v )]
        collectDeclM _ = return ()
collectDescriptionM _ = return ()

-- produces the default type mapping of a module, if there is one
defaultInstance :: MaybeTypeMap -> Maybe Instance
defaultInstance maybeTypeMap =
    if any isNothing maybeTypeMap
        then Nothing
        else Just $ Map.map ((, Set.empty) . fromJust) maybeTypeMap

-- generate a "unique" name for a particular module type instance
moduleInstanceName :: Identifier -> Instance -> Identifier
moduleInstanceName m inst = m ++ "_" ++ shortHash (m, inst)

-- name for the module without any default type parameters
moduleDefaultName :: Identifier -> Identifier
moduleDefaultName m = m ++ defaultTag
isDefaultName :: Identifier -> Bool
isDefaultName m =
    defaultTag == (reverse $ (take $ length defaultTag) $ reverse m)
defaultTag :: Identifier
defaultTag = "_sv2v_default"

-- attempt to convert an expression to syntactically equivalent type
exprToType :: Expr -> Maybe Type
exprToType (Ident       x) = Just $ Alias       x []
exprToType (PSIdent y   x) = Just $ PSAlias y   x []
exprToType (CSIdent y p x) = Just $ CSAlias y p x []
exprToType (Range e NonIndexed r) =
    case exprToType e of
        Nothing -> Nothing
        Just t -> Just $ tf (rs ++ [r])
            where (tf, rs) = typeRanges t
exprToType (Bit e i) =
    case exprToType e of
        Nothing -> Nothing
        Just t -> Just $ tf (rs ++ [r])
            where
                (tf, rs) = typeRanges t
                r = (simplify $ BinOp Sub i (RawNum 1), RawNum 0)
exprToType _ = Nothing

-- checks where a type is sufficiently resolved to be substituted
isSimpleType :: Type -> Bool
isSimpleType typ =
    (not $ typeHasQueries typ) &&
    case typ of
        IntegerVector{} -> True
        IntegerAtom  {} -> True
        NonInteger   {} -> True
        Net          {} -> True
        Implicit     {} -> True
        Struct   _ fields _ -> all (isSimpleType . fst) fields
        Union    _ fields _ -> all (isSimpleType . fst) fields
        _ -> False

-- returns whether a top-level type contains any dimension queries
typeHasQueries :: Type -> Bool
typeHasQueries =
    not . null . execWriter . collectTypeExprsM
    (collectNestedExprsM collectUnresolvedExprM)
    where
        collectUnresolvedExprM :: Expr -> Writer [Expr] ()
        collectUnresolvedExprM (expr @ PSIdent{}) = tell [expr]
        collectUnresolvedExprM (expr @ CSIdent{}) = tell [expr]
        collectUnresolvedExprM (expr @ DimsFn{}) = tell [expr]
        collectUnresolvedExprM (expr @ DimFn {}) = tell [expr]
        collectUnresolvedExprM _ = return ()

prepareTypeIdents :: Identifier -> Type -> (Type, IdentSet)
prepareTypeIdents prefix =
    runWriter . traverseNestedTypesM
        (traverseTypeExprsM $ traverseNestedExprsM prepareExprIdents)
    where
        prepareExprIdents :: Expr -> Writer IdentSet Expr
        prepareExprIdents (Ident x) = do
            tell $ Set.singleton x
            return $ Ident $ prefix ++ '_' : x
        prepareExprIdents other = return other

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
    else if any (not . isSimpleType . fst) resolvedTypes then do
        let defaults = Map.map (Left . fst) resolvedTypes
        let bindingsDefaulted = Map.toList $ Map.union bindingsMap defaults
        if isDefaultName m || bindingsDefaulted == Map.toList bindingsMap
            then return $ Instance m bindingsNamed x r p
            else return $ Instance (moduleDefaultName m) bindingsDefaulted x r p
    else do
        tell $ Set.singleton (m, resolvedTypes)
        let m' = moduleInstanceName m resolvedTypes
        return $ Instance m' (additionalBindings ++ bindings') x r p
    where
        (paramNames, maybeTypeMap) = info Map.! m
        -- attach names to unnamed parameters
        bindingsNamed =
            if all (== "") (map fst bindings) then
                zip paramNames (map snd bindings)
            else if any (== "") (map fst bindings) then
                error $ "instance has a mix of named and unnamed params: "
                    ++ show orig
            else bindings
        -- determine the types corresponding to each type parameter
        bindingsMap = Map.fromList bindingsNamed
        resolvedTypes = Map.mapWithKey resolveType maybeTypeMap
        resolveType :: Identifier -> Maybe Type -> (Type, IdentSet)
        resolveType paramName defaultType =
            case (Map.lookup paramName bindingsMap, defaultType) of
                (Nothing, Just t) -> (t, Set.empty)
                (Nothing, Nothing) ->
                    error $ "instantiation " ++ show orig ++
                        " is missing a type parameter: " ++ paramName
                (Just (Left t), _) -> prepareTypeIdents paramName t
                (Just (Right e), _) ->
                    -- Some types are parsed as expressions because of the
                    -- ambiguities of defined type names.
                    case exprToType e of
                        Just t -> prepareTypeIdents paramName t
                        Nothing ->
                            error $ "instantiation " ++ show orig
                                ++ " has expr " ++ show e
                                ++ " for type param: " ++ paramName

        -- leave only the normal expression params behind
        isParamType = flip Map.member maybeTypeMap
        bindings' = filter (not . isParamType . fst) bindingsNamed

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
