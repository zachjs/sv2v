{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `parameter type` in module instantiations
 -}

module Convert.ParamType (convert) where

import Control.Monad.Writer.Strict
import Data.Either (isRight, lefts)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type TypeMap = Map.Map Identifier Type
type Modules = Map.Map Identifier TypeMap

type Instance = Map.Map Identifier (Type, IdentSet)
type Instances = Map.Map String (Identifier, Instance)

type IdentSet = Set.Set Identifier
type DeclMap = Map.Map Identifier Decl
type UsageMap = [(Identifier, Set.Set Identifier)]

convert :: [AST] -> [AST]
convert files =
    files'''
    where
        modules = execWriter $
            mapM (collectDescriptionsM collectDescriptionM) files
        (files', instancesRaw) =
            runWriter $ mapM (mapM convertDescriptionM) files
        instances = Map.elems instancesRaw

        -- add type parameter instantiations
        files'' = map (concatMap explodeDescription) files'
        explodeDescription :: Description -> [Description]
        explodeDescription part@(Part _ _ _ _ name _ _) =
            (part :) $
            filter (not . alreadyExists) $
            map (rewriteModule part) theseInstances
            where
                theseInstances = map snd $ filter ((== name) . fst) instances
                alreadyExists = flip Map.member modules . moduleName
                moduleName :: Description -> Identifier
                moduleName = \(Part _ _ _ _ x _ _) -> x
        explodeDescription other = [other]

        -- remove or reduce source modules that are no longer needed
        files''' = map (map reduceTypeDefaults . filter keepDescription) files''
        -- produce a typed and untyped instantiation graph
        (usedUntypedModules, usedTypedModules) =
            both (Map.fromListWith Set.union) $
            execWriter $ mapM (mapM collectUsageM) files''
        collectUsageM :: Description -> Writer (UsageMap, UsageMap) ()
        collectUsageM part@(Part _ _ _ _ name _ _) =
            tell $ both makeList $ execWriter $
                (collectModuleItemsM collectModuleItemM) part
            where makeList s = zip (Set.toList s) (repeat $ Set.singleton name)
        collectUsageM _ = return ()
        collectModuleItemM :: ModuleItem -> Writer (IdentSet, IdentSet) ()
        collectModuleItemM (Instance m bindings _ _ _) =
            if all (isRight . snd) bindings
                then tell (Set.singleton m, Set.empty)
                else tell (Set.empty, Set.singleton m)
        collectModuleItemM _ = return ()
        both f (x, y) = (f x, f y) -- simple tuple map helper

        -- identify if a module is still in use
        keepDescription :: Description -> Bool
        keepDescription (Part _ _ _ _ name _ _) =
            isNewModule
            || isntTyped
            || isUsedAsUntyped
            || isUsedAsTyped && isInstantiatedViaNonTyped
            || allTypesHaveDefaults && notInstantiated && isntTemplateTagged
            where
                maybeTypeMap = Map.lookup name modules
                Just typeMap = maybeTypeMap
                isNewModule = maybeTypeMap == Nothing
                isntTyped = Map.null typeMap
                isUsedAsTyped = Map.member name usedTypedModules
                isUsedAsUntyped = Map.member name usedUntypedModules
                isInstantiatedViaNonTyped = untypedUsageSearch $ Set.singleton name
                allTypesHaveDefaults = all (/= UnknownType) (Map.elems typeMap)
                notInstantiated = lookup name instances == Nothing
                isntTemplateTagged = not $ isTemplateTagged name
        keepDescription _ = True

        -- instantiate the type parameters if this is a used default instance
        reduceTypeDefaults :: Description -> Description
        reduceTypeDefaults part@(Part _ _ _ _ name _ _) =
            if shouldntReduce
                then part
                else traverseModuleItems (traverseDecls rewriteDecl) part
            where
                shouldntReduce =
                    Map.notMember name modules || Map.null typeMap ||
                    any (== UnknownType) (Map.elems typeMap) ||
                    isTemplateTagged name
                typeMap = modules Map.! name
                rewriteDecl :: Decl -> Decl
                rewriteDecl (ParamType Parameter x t) =
                    ParamType Localparam x t
                rewriteDecl other = other
        reduceTypeDefaults other = other

        -- modules can be recursive; this checks if a typed module is not
        -- connected to any modules which are themselves used as typed modules
        untypedUsageSearch :: IdentSet -> Bool
        untypedUsageSearch visited =
            any (flip Map.notMember usedTypedModules) visited
            || Set.size visited /= Set.size visited'
                && untypedUsageSearch visited'
            where
                visited' =
                    Set.union visited $
                    Set.unions $
                    Set.map expandSet visited
                expandSet :: Identifier -> IdentSet
                expandSet ident =
                    Map.findWithDefault Set.empty ident usedTypedModules

        -- substitute in a particular instance's parameter types
        rewriteModule :: Description -> Instance -> Description
        rewriteModule part inst =
            Part attrs extern kw ml m' p (additionalParamItems ++ items')
            where
                Part attrs extern kw ml m p items = part
                m' = moduleInstanceName m inst
                items' = map rewriteModuleItem items
                rewriteModuleItem = traverseNestedModuleItems $ traverseNodes
                    rewriteExpr rewriteDecl rewriteType rewriteLHS rewriteStmt
                rewriteDecl :: Decl -> Decl
                rewriteDecl (ParamType Parameter x t) =
                    ParamType kind x $ rewriteType $
                    case Map.lookup x inst of
                        Nothing -> t
                        Just (t', _) -> t'
                    where kind = if Map.null inst
                                    then Parameter
                                    else Localparam
                rewriteDecl other =
                    traverseDeclNodes rewriteType rewriteExpr other
                additionalParamItems = concatMap makeAddedParams $
                    Map.toList $ Map.map snd inst
                rewriteExpr :: Expr -> Expr
                rewriteExpr orig@(Dot (Ident x) y) =
                    if x == m
                        then Dot (Ident m') y
                        else orig
                rewriteExpr other =
                    traverseExprTypes rewriteType $
                    traverseSinglyNestedExprs rewriteExpr other
                rewriteLHS :: LHS -> LHS
                rewriteLHS orig@(LHSDot (LHSIdent x) y) =
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
collectDescriptionM :: Description -> Writer Modules ()
collectDescriptionM part@(Part _ _ _ _ name _ _) =
    tell $ Map.singleton name typeMap
    where
        typeMap = Map.fromList $ execWriter $
            collectModuleItemsM (collectDeclsM collectDeclM) part
        collectDeclM :: Decl -> Writer [(Identifier, Type)] ()
        collectDeclM (ParamType Parameter x v) = tell [(x, v)]
        collectDeclM _ = return ()
collectDescriptionM _ = return ()

-- generate a "unique" name for a particular module type instance
moduleInstanceName :: Identifier -> Instance -> Identifier
moduleInstanceName (TemplateTag m) inst =
    moduleInstanceName m inst
moduleInstanceName m inst =
    if Map.null inst
        then TemplateTag m
        else m ++ "_" ++ shortHash (m, inst)

-- used to tag modules created for delayed type parameter instantiation
pattern TemplateTag :: Identifier -> Identifier
pattern TemplateTag x = '~' : x
isTemplateTagged :: Identifier -> Bool
isTemplateTagged TemplateTag{} = True
isTemplateTagged _ = False

-- checks where a type is sufficiently resolved to be substituted
isSimpleType :: Type -> Bool
isSimpleType typ =
    (not $ typeIsUnresolved typ) &&
    case typ of
        IntegerVector{} -> True
        IntegerAtom  {} -> True
        NonInteger   {} -> True
        Implicit     {} -> True
        Struct _ fields _ -> all (isSimpleType . fst) fields
        Union  _ fields _ -> all (isSimpleType . fst) fields
        _ -> False

-- returns whether a top-level type contains any dimension queries or
-- hierarchical references
typeIsUnresolved :: Type -> Bool
typeIsUnresolved =
    getAny . execWriter . collectTypeExprsM
    (collectNestedExprsM collectUnresolvedExprM)
    where
        collectUnresolvedExprM :: Expr -> Writer Any ()
        collectUnresolvedExprM DimsFn {} = tell $ Any True
        collectUnresolvedExprM DimFn  {} = tell $ Any True
        collectUnresolvedExprM Dot    {} = tell $ Any True
        collectUnresolvedExprM _ = return ()

prepareTypeExprs :: Identifier -> Identifier -> Type -> (Type, (IdentSet, DeclMap))
prepareTypeExprs instanceName paramName =
    runWriter . traverseNestedTypesM
        (traverseTypeExprsM $ traverseNestedExprsM prepareExpr)
    where
        prepareExpr :: Expr -> Writer (IdentSet, DeclMap) Expr
        prepareExpr e@Call{} = do
            tell (Set.empty, Map.singleton x decl)
            prepareExpr $ Ident x
            where
                decl = Param Localparam (TypeOf e) x e
                x = instanceName ++ "_sv2v_pfunc_" ++ shortHash e
        prepareExpr (Ident x) = do
            tell (Set.singleton x, Map.empty)
            return $ Ident $ paramName ++ '_' : x
        prepareExpr other = return other

addedParamName :: Identifier -> Identifier -> Identifier
addedParamName paramName var = paramName ++ '_' : var

addedParamTypeName :: Identifier -> Identifier -> Identifier
addedParamTypeName paramName var = paramName ++ '_' : var ++ "_type"

convertDescriptionM :: Description -> Writer Instances Description
convertDescriptionM (Part attrs extern kw liftetime name ports items) =
    mapM convertModuleItemM items >>=
        return . Part attrs extern kw liftetime name ports
convertDescriptionM other = return other

convertGenItemM :: GenItem -> Writer Instances GenItem
convertGenItemM (GenModuleItem item) =
    convertModuleItemM item >>= return . GenModuleItem
convertGenItemM other =
    traverseSinglyNestedGenItemsM convertGenItemM other

-- attempt to rewrite instantiations with type parameters
convertModuleItemM :: ModuleItem -> Writer Instances ModuleItem
convertModuleItemM orig@(Instance m bindings x r p) =
    if hasOnlyExprs then
        return orig
    else if not hasUnresolvedTypes then do
        let m' = moduleInstanceName m resolvedTypes
        tell $ Map.singleton m' (m, resolvedTypes)
        return $ Generate $ map GenModuleItem $
            map (MIPackageItem . Decl) addedDecls ++
            [Instance m' (additionalBindings ++ exprBindings) x r p]
    else if isTemplateTagged m then
        return orig
    else do
        let m' = TemplateTag m
        tell $ Map.singleton m' (m, Map.empty)
        return $ Instance m' bindings x r p
    where
        hasOnlyExprs = all (isRight . snd) bindings
        hasUnresolvedTypes = any (not . isSimpleType) (lefts $ map snd bindings)

        -- determine the types corresponding to each type parameter
        bindingsMap = Map.fromList bindings
        resolvedTypesWithDecls = Map.mapMaybeWithKey resolveType bindingsMap
        resolvedTypes = Map.map (\(a, (b, _)) -> (a, b)) resolvedTypesWithDecls
        addedDecls = Map.elems $ Map.unions $ map (snd . snd) $
            Map.elems resolvedTypesWithDecls
        resolveType :: Identifier -> TypeOrExpr -> Maybe (Type, (IdentSet, DeclMap))
        resolveType _ Right{} = Nothing
        resolveType paramName (Left t) =
            Just $ prepareTypeExprs x paramName t

        -- leave only the normal expression params behind
        exprBindings = filter (isRight . snd) bindings

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

convertModuleItemM (Generate items) =
    mapM convertGenItemM items >>= return . Generate
convertModuleItemM (MIAttr attr item) =
    convertModuleItemM item >>= return . MIAttr attr
convertModuleItemM other = return other
