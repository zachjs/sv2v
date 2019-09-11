{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `parameter type` in module instantiations
 -}

module Convert.ParamType (convert) where

import Control.Monad.Writer
import Data.Either (isLeft)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type MaybeTypeMap = Map.Map Identifier (Maybe Type)
type Info = Map.Map Identifier ([Identifier], MaybeTypeMap)

type Instance = Map.Map Identifier Type
type Instances = [(Identifier, Instance)]

convert :: [AST] -> [AST]
convert files =
    concatMap (map explodeDescription) files'
    where
        info = execWriter $
            mapM (collectDescriptionsM collectDescriptionM) files
        (files', instancesRaw) = runWriter $ mapM
            (mapM $ traverseModuleItemsM $ mapInstance info) files
        instances = reverse $ uniq [] instancesRaw
        -- TODO: use the unique package
        uniq curr [] = curr
        uniq curr (x : xs) =
            if elem x curr
                then uniq curr xs
                else uniq (x : curr) xs

        explodeDescription :: Description -> [Description]
        explodeDescription (part @ (Part _ _ _ name _ _)) =
            if null theseInstances
                then [part]
                else map (rewriteModule part) theseInstances
            where theseInstances = map snd $ filter ((== name) . fst) instances
        explodeDescription other = [other]

        -- TODO FIXME: Need to keep around the default instance and not perform
        -- substitutions in it.

        -- substitute in a particular instance's paramter types
        rewriteModule :: Description -> Instance -> Description
        rewriteModule part typeMap =
            Part extern kw ml m' p items'
            where
                Part extern kw ml m p items = part
                m' = renameModule info m typeMap
                items' = map rewriteDecl items
                rewriteDecl :: ModuleItem -> ModuleItem
                rewriteDecl (MIPackageItem (Decl (ParamType Parameter x _))) =
                    MIPackageItem $ Typedef (typeMap Map.! x) x
                rewriteDecl other = other
                -- TODO FIXME: Typedef conversion must be made to handle
                -- ParamTypes!
                -----items' = map (traverseDecls rewriteDecl) items
                -----rewriteDecl :: Decl -> Decl
                -----rewriteDecl (ParamType Parameter x _) =
                -----    ParamType Localparam x (Just $ typeMap Map.! x)
                -----rewriteDecl other = other


-- write down module parameter names and type parameters
collectDescriptionM :: Description -> Writer Info ()
collectDescriptionM (part @ (Part _ _ _ name _ _)) =
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
        else Just $ Map.map fromJust maybeTypeMap

-- generate a "unique" name for a particular module type instance
renameModule :: Info -> Identifier -> Instance -> Identifier
renameModule info m inst =
    if defaultInstance maybeTypeMap == Just inst
        then m -- default instances keep the original module name
        else m ++ "_" ++ shortHash (m, inst)
    where maybeTypeMap = snd $ info Map.! m


mapInstance :: Info -> ModuleItem -> Writer Instances ModuleItem
mapInstance info (orig @ (Instance m bindings x r p)) =
    if Map.notMember m info then
        return orig
    else if any (isLeft . snd) bindings' then
        error $ "param type resolution left type params: " ++ show orig
            ++ " converted to: " ++ show bindings'
    else do
        tell [(m, resolvedTypes)]
        let m' = renameModule info m resolvedTypes
        return $ Instance m' bindings' x r p
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
        resolveType :: Identifier -> Maybe Type -> Type
        resolveType paramName defaultType =
            case (Map.lookup paramName bindingsMap, defaultType) of
                (Nothing, Just t) -> t
                (Nothing, Nothing) ->
                    error $ "instantiation " ++ show orig ++
                        " is missing a type parameter: " ++ paramName
                (Just (Left t), _) -> t
                (Just (Right e), _) ->
                    -- TODO: Some types could have been parsed as an expression
                    -- (i.e. aliases). Ideally we should have any such aliases
                    -- resolved before applying this conversion.
                    error $ "instantiation " ++ show orig ++ " has expr "
                        ++ show e ++ " for type param: " ++ paramName
        -- leave only the normal expression params behind
        isParamType = flip Map.member maybeTypeMap
        bindings' = filter (not . isParamType . fst) bindingsNamed
mapInstance _ other = return other
