{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for variable-length string parameters
 -
 - While implicitly variable-length string parameters are supported in
 - Verilog-2005, some usages depend on their type information (e.g., size). In
 - such instances, an additional parameter is added encoding the width of the
 - parameter.
 -}

module Convert.StringParam (convert) where

import Control.Monad.Writer.Strict
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type PartStringParams = Map.Map Identifier [Identifier]
type Idents = Set.Set Identifier

convert :: [AST] -> [AST]
convert files =
    if Map.null partStringParams
        then files
        else map traverseModuleItem files'
    where
        (files', partStringParams) = runWriter $
            mapM (traverseDescriptionsM traverseDescriptionM) files
        traverseModuleItem = traverseDescriptions $ traverseModuleItems $
            mapInstance partStringParams

-- adds automatic width parameters for string parameters
traverseDescriptionM :: Description -> Writer PartStringParams Description
traverseDescriptionM (Part attrs extern kw lifetime name ports items) =
    if null candidateStringParams || Set.null stringParamNames
        then return $ Part attrs extern kw lifetime name ports items
        else do
            tell $ Map.singleton name $ Set.toList stringParamNames
            return $ Part attrs extern kw lifetime name ports items'
    where
        items' = map (elaborateStringParam stringParamNames) items
        candidateStringParams = mapMaybe candidateStringParam items
        stringParamNames = execWriter $
            mapM (collectNestedModuleItemsM collectModuleItemM) items
        collectModuleItemM = collectTypesM $ collectNestedTypesM $
            collectQueriedIdentsM $ Set.fromList candidateStringParams
traverseDescriptionM other = return other

-- utility pattern for candidate string parameter items
pattern StringParam :: Identifier -> String -> ModuleItem
pattern StringParam x s =
    MIPackageItem (Decl (Param Parameter UnknownType x (String s)))

-- write down which parameters may be variable-length strings
candidateStringParam :: ModuleItem -> Maybe Identifier
candidateStringParam (MIAttr _ item) = candidateStringParam item
candidateStringParam (StringParam x _) = Just x
candidateStringParam _ = Nothing

-- write down which of the given identifiers are subject to type queries
collectQueriedIdentsM :: Idents -> Type -> Writer Idents ()
collectQueriedIdentsM idents (TypeOf (Ident x)) =
    when (Set.member x idents) $ tell $ Set.singleton x
collectQueriedIdentsM _ _ = return ()

-- rewrite an existing string parameter
elaborateStringParam :: Idents -> ModuleItem -> ModuleItem
elaborateStringParam idents (MIAttr attr item) =
    MIAttr attr $ elaborateStringParam idents item
elaborateStringParam idents orig@(StringParam x str) =
    if Set.member x idents
        then Generate $ map wrap [width, param]
        else orig
    where
        wrap = GenModuleItem . MIPackageItem . Decl
        w = widthName x
        r = (BinOp Sub (Ident w) (RawNum 1), RawNum 0)
        t' =  IntegerVector TBit Unspecified [r]
        defaultWidth = DimsFn FnBits $ Right $ String str
        width = Param Parameter UnknownType w defaultWidth
        param = Param Parameter t' x (String str)
elaborateStringParam _ other = other

widthName :: Identifier -> Identifier
widthName paramName = "_sv2v_width_" ++ paramName

-- convert instances which use the converted string parameters
mapInstance :: PartStringParams -> ModuleItem -> ModuleItem
mapInstance partStringParams (Instance m params x rs ports) =
    case Map.lookup m partStringParams of
        Nothing -> Instance m params x rs ports
        Just stringParams -> Instance m params' x rs ports
            where params' = concatMap (expand stringParams) params
    where
        expand :: [Identifier] -> ParamBinding -> [ParamBinding]
        expand _ (paramName, Left t) = [(paramName, Left t)]
        expand stringParams orig@(paramName, Right expr) =
            if elem paramName stringParams
                then [(widthName paramName, Right width), orig]
                else [orig]
            where width = DimsFn FnBits $ Right expr
mapInstance _ other = other
