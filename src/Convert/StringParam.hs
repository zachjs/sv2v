{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for variable-length string parameters
 -}

module Convert.StringParam (convert) where

import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type PartStringParams = Map.Map Identifier [(Identifier, Int)]

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
    if null stringParamNames
        then return $ Part attrs extern kw lifetime name ports items
        else do
            tell $ Map.singleton name stringParamIds
            return $ Part attrs extern kw lifetime name ports items'
    where
        (items', stringParamNames) = runWriter $
            mapM (traverseNestedModuleItemsM traverseModuleItemM) items
        allParamNames = parameterNames items
        stringParamIds = filter (flip elem stringParamNames . fst) $
            zip allParamNames [0..]
traverseDescriptionM other = return other

-- given a list of module items, produces the parameter names in order
parameterNames :: [ModuleItem] -> [Identifier]
parameterNames =
    execWriter . mapM (collectNestedModuleItemsM $ collectDeclsM collectDeclM)
    where
        collectDeclM :: Decl -> Writer [Identifier] ()
        collectDeclM (Param Parameter   _ x _) = tell [x]
        collectDeclM (ParamType Parameter x _) = tell [x]
        collectDeclM _ = return ()

pattern UnknownType :: Type
pattern UnknownType = Implicit Unspecified []

-- rewrite an existing string parameter
traverseModuleItemM :: ModuleItem -> Writer [Identifier] ModuleItem
traverseModuleItemM (orig @ (MIPackageItem (Decl (Param Parameter t x e)))) =
    case (t, e) of
        (UnknownType, String str) -> do
            tell [x]
            return $ Generate $ map wrap [width str, param str]
            where wrap = GenModuleItem . MIPackageItem . Decl
        _ -> return orig
    where
        w = widthName x
        r = (BinOp Sub (Ident w) (RawNum 1), RawNum 0)
        t' =  IntegerVector TBit Unspecified [r]
        defaultWidth str = DimsFn FnBits $ Right $ String str
        width str = Param Parameter UnknownType w (defaultWidth str)
        param str = Param Parameter t' x (String str)
traverseModuleItemM other = return other

widthName :: Identifier -> Identifier
widthName paramName = "_sv2v_width_" ++ paramName

-- convert isntances which use the converted string parameters
mapInstance :: PartStringParams -> ModuleItem -> ModuleItem
mapInstance partStringParams (Instance m params x rs ports) =
    case Map.lookup m partStringParams of
        Nothing -> Instance m params x rs ports
        Just stringParams -> Instance m params' x rs ports
            where params' = concat $ zipWith (expand stringParams) params [0..]
    where
        expand :: [(Identifier, Int)] -> ParamBinding -> Int -> [ParamBinding]
        expand _ (paramName, Left t) _ = [(paramName, Left t)]
        expand stringParams (orig @ ("", Right expr)) idx =
            if elem idx $ map snd stringParams
                then [("", Right width), orig]
                else [orig]
            where width = DimsFn FnBits $ Right expr
        expand stringParams (orig @ (paramName, Right expr)) _ =
            if elem paramName $ map fst stringParams
                then [(widthName paramName, Right width), orig]
                else [orig]
            where width = DimsFn FnBits $ Right expr
mapInstance _ other = other
