{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for parameters without default values
 -
 - This conversion ensures that any parameters which don't have a default value
 - are always given an explicit value wherever that module or interface is used.
 -
 - Parameters are given a fake default value if they do not have one so that the
 - given source for that module can be consumed by downstream tools. This is not
 - done for type parameters, as those modules are rewritten by a separate
 - conversion. Localparams without defaults are expressly caught and forbidden.
 -}

module Convert.ParamNoDefault (convert) where

import Control.Monad (when)
import Control.Monad.Writer.Strict
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Parts = Map.Map Identifier [(Identifier, Bool)]

convert :: [Identifier] -> [AST] -> [AST]
convert tops files =
    flip (foldr $ ensureTopExists parts) tops $
    map convertFile files'
    where
        (files', parts) = runWriter $
            mapM (traverseDescriptionsM $ traverseDescriptionM tops) files
        convertFile = traverseDescriptions $ traverseModuleItems $
            traverseModuleItem parts

ensureTopExists :: Parts -> Identifier -> a -> a
ensureTopExists parts top =
    if Map.member top parts
        then id
        else error $ "Could not find top module " ++ top

traverseDescriptionM :: [Identifier] -> Description -> Writer Parts Description
traverseDescriptionM tops (Part attrs extern kw lifetime name ports items) = do
    let (items', params) = runWriter $ mapM traverseModuleItemM items
    tell $ Map.singleton name params
    let missing = map fst $ filter snd params
    when (not (null tops) && elem name tops && not (null missing)) $
        error $ "Specified top module " ++ name ++ " is missing default "
            ++ "parameter value(s) for " ++ intercalate ", " missing
    return $ Part attrs extern kw lifetime name ports items'
traverseDescriptionM _ other = return other

traverseModuleItemM :: ModuleItem -> Writer [(Identifier, Bool)] ModuleItem
traverseModuleItemM (MIAttr attr item) =
    traverseModuleItemM item >>= return . MIAttr attr
traverseModuleItemM (MIPackageItem (Decl decl)) =
    traverseDeclM decl >>= return . MIPackageItem . Decl
traverseModuleItemM other = return other

-- writes down the parameters for a part
traverseDeclM :: Decl -> Writer [(Identifier, Bool)] Decl
traverseDeclM (Param Localparam _ x Nil) =
    error $ "localparam " ++ show x ++ " has no default value"
traverseDeclM (Param Parameter t x e) = do
    tell [(x, e == Nil)]
    return $ if e == Nil
        then Param Parameter t x $ RawNum 0
        else Param Parameter t x e
traverseDeclM (ParamType Localparam x UnknownType) =
    error $ "localparam type " ++ show x ++ " has no default value"
traverseDeclM (ParamType Parameter x t) = do
    -- parameter types are rewritten separately, so no fake default here
    tell [(x, t == UnknownType)]
    return $ ParamType Parameter x t
traverseDeclM other = return other

-- check for instances missing values for parameters without defaults
traverseModuleItem :: Parts -> ModuleItem -> ModuleItem
traverseModuleItem parts orig@(Instance part params name _ _) =
    if maybePartInfo == Nothing || null missingParams
        then orig
        else error $ "instance " ++ show name ++ " of " ++ show part
                ++ " is missing values for parameters without defaults: "
                ++ (intercalate " " $ map show missingParams)
    where
        maybePartInfo = Map.lookup part parts
        Just partInfo = maybePartInfo
        paramsWithNoDefault = map fst $ filter snd partInfo
        missingParams = filter (needsDefault params) paramsWithNoDefault
traverseModuleItem _ other = other

-- whether a given parameter is unspecified in the given parameter bindings
needsDefault :: [(Identifier, TypeOrExpr)] -> Identifier -> Bool
needsDefault instanceParams param =
    case lookup param instanceParams of
        Nothing -> True
        Just (Right Nil) -> True
        Just _ -> False
