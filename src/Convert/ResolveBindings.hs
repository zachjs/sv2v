{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `.*` and unnamed bindings
 -
 - While positional bindings need not be converted, resolving them here
 - simplifies downstream conversions. This conversion is also responsible for
 - performing some basic validation and resolving the ambiguity between types
 - and expressions in parameter binding contexts.
 -}

module Convert.ResolveBindings
    ( convert
    , exprToType
    , resolveBindings
    ) where

import Control.Monad.Writer.Strict
import Data.List (intercalate, (\\))
import qualified Data.Map.Strict as Map

import Convert.ExprUtils (simplify)
import Convert.Traverse
import Language.SystemVerilog.AST

type Parts = Map.Map Identifier ([(Identifier, Bool)], [Identifier])

convert :: [AST] -> [AST]
convert =
    traverseFiles
        (collectDescriptionsM collectPartsM)
        (traverseDescriptions . traverseModuleItems . mapInstance)

collectPartsM :: Description -> Writer Parts ()
collectPartsM (Part _ _ _ _ name ports items) =
    tell $ Map.singleton name (params, ports)
    where params = parameterInfos items
collectPartsM _ = return ()

-- given a list of module items, produces the parameters in order
parameterInfos :: [ModuleItem] -> [(Identifier, Bool)]
parameterInfos =
    execWriter . mapM (collectNestedModuleItemsM $ collectDeclsM collectDeclM)
    where
        collectDeclM :: Decl -> Writer [(Identifier, Bool)] ()
        collectDeclM (Param Parameter   _ x _) = tell [(x, False)]
        collectDeclM (ParamType Parameter x _) = tell [(x, True)]
        collectDeclM _ = return ()

mapInstance :: Parts -> ModuleItem -> ModuleItem
mapInstance parts (Instance m paramBindings x rs portBindings) =
    -- if we can't find it, just skip :(
    if maybePartInfo == Nothing
        then Instance m paramBindings x rs portBindings
        else Instance m paramBindings' x rs portBindings'
    where
        maybePartInfo = Map.lookup m parts
        Just (paramInfos, portNames) = maybePartInfo
        paramNames = map fst paramInfos

        msg :: String -> String
        msg = flip (++) $ " in instance " ++ show x ++ " of " ++ show m

        paramBindings' = map checkParam $
            resolveBindings (msg "parameter overrides") paramNames paramBindings
        portBindings' = filter ((/= Nil) . snd) $
            resolveBindings (msg "port connections") portNames $
            concatMap expandStar portBindings

        expandStar :: PortBinding -> [PortBinding]
        expandStar ("*", Nil) =
            map (\port -> (port, Ident port)) $
            filter (flip notElem alreadyBound) portNames
            where alreadyBound = map fst portBindings
        expandStar other = [other]

        -- ensures parameter and binding kinds (type vs. expr) match
        checkParam :: ParamBinding -> ParamBinding
        checkParam (paramName, Right e) =
            (paramName, ) $
            if isType
                then case exprToType e of
                    Nothing -> kindMismatch paramName "a type" "expression" e
                    Just t -> Left t
                else Right e
            where Just isType = lookup paramName paramInfos
        checkParam (paramName, Left t) =
            if isType
                then (paramName, Left t)
                else kindMismatch paramName "an expression" "type" t
            where Just isType = lookup paramName paramInfos

        kindMismatch :: Show k => Identifier -> String -> String -> k -> a
        kindMismatch paramName expected actual value =
            error $ msg ("parameter " ++ show paramName)
                ++ " expects " ++ expected
                ++ ", but was given " ++ actual
                ++ ' ' : show value

mapInstance _ other = other

-- attempt to convert an expression to syntactically equivalent type
exprToType :: Expr -> Maybe Type
exprToType (Ident       x) = Just $ Alias       x []
exprToType (PSIdent y   x) = Just $ PSAlias y   x []
exprToType (CSIdent y p x) = Just $ CSAlias y p x []
exprToType (Range e NonIndexed r) = do
    (tf, rs) <- fmap typeRanges $ exprToType e
    Just $ tf (rs ++ [r])
exprToType (Bit e i) = do
    (tf, rs) <- fmap typeRanges $ exprToType e
    let r = (simplify $ BinOp Sub i (RawNum 1), RawNum 0)
    Just $ tf (rs ++ [r])
exprToType _ = Nothing

type Binding t = (Identifier, t)
-- give a set of bindings explicit names
resolveBindings :: String -> [Identifier] -> [Binding t] -> [Binding t]
resolveBindings _ _ [] = []
resolveBindings location available bindings =
    if length available < length bindings then
        error $ "too many bindings specified for " ++ location
    else if null $ fst $ head bindings then
        zip available $ map snd bindings
    else if not $ null unknowns then
        error $ "unknown binding" ++ unknownsPlural ++ " "
            ++ unknownsStr ++ " specified for " ++ location
    else
        bindings
    where
        unknowns = map fst bindings \\ available
        unknownsPlural = if length unknowns == 1 then "" else "s"
        unknownsStr = intercalate ", " $ map show unknowns
