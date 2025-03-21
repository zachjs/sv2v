{-# LANGUAGE FlexibleInstances #-}
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
    , resolveBindings
    ) where

import Control.Monad.Writer.Strict
import Data.List (intercalate, (\\))
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

data PartInfo = PartInfo PartKW [(Identifier, Bool)] [Identifier]
type Parts = Map.Map Identifier PartInfo

convert :: [AST] -> [AST]
convert =
    traverseFiles
        (collectDescriptionsM collectPartsM)
        (traverseDescriptions . traverseModuleItems . mapInstance)

collectPartsM :: Description -> Writer Parts ()
collectPartsM (Part _ _ kw _ name ports items) =
    tell $ Map.singleton name $ PartInfo kw params ports
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
    if isNothing maybePartInfo
        then Instance m paramBindings x rs portBindings
        else Instance m paramBindings' x rs portBindings'
    where
        maybePartInfo = Map.lookup m parts
        Just (PartInfo kw paramInfos portNames) = maybePartInfo
        paramNames = map fst paramInfos

        msg :: String -> String
        msg = flip (++) $ " in instance " ++ show x ++ " of " ++ show kw ++ " "
            ++ show m

        paramBindings' = map checkParam $
            resolveBindings (msg "parameter overrides") paramNames paramBindings
        portBindings' = resolveBindings (msg "port connections") portNames $
            concatMap expandStar $
            -- drop the trailing comma in positional port bindings
            if length portNames + 1 == length portBindings
                && last portBindings == ("", Nil)
                then init portBindings
                else portBindings

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

class BindingArg k where
    showBinding :: k -> String

instance BindingArg TypeOrExpr where
    showBinding = either show show

instance BindingArg Expr where
    showBinding = show

type Binding t = (Identifier, t)
-- give a set of bindings explicit names
resolveBindings
    :: BindingArg t => String -> [Identifier] -> [Binding t] -> [Binding t]
resolveBindings _ _ [] = []
resolveBindings location available bindings@(("", _) : _) =
    if length available < length bindings then
        error $ "too many bindings specified for " ++ location ++ ": "
            ++ describeList "specified" (map (showBinding . snd) bindings)
            ++ ", but only " ++ describeList "available" (map show available)
    else
        zip available $ map snd bindings
resolveBindings location available bindings =
    if not $ null unknowns then
        error $ "unknown binding" ++ unknownsPlural ++ " "
            ++ unknownsStr ++ " specified for " ++ location ++ ", "
            ++ describeList "available" (map show available)
    else
        bindings
    where
        unknowns = map fst bindings \\ available
        unknownsPlural = if length unknowns == 1 then "" else "s"
        unknownsStr = intercalate ", " $ map show unknowns

describeList :: String -> [String] -> String
describeList desc [] = "0 " ++ desc
describeList desc xs =
    show (length xs) ++ " " ++ desc ++ " (" ++ xsStr ++ ")"
    where xsStr = intercalate ", " xs
