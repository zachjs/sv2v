{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `.*` and unnamed bindings
 -
 - While positional bindings need not be converted, resolving them here
 - simplifies downstream conversions.
 -}

module Convert.ResolveBindings (convert) where

import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Ports = Map.Map Identifier ([Identifier], [Identifier])

convert :: [AST] -> [AST]
convert =
    traverseFiles
        (collectDescriptionsM collectPortsM)
        (traverseDescriptions . traverseModuleItems . mapInstance)

collectPortsM :: Description -> Writer Ports ()
collectPortsM (Part _ _ _ _ name ports items) =
    tell $ Map.singleton name (params, ports)
    where params = parameterNames items
collectPortsM _ = return ()

-- given a list of module items, produces the parameter names in order
parameterNames :: [ModuleItem] -> [Identifier]
parameterNames =
    execWriter . mapM (collectNestedModuleItemsM $ collectDeclsM collectDeclM)
    where
        collectDeclM :: Decl -> Writer [Identifier] ()
        collectDeclM (Param Parameter   _ x _) = tell [x]
        collectDeclM (ParamType Parameter x _) = tell [x]
        collectDeclM _ = return ()

mapInstance :: Ports -> ModuleItem -> ModuleItem
mapInstance modulePorts (Instance m paramBindings x rs portBindings) =
    -- if we can't find it, just skip :(
    if maybeModuleInfo == Nothing
        then Instance m paramBindings x rs portBindings
        else Instance m paramBindings' x rs portBindings'
    where
        maybeModuleInfo = Map.lookup m modulePorts
        Just (paramNames, portNames) = maybeModuleInfo

        msg :: String -> String
        msg = flip (++) $ " in instance " ++ show x ++ " of " ++ show m

        paramBindings' = resolveBindings (msg "parameter overrides") paramNames
            paramBindings
        portBindings' = resolveBindings (msg "port connections") portNames
            $ concatMap expandStar portBindings

        expandStar :: PortBinding -> [PortBinding]
        expandStar ("*", Nil) =
            map (\port -> (port, Ident port)) $
            filter (flip notElem alreadyBound) portNames
            where alreadyBound = map fst portBindings
        expandStar other = [other]

mapInstance _ other = other
