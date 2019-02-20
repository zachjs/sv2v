{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `.*` in module instantiation
 -}

module Convert.StarPort (convert) where

import Data.Maybe
import qualified Data.Map.Strict as Map

import Language.SystemVerilog.AST

type ModulePorts = Map.Map String [String]

convert :: AST -> AST
convert descriptions = map (convertDescription portsInfo) descriptions
    where
        portsInfo = Map.fromList $ mapMaybe getPorts descriptions
        getPorts :: Description -> Maybe (Identifier, [Identifier])
        getPorts (Module name ports _) = Just (name, ports)
        getPorts _ = Nothing

convertDescription :: ModulePorts -> Description -> Description
convertDescription info (Module name ports items) =
    Module name ports $ map (convertModuleItem info) items
convertDescription _ other = other

convertModuleItem :: ModulePorts -> ModuleItem -> ModuleItem
convertModuleItem info (Instance m p x Nothing) =
    Instance m p x (Just portBindings)
    where
        ports = case Map.lookup m info of
            Nothing -> error $ "could not convert `.*` in instantiation of " ++ m
            Just l -> l
        portBindings = map (\port -> (port, Just $ Ident port)) ports
convertModuleItem _ other = other
