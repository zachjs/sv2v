{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `.*` in module instantiation
 -}

module Convert.StarPort (convert) where

import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert descriptions =
    traverseDescriptions (traverseModuleItems mapInstance) descriptions
    where
        modulePorts = Map.fromList $ mapMaybe getPorts descriptions
        getPorts :: Description -> Maybe (Identifier, [Identifier])
        getPorts (Module name ports _) = Just (name, ports)
        getPorts _ = Nothing

        mapInstance :: ModuleItem -> ModuleItem
        mapInstance (Instance m p x Nothing) =
            Instance m p x (Just portBindings)
            where
                ports = case Map.lookup m modulePorts of
                    Nothing -> error $ "could not convert `.*` in instantiation of " ++ m
                    Just l -> l
                portBindings = map (\port -> (port, Just $ Ident port)) ports
        mapInstance other = other
