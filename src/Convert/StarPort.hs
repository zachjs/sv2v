{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `.*` in module instantiation
 -}

module Convert.StarPort (convert) where

import Control.Monad.Writer
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert descriptions =
    traverseDescriptions (traverseModuleItems mapInstance) descriptions
    where
        modulePorts = execWriter $ collectDescriptionsM getPorts descriptions
        getPorts :: Description -> Writer (Map.Map Identifier [Identifier]) ()
        getPorts (Part _ name ports _) = tell $ Map.singleton name ports
        getPorts _ = return ()

        mapInstance :: ModuleItem -> ModuleItem
        mapInstance (Instance m p x Nothing) =
            Instance m p x (Just portBindings)
            where
                ports = case Map.lookup m modulePorts of
                    Nothing -> error $ "could not convert `.*` in instantiation of " ++ m
                    Just l -> l
                portBindings = map (\port -> (port, Just $ Ident port)) ports
        mapInstance other = other
