{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `typedef`
 -}

-- TODO: Right now we only support typedefs for module data items. Function
-- parameters, block items, etc., probably support typedefs, too.

module Convert.Typedef (convert) where

import Data.Maybe
import qualified Data.Map as Map

import Language.SystemVerilog.AST

type Types = Map.Map Identifier Type

convert :: AST -> AST
convert descriptions =
    filter (not . isTypedef) $ map (convertDescription types) descriptions
    where
        types = Map.fromList $ mapMaybe getTypedef descriptions
        getTypedef :: Description -> Maybe (Identifier, Type)
        getTypedef (Typedef a b) = Just (b, a)
        getTypedef _ = Nothing

isTypedef :: Description -> Bool
isTypedef (Typedef _ _) = True
isTypedef _ = False

convertDescription :: Types -> Description -> Description
convertDescription types (Module name ports items) =
    Module name ports $ map (convertModuleItem types) items
convertDescription _ other = other

resolveType :: Types -> Type -> Type
resolveType _ (Reg   rs) = Reg   rs
resolveType _ (Wire  rs) = Wire  rs
resolveType _ (Logic rs) = Logic rs
resolveType types (Alias st rs1) =
    case resolveType types $ types Map.! st of
        (Reg   rs2) -> Reg   $ rs2 ++ rs1
        (Wire  rs2) -> Wire  $ rs2 ++ rs1
        (Logic rs2) -> Logic $ rs2 ++ rs1
        (Alias _ _) -> error $ "resolveType invariant failed on " ++ st

convertModuleItem :: Types -> ModuleItem -> ModuleItem
convertModuleItem types (LocalNet t ident val) =
    LocalNet (resolveType types t) ident val
convertModuleItem _ other = other
