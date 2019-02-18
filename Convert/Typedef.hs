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
resolveType _ (Reg   mr) = Reg   mr
resolveType _ (Wire  mr) = Wire  mr
resolveType _ (Logic mr) = Logic mr
resolveType types (Alias st mr1) =
    case resolveType types $ types Map.! st of
        (Reg   mr2) -> Reg   $ combineRanges mr1 mr2
        (Wire  mr2) -> Wire  $ combineRanges mr1 mr2
        (Logic mr2) -> Logic $ combineRanges mr1 mr2
        (Alias _ _) -> error $ "resolveType invariant failed on " ++ st
    where
        combineRanges :: Maybe Range -> Maybe Range -> Maybe Range
        combineRanges Nothing other = other
        combineRanges other Nothing = other
        combineRanges _ _ = error $ "alias " ++ st ++ " leads to 2-D vectorized type"

convertModuleItem :: Types -> ModuleItem -> ModuleItem
convertModuleItem types (LocalNet t ident val) =
    LocalNet (resolveType types t) ident val
convertModuleItem _ other = other
