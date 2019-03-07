{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `typedef`
 -
 - Aliased types can (probably) appear in all item declarations, including
 - modules, blocks, and function parameters.
 -}

module Convert.Typedef (convert) where

import Control.Monad.Writer
import qualified Data.Map as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Types = Map.Map Identifier Type

convert :: AST -> AST
convert descriptions =
    traverseDescriptions removeTypedef $
    traverseDescriptions (convertDescription types) $
    descriptions
    where
        types = execWriter $ collectDescriptionsM getTypedef descriptions
        getTypedef :: Description -> Writer Types ()
        getTypedef (PackageItem (Typedef a b)) = tell $ Map.singleton b a
        getTypedef _ = return ()
        removeTypedef :: Description -> Description
        removeTypedef (PackageItem (Typedef _ x)) =
            PackageItem $ Comment $ "removed typedef: " ++ x
        removeTypedef other = other

convertDescription :: Types -> Description -> Description
convertDescription globalTypes description =
    traverseModuleItems removeTypedef $
    traverseModuleItems (traverseTypes $ resolveType types) $
    description
    where
        types = Map.union globalTypes $
            execWriter $ collectModuleItemsM getTypedef description
        getTypedef :: ModuleItem -> Writer Types ()
        getTypedef (MIPackageItem (Typedef a b)) = tell $ Map.singleton b a
        getTypedef _ = return ()
        removeTypedef :: ModuleItem -> ModuleItem
        removeTypedef (MIPackageItem (Typedef _ x)) =
            MIPackageItem $ Comment $ "removed typedef: " ++ x
        removeTypedef other = other

resolveType :: Types -> Type -> Type
resolveType _ (Reg      rs) = Reg      rs
resolveType _ (Wire     rs) = Wire     rs
resolveType _ (Logic    rs) = Logic    rs
resolveType _ (Implicit rs) = Implicit rs
resolveType _ (IntegerT   ) = IntegerT
resolveType _ (InterfaceT x my rs) = InterfaceT x my rs
resolveType _ (Enum Nothing vals rs) = Enum Nothing vals rs
resolveType types (Enum (Just t) vals rs) = Enum (Just $ resolveType types t) vals rs
resolveType types (Struct p items rs) = Struct p items' rs
    where
        items' = map resolveItem items
        resolveItem (t, x) = (resolveType types t, x)
resolveType types (Alias st rs1) =
    if Map.notMember st types
    then InterfaceT st Nothing rs1
    else case resolveType types $ types Map.! st of
        (Reg      rs2) -> Reg      $ rs2 ++ rs1
        (Wire     rs2) -> Wire     $ rs2 ++ rs1
        (Logic    rs2) -> Logic    $ rs2 ++ rs1
        (Enum t v rs2) -> Enum t v $ rs2 ++ rs1
        (Struct p l rs2) -> Struct p l $ rs2 ++ rs1
        (InterfaceT x my rs2) -> InterfaceT x my $ rs2 ++ rs1
        (Implicit rs2) -> Implicit $ rs2 ++ rs1
        (IntegerT    ) -> error $ "resolveType encountered packed `integer` on " ++ st
        (Alias    _ _) -> error $ "resolveType invariant failed on " ++ st
