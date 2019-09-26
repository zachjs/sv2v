{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `typedef`
 -
 - Aliased types can appear in all data declarations, including modules, blocks,
 - and function parameters. They are also found in type cast expressions.
 -}

module Convert.Typedef (convert) where

import Control.Monad.Writer
import qualified Data.Map as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Types = Map.Map Identifier Type

convert :: [AST] -> [AST]
convert =
    traverseFiles
        (collectDescriptionsM getTypedef)
        (\a -> traverseDescriptions $ removeTypedef . convertDescription a)
    where
        getTypedef :: Description -> Writer Types ()
        getTypedef (PackageItem (Typedef a b)) = tell $ Map.singleton b a
        getTypedef (Part _ _ Interface _ x _ _) =
            tell $ Map.singleton x (InterfaceT x Nothing [])
        getTypedef _ = return ()
        removeTypedef :: Description -> Description
        removeTypedef (PackageItem (Typedef _ x)) =
            PackageItem $ Comment $ "removed typedef: " ++ x
        removeTypedef other = other

convertDescription :: Types -> Description -> Description
convertDescription globalTypes description =
    traverseModuleItems removeTypedef $
    traverseModuleItems convertModuleItem $
    traverseModuleItems (traverseExprs $ traverseNestedExprs $ convertExpr) $
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
        convertTypeOrExpr :: TypeOrExpr -> TypeOrExpr
        convertTypeOrExpr (Right (Ident x)) =
            if Map.member x types
                then Left $ resolveType types (Alias Nothing x [])
                else Right $ Ident x
        convertTypeOrExpr other = other
        convertExpr :: Expr -> Expr
        convertExpr (Cast v e) = Cast (convertTypeOrExpr v) e
        convertExpr (DimsFn f v) = DimsFn f (convertTypeOrExpr v)
        convertExpr (DimFn f v e) = DimFn f (convertTypeOrExpr v) e
        convertExpr other = other
        convertModuleItem :: ModuleItem -> ModuleItem
        convertModuleItem (Instance m params x r p) =
            Instance m (map mapParam params) x r p
            where mapParam (i, v) = (i, convertTypeOrExpr v)
        convertModuleItem other = other

resolveItem :: Types -> (Type, Identifier) -> (Type, Identifier)
resolveItem types (t, x) = (resolveType types t, x)

resolveType :: Types -> Type -> Type
resolveType _ (Net           kw sg rs) = Net           kw sg rs
resolveType _ (Implicit         sg rs) = Implicit         sg rs
resolveType _ (IntegerVector kw sg rs) = IntegerVector kw sg rs
resolveType _ (IntegerAtom   kw sg   ) = IntegerAtom   kw sg
resolveType _ (NonInteger    kw      ) = NonInteger    kw
resolveType _ (InterfaceT     x my rs) = InterfaceT     x my rs
resolveType _ (Enum Nothing   vals rs) = Enum Nothing   vals rs
resolveType _ (Alias (Just ps)  st rs) = Alias (Just ps)  st rs
resolveType types (Enum (Just t) vals rs) = Enum (Just $ resolveType types t) vals rs
resolveType types (Struct p items rs) = Struct p (map (resolveItem types) items) rs
resolveType types (Union  p items rs) = Union  p (map (resolveItem types) items) rs
resolveType types (Alias Nothing st rs1) =
    if Map.notMember st types
    then Alias Nothing st rs1
    else case resolveType types $ types Map.! st of
        (Net           kw sg rs2) -> Net           kw sg $ rs1 ++ rs2
        (Implicit         sg rs2) -> Implicit         sg $ rs1 ++ rs2
        (IntegerVector kw sg rs2) -> IntegerVector kw sg $ rs1 ++ rs2
        (Enum            t v rs2) -> Enum            t v $ rs1 ++ rs2
        (Struct          p l rs2) -> Struct          p l $ rs1 ++ rs2
        (Union           p l rs2) -> Union           p l $ rs1 ++ rs2
        (InterfaceT     x my rs2) -> InterfaceT     x my $ rs1 ++ rs2
        (Alias          ps x rs2) -> Alias          ps x $ rs1 ++ rs2
        (IntegerAtom   kw sg    ) -> nullRange (IntegerAtom kw sg) rs1
        (NonInteger    kw       ) -> nullRange (NonInteger  kw   ) rs1
