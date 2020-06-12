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
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ Part{}) =
    traverseModuleItems (convertTypedef types) description'
    where
        description' =
            traverseModuleItems (traverseGenItems convertGenItem) description
        types = execWriter $ collectModuleItemsM collectTypedefM description'
convertDescription other = other

convertTypedef :: Types -> ModuleItem -> ModuleItem
convertTypedef types =
    removeTypedef .
    convertModuleItem .
    (traverseExprs $ traverseNestedExprs $ convertExpr) .
    (traverseTypes $ resolveType types)
    where
        removeTypedef :: ModuleItem -> ModuleItem
        removeTypedef (MIPackageItem (Typedef _ x)) =
            MIPackageItem $ Decl $ CommentDecl $ "removed typedef: " ++ x
        removeTypedef other = other
        convertTypeOrExpr :: TypeOrExpr -> TypeOrExpr
        convertTypeOrExpr (Left (TypeOf (Ident x))) =
            if Map.member x types
                then Left $ resolveType types (Alias Nothing x [])
                else Left $ TypeOf (Ident x)
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

convertGenItem :: GenItem -> GenItem
convertGenItem (GenIf c a b) =
    GenIf c a' b'
    where
        a' = convertGenItem' a
        b' = convertGenItem' b
convertGenItem other = other

convertGenItem' :: GenItem -> GenItem
convertGenItem' item = do
    GenBlock "" items
    where
        -- convert inner generate blocks first
        item' = Generate [traverseNestedGenItems convertGenItem item]
        types = execWriter $ collectNestedModuleItemsM collectTypedefM item'
        Generate items = traverseNestedModuleItems (convertTypedef types) item'

collectTypedefM :: ModuleItem -> Writer Types ()
collectTypedefM (MIPackageItem (Typedef a b)) = tell $ Map.singleton b a
collectTypedefM _ = return ()

resolveItem :: Types -> (Type, Identifier) -> (Type, Identifier)
resolveItem types (t, x) = (resolveType types t, x)

resolveType :: Types -> Type -> Type
resolveType _ (Net           kw sg rs) = Net           kw sg rs
resolveType _ (Implicit         sg rs) = Implicit         sg rs
resolveType _ (IntegerVector kw sg rs) = IntegerVector kw sg rs
resolveType _ (IntegerAtom   kw sg   ) = IntegerAtom   kw sg
resolveType _ (NonInteger    kw      ) = NonInteger    kw
resolveType _ (InterfaceT     x my rs) = InterfaceT     x my rs
resolveType _ (Alias (Just ps)  st rs) = Alias (Just ps)  st rs
resolveType _ (TypeOf            expr) = TypeOf            expr
resolveType _ (UnpackedType  t     rs) = UnpackedType  t     rs
resolveType types (Enum   t vals  rs) = Enum (resolveType types t) vals rs
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
        (UnpackedType  t     rs2) -> UnpackedType      t $ rs1 ++ rs2
        (IntegerAtom   kw sg    ) -> nullRange (IntegerAtom kw sg) rs1
        (NonInteger    kw       ) -> nullRange (NonInteger  kw   ) rs1
        (TypeOf             expr) -> nullRange (TypeOf       expr) rs1
