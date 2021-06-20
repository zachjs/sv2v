{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `bit`, `int`, `shortint`, `longint`, and `byte`
 -}

module Convert.IntTypes (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems convertModuleItem

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem =
    traverseTypes (traverseNestedTypes convertType) .
    traverseExprs (traverseNestedExprs convertExpr)

convertType :: Type -> Type
convertType (Struct pk fields rs) =
    Struct pk fields' rs
    where fields' = convertStructFields fields
convertType (Union  pk fields rs) =
    Union  pk fields' rs
    where fields' = convertStructFields fields
convertType (IntegerAtom kw sg) = elaborateIntegerAtom $ IntegerAtom kw sg
convertType (IntegerVector TBit sg rs) = IntegerVector TLogic sg rs
convertType other = other

convertStructFields :: [(Type, Identifier)] -> [(Type, Identifier)]
convertStructFields fields =
    zip (map (convertStructFieldType . fst) fields) (map snd fields)

convertStructFieldType :: Type -> Type
convertStructFieldType (IntegerAtom TInteger sg) = IntegerAtom TInt sg
convertStructFieldType t = t

convertExpr :: Expr -> Expr
convertExpr (Pattern items) =
    Pattern $ zip names exprs
    where
        names = map (convertPatternTypeOrExpr . fst) items
        exprs = map snd items
convertExpr other = other

convertPatternTypeOrExpr :: TypeOrExpr -> TypeOrExpr
convertPatternTypeOrExpr (Left t) = Left $ convertStructFieldType t
convertPatternTypeOrExpr (Right e) = Right e
