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
convertModuleItem = traverseNodes
    traverseExpr traverseDecl traverseType traverseLHS traverseStmt
    where
        traverseLHS = traverseNestedLHSs $ traverseLHSExprs traverseExpr
        traverseStmt = traverseNestedStmts $
            traverseStmtDecls (traverseDeclNodes traverseType id) .
            traverseStmtExprs traverseExpr

traverseDecl :: Decl -> Decl
traverseDecl (Net d n s t x a e) =
    traverseDeclNodes traverseType traverseExpr $
        Net d n s (convertTypeForce t) x a e
traverseDecl decl =
    traverseDeclNodes traverseType traverseExpr decl

traverseType :: Type -> Type
traverseType =
    traverseSinglyNestedTypes traverseType .
    traverseTypeExprs traverseExpr .
    convertType

traverseExpr :: Expr -> Expr
traverseExpr =
    traverseSinglyNestedExprs traverseExpr .
    traverseExprTypes traverseType .
    convertExpr

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
    zip (map (convertTypeForce . fst) fields) (map snd fields)

convertTypeForce :: Type -> Type
convertTypeForce (IntegerAtom TInteger sg) = IntegerAtom TInt sg
convertTypeForce t = t

convertExpr :: Expr -> Expr
convertExpr (Pattern items) =
    Pattern $ zip names exprs
    where
        names = map (convertTypeOrExprForce . fst) items
        exprs = map snd items
convertExpr other = other

convertTypeOrExprForce :: TypeOrExpr -> TypeOrExpr
convertTypeOrExprForce (Left t) = Left $ convertTypeForce t
convertTypeOrExprForce (Right e) = Right e
