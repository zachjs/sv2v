{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for unbased, unsized literals ('0, '1, 'z, 'x)
 -
 - The literals are given a binary base and are made signed to allow sign
 - extension. This enables the desired implicit casting in Verilog-2005.
 - However, in self-determined contextes, the literals are given an explicit
 - size of 1.
 -}

module Convert.UnbasedUnsized (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems convertModuleItem

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem =
    traverseExprs (traverseNestedExprs convertExpr) .
    traverseStmts (traverseNestedStmts convertStmt) .
    traverseTypes (traverseNestedTypes convertType)

digits :: [Char]
digits = ['0', '1', 'x', 'z', 'X', 'Z']

literalFor :: String -> Char -> Expr
literalFor prefix ch =
    if elem ch digits
        then Number (prefix ++ [ch])
        else error $ "unexpected unbased-unsized digit: " ++ [ch]

sizedLiteralFor :: Char -> Expr
sizedLiteralFor = literalFor "1'sb"

unsizedLiteralFor :: Char -> Expr
unsizedLiteralFor '1' = UniOp UniSub $ Number "'sd1"
unsizedLiteralFor ch = literalFor "'sd" ch

convertExpr :: Expr -> Expr
convertExpr (DimsFn fn (Right e)) =
    DimsFn fn $ Right $ convertSizeExpr e
convertExpr (Concat exprs) =
    Concat $ map convertSelfDeterminedExpr exprs
convertExpr (Repeat count exprs) =
    Repeat count $ map convertSelfDeterminedExpr exprs
convertExpr (Number ['\'', ch]) =
    unsizedLiteralFor ch
convertExpr other = other

convertSelfDeterminedExpr :: Expr -> Expr
convertSelfDeterminedExpr (Number ['\'', ch]) =
    sizedLiteralFor ch
convertSelfDeterminedExpr other = other

convertStmt :: Stmt -> Stmt
convertStmt (Subroutine (fn @ (Ident ('$' : _))) (Args args [])) =
    Subroutine fn (Args args' [])
    where args' = map convertSelfDeterminedExpr args
convertStmt other = other

convertType :: Type -> Type
convertType (TypeOf e) = TypeOf $ convertSizeExpr e
convertType other = other

convertSizeExpr :: Expr -> Expr
convertSizeExpr (Mux cond e1 e2) =
    Mux cond e1' e2'
    where
        e1' = convertSelfDeterminedExpr e1
        e2' = convertSelfDeterminedExpr e2
convertSizeExpr e = convertSelfDeterminedExpr e
