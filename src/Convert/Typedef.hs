{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `typedef`
 -
 - Aliased types can appear in all data declarations, including modules, blocks,
 - and function parameters. They are also found in type cast expressions.
 -}

module Convert.Typedef (convert) where

import Control.Monad ((>=>))

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ evalScoper . scopeModule scoper
    where scoper = scopeModuleItem
            traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

traverseTypeOrExprM :: TypeOrExpr -> Scoper Type TypeOrExpr
traverseTypeOrExprM (Left (TypeOf (Ident x))) = do
    details <- lookupElemM x
    return $ case details of
        Nothing -> Left $ TypeOf $ Ident x
        Just (_, _, UnknownType) -> Left $ TypeOf $ Ident x
        Just (_, _, typ) -> Left typ
traverseTypeOrExprM (Right (Ident x)) = do
    details <- lookupElemM x
    return $ case details of
        Nothing -> Right $ Ident x
        Just (_, _, UnknownType) -> Right $ Ident x
        Just (_, _, typ) -> Left typ
traverseTypeOrExprM other = return other

traverseExprM :: Expr -> Scoper Type Expr
traverseExprM (Cast v e) = do
    v' <- traverseTypeOrExprM v
    traverseExprM' $ Cast v' e
traverseExprM (DimsFn f v) = do
    v' <- traverseTypeOrExprM v
    traverseExprM' $ DimsFn f v'
traverseExprM (DimFn f v e) = do
    v' <- traverseTypeOrExprM v
    traverseExprM' $ DimFn f v' e
traverseExprM (Pattern items) = do
    names <- mapM traverseTypeOrExprM $ map fst items
    let exprs = map snd items
    traverseExprM' $ Pattern $ zip names exprs
traverseExprM other = traverseExprM' other

traverseExprM' :: Expr -> Scoper Type Expr
traverseExprM' =
    traverseSinglyNestedExprsM traverseExprM
    >=> traverseExprTypesM traverseTypeM

traverseModuleItemM :: ModuleItem -> Scoper Type ModuleItem
traverseModuleItemM (Instance m params x rs p) = do
    let mapParam (i, v) = traverseTypeOrExprM v >>= \v' -> return (i, v')
    params' <- mapM mapParam params
    traverseModuleItemM' $ Instance m params' x rs p
traverseModuleItemM item = traverseModuleItemM' item

traverseModuleItemM' :: ModuleItem -> Scoper Type ModuleItem
traverseModuleItemM' =
    traverseNodesM traverseExprM return traverseTypeM traverseLHSM return
    where traverseLHSM = traverseLHSExprsM traverseExprM

traverseGenItemM :: GenItem -> Scoper Type GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseDeclM :: Decl -> Scoper Type Decl
traverseDeclM decl = do
    decl' <- traverseDeclNodesM traverseTypeM traverseExprM decl
    case decl' of
        Variable{} -> return decl'
        Net{} -> return decl'
        Param s (UnpackedType t rs1) x e -> do
            insertElem x UnknownType
            let (tf, rs2) = typeRanges t
            let t' = tf $ rs1 ++ rs2
            return $ Param s t' x e
        Param _ _ x _ ->
            insertElem x UnknownType >> return decl'
        ParamType Localparam x t -> do
            traverseTypeM t >>= scopeType >>= insertElem x
            return $ case t of
                Enum{} -> ParamType Localparam tmpX t
                _ -> CommentDecl $ "removed localparam type " ++ x
            where tmpX = "_sv2v_keep_enum_for_params"
        ParamType{} -> return decl'
        CommentDecl{} -> return decl'

traverseStmtM :: Stmt -> Scoper Type Stmt
traverseStmtM = traverseStmtExprsM traverseExprM

traverseTypeM :: Type -> Scoper Type Type
traverseTypeM (Alias st rs1) = do
    details <- lookupElemM st
    rs1' <- mapM traverseRangeM rs1
    return $ case details of
        Nothing -> Alias st rs1'
        Just (_, _, UnknownType) -> Alias st rs1'
        Just (_, _, typ) -> tf $ rs1' ++ rs2
            where (tf, rs2) = typeRanges typ
traverseTypeM (TypedefRef expr) = do
    details <- lookupElemM expr
    return $ case details of
        Nothing -> TypedefRef expr
        Just (_, _, typ) -> typ
traverseTypeM other =
    traverseSinglyNestedTypesM traverseTypeM other
    >>= traverseTypeExprsM traverseExprM

traverseRangeM :: Range -> Scoper Type Range
traverseRangeM = mapBothM traverseExprM
