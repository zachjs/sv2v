{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `typedef` and `localparam type`
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

type SC = Scoper IdentKind

data IdentKind
    = Type Type -- resolved typename
    | Pending -- unresolved type parameter
    | NonType String -- anything else

traverseTypeOrExprM :: TypeOrExpr -> SC TypeOrExpr
traverseTypeOrExprM tore
    | Left (TypeOf expr) <- tore = possibleTypeName tore expr
    | Right expr <- tore = possibleTypeName tore expr
    | otherwise = return tore

possibleTypeName :: TypeOrExpr -> Expr -> SC TypeOrExpr
possibleTypeName orig expr
    | Just (x, rs1) <- maybeTypeName = do
        details <- lookupElemM x
        return $ case details of
            Just (_, _, Type typ) ->
                Left $ tf $ rs1 ++ rs2
                where (tf, rs2) = typeRanges typ
            Just (_, _, Pending) ->
                Left $ Alias x rs1
            _ -> orig
    | otherwise = return orig
    where maybeTypeName = exprToTypeName [] expr

-- aliases in type-or-expr contexts are parsed as expressions
exprToTypeName :: [Range] -> Expr -> Maybe (Identifier, [Range])
exprToTypeName rs (Ident x) = Just (x, rs)
exprToTypeName rs (Bit expr idx) =
    exprToTypeName (r : rs) expr
    where r = (RawNum 0, BinOp Sub idx (RawNum 1))
exprToTypeName rs (Range expr NonIndexed r) = do
    exprToTypeName (r : rs) expr
exprToTypeName _ _ = Nothing

traverseExprM :: Expr -> SC Expr
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

traverseExprM' :: Expr -> SC Expr
traverseExprM' =
    traverseSinglyNestedExprsM traverseExprM
    >=> traverseExprTypesM traverseTypeM

traverseModuleItemM :: ModuleItem -> SC ModuleItem
traverseModuleItemM (Instance m params x rs p) = do
    let mapParam (i, v) = traverseTypeOrExprM v >>= \v' -> return (i, v')
    params' <- mapM mapParam params
    traverseModuleItemM' $ Instance m params' x rs p
traverseModuleItemM item = traverseModuleItemM' item

traverseModuleItemM' :: ModuleItem -> SC ModuleItem
traverseModuleItemM' =
    traverseNodesM traverseExprM return traverseTypeM traverseLHSM return
    where traverseLHSM = traverseNestedLHSsM $ traverseLHSExprsM traverseExprM

traverseGenItemM :: GenItem -> SC GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseDeclM :: Decl -> SC Decl
traverseDeclM decl = do
    decl' <- traverseDeclNodesM traverseTypeM traverseExprM decl
    case decl' of
        Variable _ _ x _ _ -> insertElem x (NonType "var") >> return decl'
        Net  _ _ _ _ x _ _ -> insertElem x (NonType "net") >> return decl'
        Param s (UnpackedType t rs1) x e -> do
            insertElem x (NonType $ show s)
            let (tf, rs2) = typeRanges t
            let t' = tf $ rs1 ++ rs2
            return $ Param s t' x e
        Param s _ x _ ->
            insertElem x (NonType $ show s) >> return decl'
        ParamType Localparam x t -> do
            traverseTypeM t >>= scopeType >>= insertElem x . Type
            return $ case t of
                Enum{} -> ParamType Localparam tmpX t
                _ -> CommentDecl $ "removed localparam type " ++ x
            where tmpX = "_sv2v_keep_enum_for_params"
        ParamType Parameter x _ ->
            insertElem x Pending >> return decl'
        CommentDecl{} -> return decl'

traverseStmtM :: Stmt -> SC Stmt
traverseStmtM = traverseStmtExprsM traverseExprM

traverseTypeM :: Type -> SC Type
traverseTypeM (Alias st rs1) = do
    details <- lookupElemM st
    rs1' <- mapM traverseRangeM rs1
    case details of
        Just (_, _, Type typ) ->
            return $ tf $ rs1' ++ rs2
            where (tf, rs2) = typeRanges typ
        Just (_, _, Pending) ->
            return $ Alias st rs1'
        Just (_, _, NonType kind) ->
            scopedErrorM $ "expected typename, but found " ++ kind
                ++ " identifier " ++ show st
        Nothing ->
            scopedErrorM $ "couldn't resolve typename " ++ show st
traverseTypeM (TypedefRef expr) = do
    details <- lookupElemM expr
    case details of
        Just (_, _, Type typ) -> return typ
        Just (_, _, Pending) ->
            error "TypdefRef invariant violated! Please file an issue."
        Just (_, _, NonType kind) ->
            scopedErrorM $ "expected interface-based typename, but found "
                ++ kind ++ " " ++ show expr
        -- This can occur when the interface conversion is delayed due to
        -- multi-dimension instances.
        Nothing -> return $ TypedefRef expr
traverseTypeM other =
    traverseSinglyNestedTypesM traverseTypeM other
    >>= traverseTypeExprsM traverseExprM

traverseRangeM :: Range -> SC Range
traverseRangeM = mapBothM traverseExprM
