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
convert = map $ traverseDescriptions $ partScoper
    traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

traverseTypeOrExprM :: TypeOrExpr -> Scoper Type TypeOrExpr
traverseTypeOrExprM (Left (TypeOf (Ident x))) = do
    details <- lookupElemM x
    return $ case details of
        Nothing -> Left $ TypeOf $ Ident x
        Just (_, _, typ) -> Left typ
traverseTypeOrExprM (Right (Ident x)) = do
    details <- lookupElemM x
    return $ case details of
        Nothing -> Right $ Ident x
        Just (_, _, typ) -> Left typ
traverseTypeOrExprM other = return other

traverseExprM :: Expr -> Scoper Type Expr
traverseExprM (Cast v e) = do
    v' <- traverseTypeOrExprM v
    return $ Cast v' e
traverseExprM (DimsFn f v) = do
    v' <- traverseTypeOrExprM v
    return $ DimsFn f v'
traverseExprM (DimFn f v e) = do
    v' <- traverseTypeOrExprM v
    return $ DimFn f v' e
traverseExprM other = return other

traverseModuleItemM :: ModuleItem -> Scoper Type ModuleItem
traverseModuleItemM (Instance m params x rs p) = do
    let mapParam (i, v) = traverseTypeOrExprM v >>= \v' -> return (i, v')
    params' <- mapM mapParam params
    traverseModuleItemM' $ Instance m params' x rs p
traverseModuleItemM item = traverseModuleItemM' item

traverseModuleItemM' :: ModuleItem -> Scoper Type ModuleItem
traverseModuleItemM' =
    traverseTypesM (traverseNestedTypesM traverseTypeM) >=>
    traverseExprsM (traverseNestedExprsM traverseExprM)

traverseGenItemM :: GenItem -> Scoper Type GenItem
traverseGenItemM = traverseGenItemExprsM (traverseNestedExprsM traverseExprM)

traverseDeclM :: Decl -> Scoper Type Decl
traverseDeclM decl = do
    item <- traverseModuleItemM (MIPackageItem $ Decl decl)
    let MIPackageItem (Decl decl') = item
    case decl' of
        Variable{} -> return decl'
        Param{} -> return decl'
        ParamType Localparam x t -> do
            t' <- traverseNestedTypesM traverseTypeM t
            insertElem x t'
            return $ CommentDecl $ "removed localparam type " ++ x
        ParamType{} -> return decl'
        CommentDecl{} -> return decl'

traverseStmtM :: Stmt -> Scoper Type Stmt
traverseStmtM = traverseStmtExprsM $ traverseNestedExprsM traverseStmtExprM
    where
        traverseStmtExprM :: Expr -> Scoper Type Expr
        traverseStmtExprM =
            traverseExprTypesM (traverseNestedTypesM traverseTypeM) >=>
            traverseExprM

traverseTypeM :: Type -> Scoper Type Type
traverseTypeM (Alias st rs1) = do
    details <- lookupElemM st
    return $ case details of
        Nothing -> Alias st rs1
        Just (_, _, typ) -> case typ of
            Net           kw sg rs2 -> Net           kw sg $ rs1 ++ rs2
            Implicit         sg rs2 -> Implicit         sg $ rs1 ++ rs2
            IntegerVector kw sg rs2 -> IntegerVector kw sg $ rs1 ++ rs2
            Enum            t v rs2 -> Enum            t v $ rs1 ++ rs2
            Struct          p l rs2 -> Struct          p l $ rs1 ++ rs2
            Union           p l rs2 -> Union           p l $ rs1 ++ rs2
            InterfaceT     x my rs2 -> InterfaceT     x my $ rs1 ++ rs2
            Alias            xx rs2 -> Alias            xx $ rs1 ++ rs2
            PSAlias    ps    xx rs2 -> PSAlias    ps    xx $ rs1 ++ rs2
            CSAlias    ps pm xx rs2 -> CSAlias    ps pm xx $ rs1 ++ rs2
            UnpackedType  t     rs2 -> UnpackedType      t $ rs1 ++ rs2
            IntegerAtom   kw sg     -> nullRange (IntegerAtom kw sg) rs1
            NonInteger    kw        -> nullRange (NonInteger  kw   ) rs1
            TypeOf             expr -> nullRange (TypeOf       expr) rs1
traverseTypeM other = return other
