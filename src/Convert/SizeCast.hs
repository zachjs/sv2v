{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion of size casts on non-constant expressions.
 -}

module Convert.SizeCast (convert) where

import Control.Monad.Writer.Strict
import Data.List (isPrefixOf)

import Convert.ExprUtils
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription =
    traverseModuleItems dropDuplicateCaster . partScoper
    traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

traverseDeclM :: Decl -> Scoper Type Decl
traverseDeclM decl = do
    decl' <- case decl of
        Variable _ t x _ _ -> do
            details <- lookupElemM x
            if isPrefixOf "sv2v_cast_" x && details /= Nothing
                then return $ Variable Local t DuplicateTag [] Nil
                else insertElem x t >> return decl
        Param    _ t x   _ -> do
            inProcedure <- withinProcedureM
            when (not inProcedure) $ insertElem x t
            return decl
        ParamType    _ _ _ -> return decl
        CommentDecl      _ -> return decl
    traverseDeclExprsM traverseExprM decl'

pattern DuplicateTag :: Identifier
pattern DuplicateTag = ":duplicate_cast_to_be_removed:"

dropDuplicateCaster :: ModuleItem -> ModuleItem
dropDuplicateCaster (MIPackageItem (Function _ _ DuplicateTag _ _)) =
    Generate []
dropDuplicateCaster other = other

traverseModuleItemM :: ModuleItem -> Scoper Type ModuleItem
traverseModuleItemM (Genvar x) =
    insertElem x (IntegerAtom TInteger Unspecified) >> return (Genvar x)
traverseModuleItemM item =
    traverseExprsM traverseExprM item

traverseGenItemM :: GenItem -> Scoper Type GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseStmtM :: Stmt -> Scoper Type Stmt
traverseStmtM = traverseStmtExprsM traverseExprM

pattern ConvertedUU :: Integer -> Integer -> Expr
pattern ConvertedUU a b = Number (Based 1 True Binary a b)

traverseExprM :: Expr -> Scoper Type Expr
traverseExprM =
    traverseNestedExprsM convertExprM
    where
        convertExprM :: Expr -> Scoper Type Expr
        convertExprM (Cast (Right (Number s)) (Number n)) =
            case n of
                UnbasedUnsized{} -> fallback
                Decimal (-32) True val ->
                    num $ Decimal (fromIntegral size) False val'
                    where
                        Just size = numberToInteger s
                        val' = val `mod` (2 ^ size)
                Decimal size signed val ->
                    if sizesMatch
                        then num $ Decimal (abs size) signed val
                        else fallback
                Based size signed base vals knds ->
                    if sizesMatch
                        then num $ Based (abs size) signed base vals knds
                        else fallback
            where
                sizesMatch = numberToInteger s == Just (numberBitLength n)
                fallback = convertCastM (Number s) (Number n)
                num = return . Number
        convertExprM (Cast (Right (Ident x)) e) = do
            details <- lookupElemM x
            -- can't convert this cast yet because x could be a typename
            if details == Nothing
                then return $ Cast (Right $ Ident x) e
                else convertCastM (Ident x) e
        convertExprM (Cast (Right s) e) =
            if isSimpleExpr s
                then convertCastM s e
                else return $ Cast (Right s) e
        convertExprM (Cast (Left (IntegerVector _ Signed rs)) e) =
            convertCastWithSigningM (dimensionsSize rs) e Signed
        convertExprM (Cast (Left (IntegerVector _ _ rs)) e) =
            convertExprM $ Cast (Right $ dimensionsSize rs) e
        convertExprM other = return other

        convertCastM :: Expr -> Expr -> Scoper Type Expr
        convertCastM (RawNum n) (ConvertedUU a b) =
            return $ Number $ Based (fromIntegral n) False Binary
                (extend a) (extend b)
            where
                extend 0 = 0
                extend 1 = (2 ^ n) - 1
                extend _ = error "not possible"
        convertCastM s e = do
            signing <- embedScopes exprSigning e
            case signing of
                Just sg -> convertCastWithSigningM s e sg
                _ -> return $ Cast (Right s) e

        convertCastWithSigningM :: Expr -> Expr -> Signing -> Scoper Type Expr
        convertCastWithSigningM (RawNum size) (RawNum val) Signed =
            return $ Number $ Decimal (fromIntegral size) True val'
            where val' = val `mod` (2 ^ size)
        convertCastWithSigningM s e sg = do
            details <- lookupElemM $ castFnName s sg
            when (details == Nothing) $ injectItem $ MIPackageItem $ castFn s sg
            let f = castFnName s sg
            let args = Args [e] []
            return $ Call (Ident f) args

isSimpleExpr :: Expr -> Bool
isSimpleExpr =
    null . execWriter . collectNestedExprsM collectUnresolvedExprM
    where
        collectUnresolvedExprM :: Expr -> Writer [Expr] ()
        collectUnresolvedExprM (expr @ PSIdent{}) = tell [expr]
        collectUnresolvedExprM (expr @ CSIdent{}) = tell [expr]
        collectUnresolvedExprM (expr @ DimsFn{}) = tell [expr]
        collectUnresolvedExprM (expr @ DimFn {}) = tell [expr]
        collectUnresolvedExprM _ = return ()

castFn :: Expr -> Signing -> PackageItem
castFn e sg =
    Function Automatic t fnName [decl] [Return $ Ident inp]
    where
        inp = "inp"
        r = (simplify $ BinOp Sub e (RawNum 1), RawNum 0)
        t = IntegerVector TLogic sg [r]
        fnName = castFnName e sg
        decl = Variable Input t inp [] Nil

castFnName :: Expr -> Signing -> String
castFnName e sg =
    if sg == Unspecified
        then init name
        else name
    where
        sizeStr = case e of
            Number n ->
                case numberToInteger n of
                    Just v -> show v
                    _ -> shortHash e
            _ -> shortHash e
        name = "sv2v_cast_" ++ sizeStr ++ "_" ++ show sg

exprSigning :: Scopes Type -> Expr -> Maybe Signing
exprSigning scopes (BinOp op e1 e2) =
    combiner sg1 sg2
    where
        sg1 = exprSigning scopes e1
        sg2 = exprSigning scopes e2
        combiner = case op of
            BitAnd  -> combineSigning
            BitXor  -> combineSigning
            BitXnor -> combineSigning
            BitOr   -> combineSigning
            Mul     -> combineSigning
            Div     -> combineSigning
            Add     -> combineSigning
            Sub     -> combineSigning
            Mod     -> curry fst
            Pow     -> curry fst
            ShiftAL -> curry fst
            ShiftAR -> curry fst
            _ -> \_ _ -> Just Unspecified
exprSigning scopes expr =
    case lookupElem scopes expr of
        Just (_, _, t) -> typeSigning t
        Nothing -> Just Unspecified

combineSigning :: Maybe Signing -> Maybe Signing -> Maybe Signing
combineSigning Nothing _ = Nothing
combineSigning _ Nothing = Nothing
combineSigning (Just Unspecified) msg = msg
combineSigning msg (Just Unspecified) = msg
combineSigning (Just Signed) _ = Just Signed
combineSigning _ (Just Signed) = Just Signed
combineSigning (Just Unsigned) _ = Just Unsigned

typeSigning :: Type -> Maybe Signing
typeSigning (Net           _ sg _) = Just sg
typeSigning (Implicit        sg _) = Just sg
typeSigning (IntegerVector _ sg _) = Just sg
typeSigning (IntegerAtom   t sg  ) =
    Just $ case (sg, t) of
        (Unspecified, TTime) -> Unsigned
        (Unspecified, _    ) -> Signed
        (_          , _    ) -> sg
typeSigning _ = Nothing
