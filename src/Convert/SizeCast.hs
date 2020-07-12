{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion of size casts on non-constant expressions.
 -}

module Convert.SizeCast (convert) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.ExprUtils
import Convert.Traverse
import Language.SystemVerilog.AST

type TypeMap = Map.Map Identifier Type
type CastSet = Set.Set (Expr, Signing)

type ST = StateT TypeMap (Writer CastSet)

convert :: [AST] -> [AST]
convert = map convertFile

convertFile :: AST -> AST
convertFile descriptions =
    descriptions' ++ map (uncurry castFn) funcs
    where
        results = map convertDescription descriptions
        descriptions' = map fst results
        funcs = Set.toList $ Set.unions $ map snd results

convertDescription :: Description -> (Description, CastSet)
convertDescription description =
    (description', info)
    where
        (description', info) =
            runWriter $
                scopedConversionM traverseDeclM traverseModuleItemM traverseStmtM
                Map.empty description

traverseDeclM :: Decl -> ST Decl
traverseDeclM decl = do
    case decl of
        Variable _ t x _ _ -> modify $ Map.insert x t
        Param    _ t x   _ -> modify $ Map.insert x t
        ParamType    _ _ _ -> return ()
        CommentDecl      _ -> return ()
    return decl

traverseModuleItemM :: ModuleItem -> ST ModuleItem
traverseModuleItemM item = traverseExprsM traverseExprM item

traverseStmtM :: Stmt -> ST Stmt
traverseStmtM stmt = traverseStmtExprsM traverseExprM stmt

pattern ConvertedUU :: Integer -> Integer -> Expr
pattern ConvertedUU a b = Number (Based 1 True Binary a b)

traverseExprM :: Expr -> ST Expr
traverseExprM =
    traverseNestedExprsM convertExprM
    where
        convertExprM :: Expr -> ST Expr
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
            typeMap <- get
            -- can't convert this cast yet because x could be a typename
            if Map.notMember x typeMap
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

        convertCastM :: Expr -> Expr -> ST Expr
        convertCastM (RawNum n) (ConvertedUU a b) =
            return $ Number $ Based (fromIntegral n) False Binary
                (extend a) (extend b)
            where
                extend 0 = 0
                extend 1 = (2 ^ n) - 1
                extend _ = error "not possible"
        convertCastM s e = do
            typeMap <- get
            case exprSigning typeMap e of
                Just sg -> convertCastWithSigningM s e sg
                _ -> return $ Cast (Right s) e

        convertCastWithSigningM :: Expr -> Expr -> Signing -> ST Expr
        convertCastWithSigningM s e sg = do
            lift $ tell $ Set.singleton (s, sg)
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

castFn :: Expr -> Signing -> Description
castFn e sg =
    PackageItem $
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

exprSigning :: TypeMap -> Expr -> Maybe Signing
exprSigning typeMap (Ident x) =
    case Map.lookup x typeMap of
        Just t -> typeSigning t
        Nothing -> Just Unspecified
exprSigning typeMap (BinOp op e1 e2) =
    combiner sg1 sg2
    where
        sg1 = exprSigning typeMap e1
        sg2 = exprSigning typeMap e2
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
exprSigning _ _ = Just Unspecified

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
