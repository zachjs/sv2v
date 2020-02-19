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

traverseExprM :: Expr -> ST Expr
traverseExprM =
    traverseNestedExprsM convertExprM
    where
        convertExprM :: Expr -> ST Expr
        convertExprM (Cast (Right (Number s)) (Number n)) =
            case (readNumber s, readNumber n) of
                (Just s', Just n') ->
                    return $ Number str
                    where
                        str = (show size) ++ "'d"  ++ (show num)
                        size = s'
                        num = n' `mod` (2 ^ s')
                _ -> convertCastM (Number s) (Number n)
        convertExprM (orig @ (Cast (Right DimsFn{}) _)) =
            return orig
        convertExprM (Cast (Right s) e) =
            convertCastM s e
        convertExprM (Cast (Left (IntegerVector _ Signed rs)) e) =
            convertCastWithSigningM (dimensionsSize rs) e Signed
        convertExprM (Cast (Left (IntegerVector _ _ rs)) e) =
            convertExprM $ Cast (Right $ dimensionsSize rs) e
        convertExprM other = return other

        convertCastM :: Expr -> Expr -> ST Expr
        convertCastM s e = do
            typeMap <- get
            case exprSigning typeMap e of
                Just sg -> convertCastWithSigningM s e sg
                _ -> return $ Cast (Right s) e

        convertCastWithSigningM :: Expr -> Expr -> Signing -> ST Expr
        convertCastWithSigningM s e sg = do
            lift $ tell $ Set.singleton (s, sg)
            let f = castFnName s sg
            let args = Args [Just e] []
            return $ Call (Ident f) args

castFn :: Expr -> Signing -> Description
castFn e sg =
    PackageItem $
    Function Automatic t fnName [decl] [Return $ Ident inp]
    where
        inp = "inp"
        r = (simplify $ BinOp Sub e (Number "1"), Number "0")
        t = IntegerVector TLogic sg [r]
        fnName = castFnName e sg
        decl = Variable Input t inp [] Nothing

castFnName :: Expr -> Signing -> String
castFnName e sg =
    if sg == Unspecified
        then init name
        else name
    where
        sizeStr = case e of
            Number n ->
                case readNumber n of
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
typeSigning _ = Nothing
