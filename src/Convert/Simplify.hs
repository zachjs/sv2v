{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Elaboration of size casts, dimension query system functions, and ternary
 - expressions where the condition references a localparam.
 -
 - Our conversions generate a lot of ternary expressions. This conversion
 - attempts to make the code output a bit cleaner. Note that we can only do this
 - simplification on localparams because parameters can be overridden at
 - instantiation.
 -
 - This conversion applies the heuristic that it will only make substitutions
 - into a ternary condition if making substitutions immediately enables the
 - expression to be simplified further.
 -}

module Convert.Simplify (convert) where

import Control.Monad (when)

import Convert.ExprUtils
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription =
    partScoper traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

traverseDeclM :: Decl -> Scoper Expr Decl
traverseDeclM decl = do
    decl' <- traverseDeclExprsM traverseExprM decl
    case decl' of
        Param Localparam UnknownType x e ->
            insertExpr x e
        Param Localparam (Implicit sg rs) x e ->
            insertExpr x $ Cast (Left t) e
            where t = IntegerVector TLogic sg rs
        Param Localparam t x e ->
            insertExpr x $ Cast (Left t) e
        Variable _ _ x _ _ -> insertElem x Nil
        Net  _ _ _ _ x _ _ -> insertElem x Nil
        _ -> return ()
    return decl'

pattern SimpleVector :: Signing -> Integer -> Integer -> Type
pattern SimpleVector sg l r <- IntegerVector _ sg [(RawNum l, RawNum r)]

insertExpr :: Identifier -> Expr -> Scoper Expr ()
insertExpr ident expr = do
    expr' <- substituteExprM expr
    case expr' of
        Cast (Left (SimpleVector sg l r)) (Number n) ->
            insertElem ident $ Number $ numberCast signed size n
            where
                signed = sg == Signed
                size = fromIntegral $ abs $ l - r + 1
        _ -> when (isSimpleExpr expr') $ insertElem ident expr'

isSimpleExpr :: Expr -> Bool
isSimpleExpr Number{}  = True
isSimpleExpr String{}  = True
isSimpleExpr (Cast Left{} e) = isSimpleExpr e
isSimpleExpr _ = False

traverseModuleItemM :: ModuleItem -> Scoper Expr ModuleItem
traverseModuleItemM (Genvar x) =
    insertElem x Nil >> return (Genvar x)
traverseModuleItemM (Instance m p x rs l) = do
    p' <- mapM paramBindingMapper p
    traverseExprsM traverseExprM $ Instance m p' x rs l
    where
        paramBindingMapper (param, Left t) = do
            t' <- traverseNestedTypesM (traverseTypeExprsM substituteExprM) t
            return (param, Left t')
        paramBindingMapper (param, Right e) = return (param, Right e)
traverseModuleItemM item = traverseExprsM traverseExprM item

traverseGenItemM :: GenItem -> Scoper Expr GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseStmtM :: Stmt -> Scoper Expr Stmt
traverseStmtM stmt = traverseStmtExprsM traverseExprM stmt

traverseExprM :: Expr -> Scoper Expr Expr
traverseExprM = embedScopes convertExpr

substituteExprM :: Expr -> Scoper Expr Expr
substituteExprM = embedScopes substitute

convertExpr :: Scopes Expr -> Expr -> Expr
convertExpr info (Cast (Left t) e) =
    Cast (Left t') e'
    where
        t' = traverseNestedTypes (traverseTypeExprs $ substitute info) t
        e' = convertExpr info e
convertExpr info (Cast (Right c) e) =
    Cast (Right c') e'
    where
        c' = convertExpr info $ substitute info c
        e' = convertExpr info e
convertExpr info (DimFn f v e) =
    DimFn f v e'
    where e' = convertExpr info $ substitute info e
convertExpr info (Call (Ident "$clog2") (Args [e] [])) =
    if val' == val
        then val
        else val'
    where
        e' = convertExpr info $ substitute info e
        val = Call (Ident "$clog2") (Args [e'] [])
        val' = simplifyStep val
convertExpr info (Mux cc aa bb) =
    if before == after
        then simplifyStep $ Mux cc' aa' bb'
        else simplifyStep $ Mux after aa' bb'
    where
        before = substitute info cc'
        after = convertExpr info before
        aa' = convertExpr info aa
        bb' = convertExpr info bb
        cc' = convertExpr info cc
convertExpr info (BinOp op e1 e2) =
    case simplifyStep $ BinOp op e1'Sub e2'Sub of
        Number n -> Number n
        _ -> simplifyStep $ BinOp op e1' e2'
    where
        e1' = convertExpr info e1
        e2' = convertExpr info e2
        e1'Sub = substituteIdent info e1'
        e2'Sub = substituteIdent info e2'
convertExpr info (UniOp op expr) =
    simplifyStep $ UniOp op $ convertExpr info expr
convertExpr info (Repeat expr exprs) =
    simplifyStep $ Repeat
        (convertExpr info expr)
        (map (convertExpr info) exprs)
convertExpr info (Concat exprs) =
    simplifyStep $ Concat (map (convertExpr info) exprs)
convertExpr info expr =
    traverseSinglyNestedExprs (convertExpr info) expr

substitute :: Scopes Expr -> Expr -> Expr
substitute scopes expr =
    substitute' expr
    where
        substitute' :: Expr -> Expr
        substitute' (Ident x) =
            case lookupElem scopes x of
                Just (_, _, e) | e /= Nil -> e
                _ -> Ident x
        substitute' other =
            traverseSinglyNestedExprs substitute' other

substituteIdent :: Scopes Expr -> Expr -> Expr
substituteIdent scopes (Ident x) =
    case lookupElem scopes x of
        Just (_, _, n@Number{}) -> n
        _ -> Ident x
substituteIdent _ other = other
