{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion of elaborated type casts
 -
 - Much of the work of elaborating various casts into explicit integer vector
 - type casts happens in the TypeOf conversion, which contains the primary logic
 - for resolving the type and signedness of expressions. It also removes
 - redundant explicit casts to produce cleaner output.
 -
 - Type casts are defined as producing the result of the expression assigned to
 - a variable of the given type. In the general case, this conversion generates
 - a pass-through function which performs this assignment-based casting. This
 - allows for casts to be used anywhere expressions are used, including within
 - constant expressions.
 -
 - It is possible for the type in a cast to refer to localparams within a
 - procedure. Without evaluating the localparam itself, a function outside of
 - the procedure cannot refer to the size of the type in the cast. In these
 - scenarios, the cast is instead performed by adding a temporary parameter or
 - data declaration within the procedure and assigning the expression to that
 - declaration to perform the cast.
 -
 - A few common cases of casts on number literals are fully elaborated into
 - their corresponding resulting number literals to avoid excessive noise.
 -}

module Convert.Cast (convert) where

import Control.Monad.Writer.Strict
import Data.List (isPrefixOf)

import Convert.ExprUtils
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription description =
    traverseModuleItems dropDuplicateCaster $
    partScoper
        traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM
        description

type ST = Scoper Expr

traverseDeclM :: Decl -> ST Decl
traverseDeclM decl = do
    decl' <- case decl of
        Variable d t x a e -> do
            enterStmt
            e' <- traverseExprM e
            exitStmt
            details <- lookupLocalIdentM x
            if isPrefixOf "sv2v_cast_" x && details /= Nothing
                then return $ Variable Local t DuplicateTag [] Nil
                else do
                    insertElem x Nil
                    return $ Variable d t x a e'
        Param _ _ x _ ->
            insertElem x Nil >> return decl
        ParamType    _ _ _ -> return decl
        CommentDecl      _ -> return decl
    traverseDeclExprsM traverseExprM decl'

pattern DuplicateTag :: Identifier
pattern DuplicateTag = ":duplicate_cast_to_be_removed:"

dropDuplicateCaster :: ModuleItem -> ModuleItem
dropDuplicateCaster (MIPackageItem (Function _ _ DuplicateTag _ _)) =
    Generate []
dropDuplicateCaster other = other

traverseModuleItemM :: ModuleItem -> ST ModuleItem
traverseModuleItemM (Genvar x) =
    insertElem x Nil >> return (Genvar x)
traverseModuleItemM item =
    traverseExprsM traverseExprM item

traverseGenItemM :: GenItem -> ST GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseStmtM :: Stmt -> ST Stmt
traverseStmtM stmt = do
    enterStmt
    stmt' <- traverseStmtExprsM traverseExprM stmt
    exitStmt
    return stmt'

traverseExprM :: Expr -> ST Expr
traverseExprM (Cast (Left (IntegerVector _ sg rs)) value) = do
    value' <- traverseExprM value
    size' <- traverseExprM size
    convertCastM size' value' signed
    where
        signed = sg == Signed
        size = dimensionsSize rs
traverseExprM other =
    traverseSinglyNestedExprsM traverseExprM other

convertCastM :: Expr -> Expr -> Bool -> ST Expr
convertCastM (RawNum size) (RawNum val) signed =
    return $ Number $ Decimal (fromIntegral size) signed val'
    where val' = val `mod` (2 ^ size)
convertCastM (RawNum size) (Number (Based 1 True Binary a b)) signed =
    return $ Number $ Based (fromIntegral size) signed Binary
        (val * a) (val * b)
    where val = (2 ^ size) - 1
convertCastM (RawNum size) (Number (UnbasedUnsized ch)) signed =
    convertCastM (RawNum size) (Number num) signed
    where
        num = Based 1 True Binary a b
        (a, b) = case ch of
            '0' -> (0, 0)
            '1' -> (1, 0)
            'x' -> (0, 1)
            'z' -> (1, 1)
            _ -> error $ "unexpected unbased-unsized digit: " ++ [ch]
convertCastM size value signed = do
    value' <- traverseExprM value
    useFn <- embedScopes canUseCastFn size
    if useFn then do
        let name = castFnName size signed
        details <- lookupLocalIdentM name
        when (details == Nothing) $
            injectItem $ castFn name size signed
        return $ Call (Ident name) (Args [value'] [])
    else do
        name <- castDeclName 0
        insertElem name Nil
        useVar <- withinStmt
        injectDecl $ castDecl useVar name value' size signed
        return $ Ident name

-- checks if a cast size can be hoisted to a cast function
canUseCastFn :: Scopes a -> Expr -> Bool
canUseCastFn scopes size =
    not (inProcedure && anyNonLocal)
    where
        inProcedure = withinProcedure scopes
        anyNonLocal = getAny $ execWriter $
            collectNestedExprsM collectNonLocalExprM size
        collectNonLocalExprM :: Expr -> Writer Any ()
        collectNonLocalExprM expr =
            case lookupElem scopes expr of
                Nothing -> return ()
                Just ([_, _], _, _) -> return ()
                Just (_, _, _) -> tell $ Any True

castType :: Expr -> Bool -> Type
castType size signed =
    IntegerVector TLogic sg [r]
    where
        r = (simplify $ BinOp Sub size (RawNum 1), RawNum 0)
        sg = if signed then Signed else Unspecified

castFn :: Identifier -> Expr -> Bool -> ModuleItem
castFn name size signed =
    MIPackageItem $ Function Automatic t name [decl] [stmt]
    where
        inp = "inp"
        t = castType size signed
        decl = Variable Input t inp [] Nil
        stmt = Asgn AsgnOpEq Nothing (LHSIdent name) (Ident inp)

castFnName :: Expr -> Bool -> String
castFnName size signed =
    "sv2v_cast_" ++ sizeStr ++ suffix
    where
        sizeStr = case size of
            Number n ->
                case numberToInteger n of
                    Just v -> show v
                    _ -> shortHash size
            _ -> shortHash size
        suffix = if signed then "_signed" else ""

castDecl :: Bool -> Identifier -> Expr -> Expr -> Bool -> Decl
castDecl useVar name value size signed =
    if useVar
        then Variable Local t name [] value
        else Param Localparam t name value
    where t = castType size signed

castDeclName :: Int -> ST String
castDeclName counter = do
    details <- lookupElemM name
    if details == Nothing
        then return name
        else castDeclName (counter + 1)
    where
        name = if counter == 0
            then prefix
            else prefix ++ '_' : show counter
        prefix = "sv2v_tmp_cast"

-- track whether procedural casts should use variables
pattern WithinStmt :: Identifier
pattern WithinStmt = ":within_stmt:"
withinStmt :: ST Bool
withinStmt = do
    details <- lookupElemM WithinStmt
    return $ case details of
        Just (_, _, t) -> t /= Nil
        Nothing -> False
enterStmt :: ST ()
enterStmt = do
    inProcedure <- withinProcedureM
    when inProcedure $ insertElem WithinStmt (RawNum 1)
exitStmt :: ST ()
exitStmt = do
    inProcedure <- withinProcedureM
    when inProcedure $ insertElem WithinStmt Nil
