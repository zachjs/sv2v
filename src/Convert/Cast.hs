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
import Data.Maybe (isJust)

import Convert.ExprUtils
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription =
    traverseModuleItems dropDuplicateCaster . evalScoper . scopePart scoper
    where scoper = scopeModuleItem
            traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

type SC = Scoper ()

traverseDeclM :: Decl -> SC Decl
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
                    insertElem x ()
                    return $ Variable d t x a e'
        Net d n s t x a e -> do
            enterStmt
            e' <- traverseExprM e
            exitStmt
            insertElem x ()
            return $ Net d n s t x a e'
        Param _ _ x _ ->
            insertElem x () >> return decl
        ParamType    _ _ _ -> return decl
        CommentDecl      _ -> return decl
    traverseDeclExprsM traverseExprM decl'

pattern DuplicateTag :: Identifier
pattern DuplicateTag = ":duplicate_cast_to_be_removed:"

dropDuplicateCaster :: ModuleItem -> ModuleItem
dropDuplicateCaster (MIPackageItem (Function _ _ DuplicateTag _ _)) =
    Generate []
dropDuplicateCaster other = other

traverseModuleItemM :: ModuleItem -> SC ModuleItem
traverseModuleItemM (Genvar x) =
    insertElem x () >> return (Genvar x)
traverseModuleItemM item =
    traverseExprsM traverseExprM item

traverseGenItemM :: GenItem -> SC GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseStmtM :: Stmt -> SC Stmt
traverseStmtM stmt = do
    enterStmt
    stmt' <- traverseStmtExprsM traverseExprM stmt
    exitStmt
    return stmt'

traverseExprM :: Expr -> SC Expr
traverseExprM (Cast (Left (IntegerVector kw sg rs)) value) | kw /= TBit = do
    value' <- fmap simplify $ traverseExprM value
    size' <- traverseExprM size
    convertCastM size' value' signed
    where
        signed = sg == Signed
        size = dimensionsSize rs
traverseExprM other =
    traverseSinglyNestedExprsM traverseExprM other

convertCastM :: Expr -> Expr -> Bool -> SC Expr
convertCastM (Number size) _ _
    | maybeInt == Nothing = illegal "an integer"
    | int <= 0            = illegal "a positive integer"
    where
        maybeInt = numberToInteger size
        Just int = maybeInt
        illegal = scopedErrorM . msg
        msg s = "size cast width " ++ show size ++ " is not " ++ s
convertCastM (Number size) (Number value) signed =
    return $ Number $
        numberCast signed (fromIntegral size') value
    where Just size' = numberToInteger size
convertCastM size@Number{} (String str) signed =
    convertCastM size (stringToNumber str) signed
convertCastM size value signed = do
    sizeUsesLocalVars <- embedScopes usesLocalVars size
    inProcedure <- withinProcedureM
    if not sizeUsesLocalVars || not inProcedure then do
        let name = castFnName size signed
        let item = castFn name size signed
        if sizeUsesLocalVars
            then do
                details <- lookupLocalIdentM name
                when (details == Nothing) (injectItem item)
            else do
                details <- lookupElemM name
                when (details == Nothing) (injectTopItem item)
        return $ Call (Ident name) (Args [value] [])
    else do
        name <- castDeclName 0
        insertElem name ()
        useVar <- withinStmt
        injectDecl $ castDecl useVar name value size signed
        return $ Ident name

-- checks if a cast size references any vars not defined at the top level scope
usesLocalVars :: Scopes a -> Expr -> Bool
usesLocalVars scopes =
    getAny . execWriter . collectNestedExprsM collectLocalVarsM
    where
        collectLocalVarsM :: Expr -> Writer Any ()
        collectLocalVarsM expr@(Ident x) =
            if isLoopVar scopes x
                then tell $ Any True
                else resolve expr
        collectLocalVarsM expr = resolve expr
        resolve :: Expr -> Writer Any ()
        resolve expr =
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
            Number n -> show v
                where Just v = numberToInteger n
            _ -> shortHash size
        suffix = if signed then "_signed" else ""

castDecl :: Bool -> Identifier -> Expr -> Expr -> Bool -> Decl
castDecl useVar name value size signed =
    if useVar
        then Variable Local t name [] value
        else Param Localparam t name value
    where t = castType size signed

castDeclName :: Int -> SC String
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
withinStmtKey :: Identifier
withinStmtKey = ":within_stmt:"
withinStmt :: SC Bool
withinStmt = fmap isJust $ lookupElemM withinStmtKey
enterStmt :: SC ()
enterStmt = do
    inProcedure <- withinProcedureM
    when inProcedure $ insertElem withinStmtKey ()
exitStmt :: SC ()
exitStmt = do
    inProcedure <- withinProcedureM
    when inProcedure $ removeElem withinStmtKey
