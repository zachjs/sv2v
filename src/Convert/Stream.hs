{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion of streaming concatenations.
 -}

module Convert.Stream (convert) where

import Control.Monad (zipWithM)

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription = partScoper
    traverseDeclM traverseModuleItemM return traverseStmtM

traverseDeclM :: Decl -> Scoper () Decl
traverseDeclM (Variable d t x [] (Stream StreamR _ exprs)) =
    return $ Variable d t x [] expr'
    where
        expr = Concat exprs
        expr' = resize exprSize lhsSize expr
        lhsSize = DimsFn FnBits $ Left t
        exprSize = sizeof expr
traverseDeclM (Variable d t x [] expr@(Stream StreamL chunk exprs)) = do
    inProcedure <- withinProcedureM
    if inProcedure
        then return $ Variable d t x [] expr
        else do
            injectItem $ MIPackageItem func
            return $ Variable d t x [] expr'
    where
        fnName = streamerFuncName x
        func = streamerFunc fnName chunk (TypeOf $ Concat exprs) t
        expr' = Call (Ident fnName) (Args [Concat exprs] [])
traverseDeclM (Variable d t x a expr) =
    traverseExprM expr >>= return . Variable d t x a
traverseDeclM decl@Net{} = traverseNetAsVarM traverseDeclM decl
traverseDeclM decl = return decl

traverseModuleItemM :: ModuleItem -> Scoper () ModuleItem
traverseModuleItemM (Assign opt lhs (Stream StreamL chunk exprs)) =
    injectItem (MIPackageItem func) >> return (Assign opt lhs expr')
    where
        fnName = streamerFuncName $ shortHash lhs
        t = TypeOf $ lhsToExpr lhs
        arg = Concat exprs
        func = streamerFunc fnName chunk (TypeOf arg) t
        expr' = Call (Ident fnName) (Args [arg] [])
traverseModuleItemM (Assign opt (LHSStream StreamL chunk lhss) expr) =
    traverseModuleItemM $
        Assign opt (LHSConcat lhss)
        (Stream StreamL chunk [expr])
traverseModuleItemM (Assign opt lhs expr) =
    traverseExprM expr' >>= return . Assign opt lhs'
    where Asgn AsgnOpEq Nothing lhs' expr' =
            traverseAsgn (lhs, expr) (Asgn AsgnOpEq Nothing)
traverseModuleItemM item = return item

traverseStmtM :: Stmt -> Scoper () Stmt
traverseStmtM (Subroutine fn (Args args [])) = do
    args' <- traverseCall fn args
    return $ Subroutine fn $ Args args' []
traverseStmtM (Asgn op mt lhs expr) = do
    expr' <- traverseExprM expr
    return $ traverseAsgn (lhs, expr') (Asgn op mt)
traverseStmtM stmt = traverseStmtExprsM traverseExprM stmt

-- replace streaming concatenations in function arguments
traverseExprM :: Expr -> Scoper () Expr
traverseExprM (Call fn (Args args [])) = do
    args' <- traverseCall fn args
    return $ Call fn $ Args args' []
traverseExprM expr = traverseSinglyNestedExprsM traverseExprM expr

traverseCall :: Expr -> [Expr] -> Scoper () [Expr]
traverseCall fn = zipWithM wrapper ([0..] :: [Int])
    where wrapper = traverseCallArg . TypeOf . Dot fn . show

-- task and function arguments are "assignment-like contexts"
traverseCallArg :: Type -> Expr -> Scoper () Expr
traverseCallArg t expr@(Stream op _ _) = do
    inProcedure <- withinProcedureM
    if inProcedure && op == StreamL
        then injectDecl decl >> return (Ident tmp)
        else do
            decl' <- traverseDeclM decl
            let Variable _ _ _ _ expr' = decl'
            return expr'
    where
        tmp = "_arg_tmp_" ++ shortHash (t, expr)
        decl = Variable Local t tmp [] expr
traverseCallArg _ arg = traverseExprM arg

-- produces a function used to capture an inline streaming concatenation
streamerFunc :: Identifier -> Expr -> Type -> Type -> PackageItem
streamerFunc fnName chunk rawInType rawOutType =
    Function Automatic outType fnName [decl] [stmt]
    where
        decl = Variable Input inType "inp" [] Nil
        lhs = LHSIdent fnName
        expr = Stream StreamL chunk [Ident "inp"]
        stmt = traverseAsgn (lhs, expr) (Asgn AsgnOpEq Nothing)

        inType = sizedType rawInType
        outType = sizedType rawOutType
        sizedType :: Type -> Type
        sizedType t = IntegerVector TLogic Unspecified [(hi, RawNum 0)]
            where hi = BinOp Sub (DimsFn FnBits $ Left t) (RawNum 1)

streamerFuncName :: Identifier -> Identifier
streamerFuncName = (++) "_sv2v_strm_"

-- produces a block corresponding to the given leftward streaming concatenation
streamerBlock :: Expr -> Expr -> Expr -> (LHS -> Expr -> Stmt) -> LHS -> Expr -> Stmt
streamerBlock chunk inSize outSize asgn output input =
    Block Seq ""
    [ Variable Local t inp [] input
    , Variable Local t out [] Nil
    , Variable Local (IntegerAtom TInteger Unspecified) idx [] Nil
    ]
    [ For inits cmp incr stmt
    , If NoCheck cmp2 stmt2 Null
    , asgn output result
    ]
    where
        lo = RawNum 0
        hi = BinOp Sub inSize (RawNum 1)
        t = IntegerVector TLogic Unspecified [(hi, lo)]
        name = streamerBlockName chunk inSize
        inp = name ++ "_inp"
        out = name ++ "_out"
        idx = name ++ "_idx"
        -- main chunk loop
        inits = [(LHSIdent idx, lo)]
        cmp = BinOp Le (Ident idx) (BinOp Sub inSize chunk)
        incr = [(LHSIdent idx, AsgnOp Add, chunk)]
        lhs = LHSRange (LHSIdent out) IndexedMinus (BinOp Sub hi (Ident idx), chunk)
        expr = Range (Ident inp) IndexedPlus (Ident idx, chunk)
        stmt = Asgn AsgnOpEq Nothing lhs expr
        -- final chunk loop
        stub = BinOp Mod inSize chunk
        lhs2 = LHSRange (LHSIdent out) IndexedPlus (RawNum 0, stub)
        expr2 = Range (Ident inp) IndexedPlus (Ident idx, stub)
        stmt2 = Asgn AsgnOpEq Nothing lhs2 expr2
        cmp2 = BinOp Gt stub (RawNum 0)
        -- size mismatch padding
        result = resize inSize outSize (Ident out)

streamerBlockName :: Expr -> Expr -> Identifier
streamerBlockName chunk size =
    "_sv2v_strm_" ++ shortHash (chunk, size)

-- pad or truncate the right side of an expression
resize :: Expr -> Expr -> Expr -> Expr
resize inSize outSize expr =
    Mux
        (BinOp Le inSize outSize)
        (BinOp ShiftL expr (BinOp Sub outSize inSize))
        (BinOp ShiftR expr (BinOp Sub inSize outSize))

-- rewrite a given assignment if it uses a streaming concatenation
traverseAsgn :: (LHS, Expr) -> (LHS -> Expr -> Stmt) -> Stmt
traverseAsgn (lhs, Stream StreamR _ exprs) constructor =
    constructor lhs $ resize exprSize lhsSize expr
    where
        expr = Concat exprs
        lhsSize = sizeof $ lhsToExpr lhs
        exprSize = sizeof expr
traverseAsgn (LHSStream StreamR _ lhss, expr) constructor =
    constructor lhs $ resize exprSize lhsSize expr
    where
        lhs = LHSConcat lhss
        lhsSize = sizeof $ lhsToExpr lhs
        exprSize = sizeof expr
traverseAsgn (lhs, Stream StreamL chunk exprs) constructor =
    streamerBlock chunk exprSize lhsSize constructor lhs expr
    where
        expr = Concat exprs
        lhsSize = sizeof $ lhsToExpr lhs
        exprSize = sizeof expr
traverseAsgn (LHSStream StreamL chunk lhss, expr) constructor =
    streamerBlock chunk exprSize lhsSize constructor lhs expr
    where
        lhs = LHSConcat lhss
        lhsSize = sizeof $ lhsToExpr lhs
        exprSize = sizeof expr
traverseAsgn (lhs, expr) constructor =
    constructor lhs expr

sizeof :: Expr -> Expr
sizeof = DimsFn FnBits . Right
