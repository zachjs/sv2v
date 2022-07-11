{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Verilog-2005 requires that all functions have at least one input port.
 - SystemVerilog allows functions to have no arguments. This conversion adds a
 - dummy argument to such functions.
 -}

module Convert.EmptyArgs (convert) where

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type SC = Scoper ()

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions traverseDescription

traverseDescription :: Description -> Description
traverseDescription =
    evalScoper . scopePart scoper .
    traverseModuleItems addDummyArg
    where scoper = scopeModuleItem
            traverseDecl traverseModuleItem traverseGenItem traverseStmt

-- add a dummy argument to functions with no input ports
addDummyArg :: ModuleItem -> ModuleItem
addDummyArg (MIPackageItem (Function l t f decls stmts))
    | all (not . isInput) decls =
        MIPackageItem $ Function l t f (dummyDecl : decls) stmts
addDummyArg other = other

isInput :: Decl -> Bool
isInput (Variable Input _ _ _ _) = True
isInput _ = False

-- write down all declarations so we can look up the dummy arg
traverseDecl :: Decl -> SC Decl
traverseDecl decl = do
    decl' <- case decl of
        Param    _ _ x _   -> insertElem x () >> return decl
        ParamType  _ x _   -> insertElem x () >> return decl
        Variable d t x a e -> do
            insertElem x ()
            -- new dummy args have a special name for idempotence
            return $ if x == dummyIdent
                then Variable d t dummyIdentFinal a e
                else decl
        Net  _ _ _ _ x _ _ -> insertElem x () >> return decl
        CommentDecl{} -> return decl
    traverseDeclExprsM traverseExpr decl'

traverseModuleItem :: ModuleItem -> SC ModuleItem
traverseModuleItem = traverseExprsM traverseExpr

traverseGenItem :: GenItem -> SC GenItem
traverseGenItem = traverseGenItemExprsM traverseExpr

traverseStmt :: Stmt -> SC Stmt
traverseStmt = traverseStmtExprsM traverseExpr

-- pass a dummy value to functions which had no inputs
traverseExpr :: Expr -> SC Expr
traverseExpr (Call func (Args args [])) = do
    details <- lookupElemM $ Dot func dummyIdent
    let args' = if details /= Nothing
                    then RawNum 0 : args
                    else args
    return $ Call func (Args args' [])
traverseExpr expr =
    traverseSinglyNestedExprsM traverseExpr expr

dummyIdent :: Identifier
dummyIdent = '?' : dummyIdentFinal

dummyIdentFinal :: Identifier
dummyIdentFinal = "_sv2v_unused"

dummyType :: Type
dummyType = IntegerVector TReg Unspecified []

dummyDecl :: Decl
dummyDecl = Variable Input dummyType dummyIdent [] Nil
