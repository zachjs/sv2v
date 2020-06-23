{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Verilog-2005 requires that for loops have have exactly one assignment in the
 - initialization section. For procedural for loops, we pull the declarations
 - out to a wrapping block, and convert all but one assignment to a preceding
 - statement. If a for loop has no assignments or declarations, a dummy
 - declaration is generated.
 -}

module Convert.ForDecl (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $ traverseDescriptions $ traverseModuleItems $
    traverseStmts convertStmt

convertStmt :: Stmt -> Stmt
convertStmt (For (Left []) cc asgns stmt) =
    convertStmt $ For (Right []) cc asgns stmt
convertStmt (For (Right []) cc asgns stmt) =
    convertStmt $ For inits cc asgns stmt
    where inits = Left [dummyDecl $ Number "0"]
convertStmt (orig @ (For (Right [_]) _ _ _)) = orig

convertStmt (For (Left inits) cc asgns stmt) =
    Block Seq "" decls $
        initAsgns ++
        [For (Right [(lhs, expr)]) cc asgns stmt]
    where
        splitDecls = map splitDecl $ filter (not . isComment) inits
        decls = map fst splitDecls
        initAsgns = map asgnStmt $ init $ map snd splitDecls
        (lhs, expr) = snd $ last splitDecls

convertStmt (For (Right origPairs) cc asgns stmt) =
    Block Seq "" [] $
        initAsgns ++
        [For (Right [(lhs, expr)]) cc asgns stmt]
    where
        (lhs, expr) = last origPairs
        initAsgns = map asgnStmt $ init origPairs

convertStmt other = other

splitDecl :: Decl -> (Decl, (LHS, Expr))
splitDecl (decl @ (Variable _ _ _ _ Nil)) =
    error $ "invalid for loop decl: " ++ show decl
splitDecl (Variable d t ident a e) =
    (Variable d t ident a Nil, (LHSIdent ident, e))
splitDecl decl =
    error $ "invalid for loop decl: " ++ show decl

isComment :: Decl -> Bool
isComment CommentDecl{} = True
isComment _ = False

asgnStmt :: (LHS, Expr) -> Stmt
asgnStmt = uncurry $ Asgn AsgnOpEq Nothing

dummyDecl :: Expr -> Decl
dummyDecl = Variable Local (IntegerAtom TInteger Unspecified) "_sv2v_dummy" []
