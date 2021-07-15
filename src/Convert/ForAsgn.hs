{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Verilog-2005 requires that for loops have have one initialization and one
 - incrementation. If there are excess initializations, they are turned into
 - preceding statements. If there is no loop variable, a dummy loop variable is
 - created. If there are multiple incrementations, they are all safely combined
 - into a single concatenation. If there is no incrementation, a no-op
 - assignment is added.
 -}

module Convert.ForAsgn (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $ traverseDescriptions $ traverseModuleItems $
    traverseStmts $ traverseNestedStmts convertStmt

convertStmt :: Stmt -> Stmt

-- for loop with multiple incrementations
convertStmt (For inits cond incrs@(_ : _ : _) stmt) =
    convertStmt $ For inits cond incrs' stmt
    where
        incrs' = [(LHSConcat lhss, AsgnOpEq, Concat exprs)]
        lhss = map (\(lhs, _, _) -> lhs) incrs
        exprs = map toRHS incrs
        toRHS :: (LHS, AsgnOp, Expr) -> Expr
        toRHS (lhs, AsgnOpEq, expr) =
            Cast (Left $ TypeOf $ lhsToExpr lhs) expr
        toRHS (lhs, asgnop, expr) =
            toRHS (lhs, AsgnOpEq, BinOp binop (lhsToExpr lhs) expr)
            where AsgnOp binop = asgnop

-- for loop with no initializations
convertStmt (For [] cond incrs stmt) =
    Block Seq "" [dummyDecl Nil] $ pure $
    For [(LHSIdent dummyIdent, RawNum 0)] cond incrs stmt

-- for loop with no incrementations
convertStmt (For inits cond [] stmt) =
    convertStmt $ For inits cond incrs stmt
    where
        (lhs, _) : _ = inits
        incrs = [(lhs, AsgnOpEq, lhsToExpr lhs)]

-- for loop with multiple initializations
convertStmt (For inits@(_ : _ : _) cond incrs@[_] stmt) =
    Block Seq "" [] $
        (map asgnStmt $ init inits) ++
        [For [last inits] cond incrs stmt]

convertStmt other = other

asgnStmt :: (LHS, Expr) -> Stmt
asgnStmt = uncurry $ Asgn AsgnOpEq Nothing

dummyIdent :: Identifier
dummyIdent = "_sv2v_dummy"

dummyDecl :: Expr -> Decl
dummyDecl = Variable Local (IntegerAtom TInteger Unspecified) dummyIdent []
