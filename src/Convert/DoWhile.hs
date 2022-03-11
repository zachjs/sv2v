{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `do` `while` loops.
 -
 - These are converted into while loops with an extra condition which is
 - initially true and immediately set to false in the body. This strategy is
 - preferrable to simply duplicating the loop body as it could contain jumps.
 -}

module Convert.DoWhile (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $ traverseDescriptions $ traverseModuleItems $
    traverseStmts $ traverseNestedStmts convertStmt

convertStmt :: Stmt -> Stmt
convertStmt (DoWhile cond body) =
    Block Seq "" [decl] [While cond' body']
    where
        ident = "sv2v_do_while"
        typ = IntegerVector TLogic Unspecified []
        decl = Variable Local typ ident [] (RawNum 1)
        cond' = BinOp LogOr (Ident ident) cond
        asgn = Asgn AsgnOpEq Nothing (LHSIdent ident) (RawNum 0)
        body' = Block Seq "" [] [asgn, body]
convertStmt other = other
