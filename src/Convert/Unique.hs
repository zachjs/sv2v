{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `unique`, `unique0`, and `priority` (verification checks)
 -
 - This conversion adds full_case and parallel_case synthesis attributes
 - for priority and unique respectively.
 -}

module Convert.Unique (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $ traverseDescriptions $ traverseModuleItems $ traverseStmts $
        traverseNestedStmts convertStmt

convertStmt :: Stmt -> Stmt
convertStmt (If _ cc s1 s2) =
    If NoCheck cc s1 s2
convertStmt (Case Priority kw expr cases) =
    StmtAttr caseAttr caseStmt
    where
        caseAttr = Attr [("synthesis", Nil), ("full_case", Nil)]
        caseStmt = Case NoCheck kw expr cases
convertStmt (Case Unique kw expr cases) =
    StmtAttr caseAttr caseStmt
    where
        caseAttr = Attr [("synthesis", Nil), ("parallel_case", Nil)]
        caseStmt = Case NoCheck kw expr cases
convertStmt (Case Unique0 kw expr cases) =
    convertStmt (Case Unique kw expr cases)
convertStmt other = other
