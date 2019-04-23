{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `unique`, `unique0`, and `priority`
 -
 - This conversion simply drops the keywords, as it is only used for
 - optimization. There is no way to force toolchains which don't support the
 - keyword to perform such optimization.
 -}

module Convert.Unique (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $ traverseDescriptions $ traverseModuleItems $ traverseStmts convertStmt

convertStmt :: Stmt -> Stmt
convertStmt (If (Just _) cc s1 s2) =
    If Nothing cc s1 s2
convertStmt (Case (Just _) kw expr cases def) =
    Case Nothing kw expr cases def
convertStmt other = other
