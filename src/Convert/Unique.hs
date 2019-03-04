{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `unique`
 -
 - This conversion simply drops the `unique` keyword, as it is only used for
 - optimization. There is no way to force toolchains which don't support the
 - keyword to perform such optimization.
 -}

module Convert.Unique (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert = traverseDescriptions $ traverseModuleItems $ traverseStmts convertStmt

convertStmt :: Stmt -> Stmt
convertStmt (Case True kw expr cases def) =
    Case False kw expr cases def
convertStmt other = other
