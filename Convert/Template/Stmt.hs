{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Template converter for Stmt transformations
 -}

module Convert.Template.Stmt (stmtConverter) where

import Convert.Template.ModuleItem (moduleItemConverter)
import Language.SystemVerilog.AST

type Converter = Stmt -> Stmt

stmtConverter :: Converter -> (AST -> AST)
stmtConverter = moduleItemConverter . convertModuleItem

convertModuleItem :: Converter -> ModuleItem -> ModuleItem
convertModuleItem f (AlwaysC kw stmt) =
    AlwaysC kw (convertStmt f stmt)
convertModuleItem f (Function ret name decls stmt) =
    Function ret name decls (convertStmt f stmt)
convertModuleItem _ other = other

convertStmt :: Converter -> (Stmt -> Stmt)
convertStmt f = f . convertStmt'
    where
        cs :: Stmt -> Stmt
        cs = convertStmt f
        convertStmt' :: Stmt -> Stmt
        convertStmt' (Block decls stmts) = Block decls (map cs stmts)
        convertStmt' (Case kw expr cases def) =
            Case kw expr cases' def'
            where
                cases' = map (\(exprs, stmt) -> (exprs, cs stmt)) cases
                def' = maybe Nothing (Just . cs) def
        convertStmt' (AsgnBlk lhs expr) = AsgnBlk lhs expr
        convertStmt' (Asgn    lhs expr) = Asgn    lhs expr
        convertStmt' (For a b c stmt) = For a b c (cs stmt)
        convertStmt' (If e s1 s2) = If e (cs s1) (cs s2)
        convertStmt' (Timing sense stmt) = Timing sense (cs stmt)
        convertStmt' (Null) = Null
