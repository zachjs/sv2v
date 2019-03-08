{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion which makes function `logic` and `reg` return types implicit
 -}

module Convert.FuncRet (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert = traverseDescriptions $ traverseModuleItems convertFunction

convertFunction :: ModuleItem -> ModuleItem
convertFunction (MIPackageItem (Function ml t f decls stmts)) =
    MIPackageItem $ Function ml t' f decls stmts
    where
        t' = case t of
            Reg rs -> Implicit rs
            Logic rs -> Implicit rs
            _ -> t
convertFunction other = other
