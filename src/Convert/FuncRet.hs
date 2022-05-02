{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion which makes function `logic` and `reg` return types implicit and
 - converts `void` functions to tasks
 -
 - Verilog-2005 restricts function return types to `integer`, `real`,
 - `realtime`, `time`, and implicit signed/dimensioned types.
 -}

module Convert.FuncRet (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems convertFunction

convertFunction :: ModuleItem -> ModuleItem
convertFunction (MIPackageItem (Function ml Void f decls stmts)) =
    MIPackageItem $ Task ml f decls stmts
convertFunction (MIPackageItem (Function ml t f decls stmts)) =
    MIPackageItem $ Function ml t' f decls stmts
    where
        t' = case t of
            IntegerVector TReg   sg rs -> Implicit sg rs
            _ -> t
convertFunction other = other
