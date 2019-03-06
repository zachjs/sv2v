{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion which makes function `logic` return types implicit
 -}

module Convert.FuncRet (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert = traverseDescriptions $ traverseModuleItems convertFunction

convertFunction :: ModuleItem -> ModuleItem
convertFunction (Function ml (Logic r) f decls stmts) =
    Function ml (Implicit r) f decls stmts
convertFunction other = other
