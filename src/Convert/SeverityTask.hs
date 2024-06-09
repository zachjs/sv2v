{- sv2v
 - Author: Ethan Sifferman <ethan@sifferman.dev>
 -
 - Conversion of standard and elaboration severity tasks `$info`, `$warning`,
 - `$error`, and `$fatal` (20.10 and 20.11).
 -
 - 1. Severity task messages are converted into `$display` tasks.
 - 2. `$fatal` tasks run `$finish` directly after running `$display`.
 -}

module Convert.SeverityTask (convert) where

import Data.Char (toUpper)

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems convertModuleItem

-- Convert Elaboration Severity Tasks
convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem (ElabTask severity taskArgs) =
    Initial $ elab severity taskArgs "Elaboration" []
convertModuleItem other =
    traverseStmts (traverseNestedStmts convertStmt) other

-- Convert Standard Severity Tasks
convertStmt :: Stmt -> Stmt
convertStmt (SeverityStmt severity taskArgs) =
    elab severity taskArgs "[%0t]" [Ident "$time"]
convertStmt other = other

elab :: Severity -> [Expr] -> String -> [Expr] -> Stmt
elab severity taskArgs prefixStr prefixArgs =
    Block Seq "" [] [stmtDisplay, stmtFinish]
    where
        stmtDisplay = call "$display" $ msg : prefixArgs ++ args
        msg = String $ prefixStr ++ ' ' : severityToString severity ++ ':' : trailingSpace
        trailingSpace = if null args then "" else " "
        args = if severity /= SeverityFatal || null taskArgs
            then taskArgs
            else tail taskArgs
        stmtFinish = if severity /= SeverityFatal
            then Null
            else call "$finish" argsFinish
        argsFinish = if null taskArgs then [] else [head taskArgs]

call :: Identifier -> [Expr] -> Stmt
call func args = Subroutine (Ident func) (Args args [])

severityToString :: Severity -> String
severityToString severity = toUpper ch : str
    where '$' : ch : str = show severity
