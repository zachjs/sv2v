{- sv2v
- Author: Ethan Sifferman <ethan@sifferman.dev>
-
- Conversion of SystemVerilog System Tasks to Verilog.
-}

module Convert.SystemTasks (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $ traverseDescriptions $ traverseModuleItems $
    traverseStmts $ traverseNestedStmts convertStmt

timeCall :: Expr
timeCall = Call (Ident "$time") (Args [] [])

convertStmt :: Stmt -> Stmt
convertStmt (Subroutine (Ident "$info") args) =
    Block Seq "" [] [
        (Subroutine (Ident "$write") (Args [(String "[%0t] Info: "), timeCall] [])),
        (Subroutine (Ident "$display") args)
    ]

convertStmt (Subroutine (Ident "$warning") args) =
    Block Seq "" [] [
        (Subroutine (Ident "$write") (Args [(String "[%0t] Warning: "), timeCall] [])),
        (Subroutine (Ident "$display") args)
    ]

convertStmt (Subroutine (Ident "$error") args) =
    Block Seq "" [] [
        (Subroutine (Ident "$write") (Args [(String "[%0t] Error: "), timeCall] [])),
        (Subroutine (Ident "$display") args)
    ]

convertStmt (Subroutine (Ident "$fatal") (Args [] [])) =
    Block Seq "" [] [
        (Subroutine (Ident "$display") (Args [(String "[%0t] Fatal:"), timeCall] [])),
        (Subroutine (Ident "$finish") (Args [] []))
    ]
convertStmt (Subroutine (Ident "$fatal") (Args (finishArgs:displayArgs) _)) =
    Block Seq "" [] [
        (Subroutine (Ident "$write") (Args [(String "[%0t] Fatal: "), timeCall] [])),
        (Subroutine (Ident "$display") (Args displayArgs [])),
        (Subroutine (Ident "$finish") (Args [finishArgs] []))
    ]

convertStmt other = other
