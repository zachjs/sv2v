{- sv2v
 - Author: Ethan Sifferman <ethan@sifferman.dev>
 -
 - Conversion of standard and elaboration severity tasks `$info`, `$warning`,
 - `$error`, and `$fatal` (20.10 and 20.11).
 -
 -  * Severity task messages are converted into `$display` tasks.
 -  * Standard `$fatal` tasks run `$finish` directly after running `$display`.
 -  * Elaboration `$fatal` tasks set `_sv2v_elaboration_fatal` to a
 -    non-negative value, causing the simulation to exit once all elaboration
 -    severity task messages have been printed.
 -}

module Convert.SeverityTask (convert) where

import Data.Char (toUpper)

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions traverseDescription

-- Identifiers for fatal elaboration checker
elaborationFatalIdent :: Identifier
elaborationFatalIdent = "_sv2v_elaboration_fatal"
elaborationFatalCancelCode :: Expr
elaborationFatalCancelCode = UniOp UniSub $ RawNum 1

-- Checker for fatal elaboration
elaborationFatalDecl :: ModuleItem
elaborationFatalDecl = MIPackageItem $ Decl $ Variable Local t elaborationFatalIdent [] elaborationFatalCancelCode
    where t = IntegerAtom TInteger Unspecified
elaborationFatalCheck :: ModuleItem
elaborationFatalCheck = Initial (Block Seq "" [] [
        zeroDelay,
        If NoCheck checkCancelCode finishCall Null
    ]) where
        zeroDelay = Timing (Delay (RawNum 0)) Null
        checkCancelCode = BinOpA Ne [] (Ident elaborationFatalIdent) elaborationFatalCancelCode
        finishCall = Subroutine (Ident "$finish") (Args [(Ident elaborationFatalIdent)] [])

-- If SV code uses elab fatal, add checker for fatal elaboration
traverseDescription :: Description -> Description
traverseDescription (Part att ext kw lif name pts items) =
    traverseModuleItems convertModuleItem $
    Part att ext kw lif name pts $
    if hasElaborationFatal
        then elaborationFatalDecl : items ++ [elaborationFatalCheck]
        else items
    where
        hasElaborationFatal = any isElabTaskFatal items
        isElabTaskFatal (ElabTask SeverityFatal _) = True
        isElabTaskFatal _ = False
traverseDescription description = traverseModuleItems convertModuleItem description

-- Convert Elaboration Severity Tasks
severityToString :: Severity -> String
severityToString severity = toUpper ch : str
    where '$' : ch : str = show severity

severityElabTaskToDisplay :: ModuleItem -> [Stmt]
severityElabTaskToDisplay (ElabTask severity taskArgs) =
    [Subroutine (Ident "$display") (Args (
        [(String ("Elaboration "++(severityToString severity)++":"++(trailingSpace)))] ++ args
    ) [])]
    where
        args = if severity /= SeverityFatal || null taskArgs
            then taskArgs
            else tail taskArgs
        trailingSpace = if (length args) > 0 then " " else ""
severityElabTaskToDisplay _ = []

severityElabTaskToFinish :: ModuleItem -> [Stmt]
severityElabTaskToFinish (ElabTask SeverityFatal []) =
    [Asgn AsgnOpEq Nothing (LHSIdent elaborationFatalIdent) (RawNum 0)]
severityElabTaskToFinish (ElabTask SeverityFatal (finishArgs : _)) =
    [Asgn AsgnOpEq Nothing (LHSIdent elaborationFatalIdent) finishArgs]
severityElabTaskToFinish _ = []

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem task@(ElabTask _ _) =
    Initial (Block Seq "" [] ((severityElabTaskToDisplay task) ++ (severityElabTaskToFinish task)))
convertModuleItem other =
    traverseStmts (traverseNestedStmts convertStmt) other

-- Convert Standard Severity Tasks

timeExpr :: Expr
timeExpr = Ident "$time"

severityTaskToDisplay :: Stmt -> [Stmt]
severityTaskToDisplay (SeverityStmt severity taskArgs) =
    [Subroutine (Ident "$display") (Args (
        [(String ("[%0t] "++(severityToString severity)++":"++(trailingSpace))), timeExpr] ++ args
    ) [])]
    where
        args = if severity /= SeverityFatal || null taskArgs
            then taskArgs
            else tail taskArgs
        trailingSpace = if (length args) > 0 then " " else ""
severityTaskToDisplay _ = []

severityTaskToFinish :: Stmt -> [Stmt]
severityTaskToFinish (SeverityStmt SeverityFatal []) =
    [Subroutine (Ident "$finish") (Args [] [])]
severityTaskToFinish (SeverityStmt SeverityFatal (finishArgs : _)) =
    [Subroutine (Ident "$finish") (Args [finishArgs] [])]
severityTaskToFinish _ = []

convertStmt :: Stmt -> Stmt
convertStmt task@SeverityStmt{} =
    Block Seq "" [] $ severityTaskToDisplay task ++ severityTaskToFinish task
convertStmt other = other
