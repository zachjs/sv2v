{- sv2v
 - Author: Ethan Sifferman <ethan@sifferman.dev>
 -
 - Conversion of standard and elaboration severity tasks `$info`, `$warning`,
 - `$error`, and `$fatal` (20.10 and 20.11).
 -
 - 1. Severity task messages are converted into `$display` tasks.
 - 2. Standard `$fatal` tasks run `$finish` directly after running `$display`.
 - 3. Elaboration `$fatal` tasks set `_sv2v_elaboration_fatal` to a non-negative
 - value, causing the simulation to exit once all elaboration severity task
 - messages have been printed.
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
elaborationFatalDecl =
    MIPackageItem $ Decl $
    Variable Local t elaborationFatalIdent [] elaborationFatalCancelCode
    where t = IntegerAtom TInteger Unspecified
elaborationFatalCheck :: ModuleItem
elaborationFatalCheck =
    Initial $ Block Seq "" []
    [ zeroDelay
    , If NoCheck checkCancelCode finishCall Null
    ]
    where
        zeroDelay = Timing (Delay $ RawNum 0) Null
        checkCancelCode = BinOpA Ne [] (Ident elaborationFatalIdent) elaborationFatalCancelCode
        finishCall = call "$finish" [Ident elaborationFatalIdent]

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

severityToString :: Severity -> String
severityToString severity = toUpper ch : str
    where '$' : ch : str = show severity

-- Convert Elaboration Severity Tasks
convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem (ElabTask severity taskArgs) =
    Initial $ Block Seq "" [] [stmtDisplay, stmtFinish]
    where
        stmtDisplay = call "$display" $ msg : args
        args = if severity /= SeverityFatal || null taskArgs
            then taskArgs
            else tail taskArgs
        msg = String $ "Elaboration " ++ severityToString severity ++ ':' : trailingSpace
        trailingSpace = if length args > 0 then " " else ""

        stmtFinish = if severity /= SeverityFatal
            then Null
            else Asgn AsgnOpEq Nothing (LHSIdent elaborationFatalIdent) finishArg
        finishArg = if null taskArgs then RawNum 0 else head taskArgs

convertModuleItem other =
    traverseStmts (traverseNestedStmts convertStmt) other

timeExpr :: Expr
timeExpr = Ident "$time"

-- Convert Standard Severity Tasks
convertStmt :: Stmt -> Stmt
convertStmt (SeverityStmt severity taskArgs) =
    Block Seq "" [] [stmtDisplay, stmtFinish]
    where
        stmtDisplay = call "$display" $ [msg, timeExpr] ++ args
        msg = String $ "[%0t] " ++ severityToString severity ++ ':' : trailingSpace
        trailingSpace = if length args > 0 then " " else ""
        args = if severity /= SeverityFatal || null taskArgs
            then taskArgs
            else tail taskArgs

        stmtFinish = if severity /= SeverityFatal
            then Null
            else call "$finish" argsFinish
        argsFinish = if null taskArgs then [] else [head taskArgs]

convertStmt other = other

call :: Identifier -> [Expr] -> Stmt
call func args = Subroutine (Ident func) (Args args [])
