{- sv2v
 - Author: Ethan Sifferman <ethan@sifferman.dev>
 -
 - Conversion of severity system tasks (IEEE 1800-2017 Section 20.10) and
 - elaboration system tasks (Section 20.11) `$info`, `$warning`, `$error`, and
 - `$fatal`, which sv2v collectively refers to as "severity tasks".
 -
 - 1. Severity task messages are converted into `$display` tasks.
 - 2. `$fatal` tasks also run `$finish` directly after running `$display`.
 -}

module Convert.SeverityTask (convert) where

import Data.Char (toUpper)
import Data.Functor ((<&>))

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type SC = Scoper ()

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions traverseDescription

traverseDescription :: Description -> Description
traverseDescription = partScoper return traverseModuleItem return traverseStmt

-- convert elaboration severity tasks
traverseModuleItem :: ModuleItem -> SC ModuleItem
traverseModuleItem (ElabTask severity taskArgs) =
    elab severity taskArgs "elaboration" [] <&> Initial
traverseModuleItem other = return other

-- convert standard severity tasks
traverseStmt :: Stmt -> SC Stmt
traverseStmt (SeverityStmt severity taskArgs) =
    elab severity taskArgs "%0t" [Ident "$time"]
traverseStmt other = return other

elab :: Severity -> [Expr] -> String -> [Expr] -> SC Stmt
elab severity args prefixStr prefixArgs = do
    scopeName <- hierarchyPathM
    fileLocation <- sourceLocationM
    let contextArg = String $ msg scopeName fileLocation
    let stmtDisplay = call "$display" $ contextArg : prefixArgs ++ displayArgs
    return $ Block Seq "" [] [stmtDisplay, stmtFinish]
    where
        msg scope file = severityToString severity ++ " [" ++ prefixStr ++ "] "
            ++ file ++ " - " ++ scope
            ++ if null displayArgs then "" else "\\n msg: "
        displayArgs = if severity /= SeverityFatal || null args
            then args
            else tail args
        stmtFinish = if severity /= SeverityFatal
            then Null
            else call "$finish" $ if null args then [] else [head args]

call :: Identifier -> [Expr] -> Stmt
call func args = Subroutine (Ident func) (Args args [])

severityToString :: Severity -> String
severityToString severity = toUpper ch : str
    where '$' : ch : str = show severity
