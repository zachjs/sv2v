{- sv2v
- Author: Ethan Sifferman <ethan@sifferman.dev>
-
- Conversion of SystemVerilog System Tasks to Verilog.
-}

module Convert.SystemTasks (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions traverseDescription

elaborationFatalIdent :: Identifier
elaborationFatalIdent = "_sv2v_elaboration_fatal"

elaborationFatalCancelCode :: Expr
elaborationFatalCancelCode = RawNum (-1)

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

convertModuleItem :: ModuleItem -> ModuleItem

convertModuleItem (ElabTask SeverityInfo (Args args [])) =
    Initial (Block Seq "" [] [
        (Subroutine (Ident "$display") (Args ([(String "Elaboration Info: ")] ++ args) []))
    ])

convertModuleItem (ElabTask SeverityWarning (Args args [])) =
    Initial (Block Seq "" [] [
        (Subroutine (Ident "$display") (Args ([(String "Elaboration Warning: ")] ++ args) []))
    ])

convertModuleItem (ElabTask SeverityError (Args args [])) =
    Initial (Block Seq "" [] [
        (Subroutine (Ident "$display") (Args ([(String "Elaboration Error: ")] ++ args) []))
    ])

convertModuleItem (ElabTask SeverityFatal (Args [] [])) =
    Initial (Block Seq "" [] [
        (Subroutine (Ident "$display") (Args [(String "Elaboration Fatal: ")] [])),
        (Asgn AsgnOpEq Nothing (LHSIdent elaborationFatalIdent) (RawNum 0))
    ])

convertModuleItem (ElabTask SeverityFatal (Args (finishArgs:displayArgs) _)) =
    Initial (Block Seq "" [] [
        (Subroutine (Ident "$display") (Args ([(String "Elaboration Fatal: ")] ++ displayArgs) [])),
        (Asgn AsgnOpEq Nothing (LHSIdent elaborationFatalIdent) finishArgs)
    ])

convertModuleItem other =
    traverseStmts (traverseNestedStmts convertStmt) other

timeCall :: Expr
timeCall = Call (Ident "$time") (Args [] [])

convertStmt :: Stmt -> Stmt
convertStmt (Subroutine (Ident "$info") (Args args [])) =
    Block Seq "" [] [
        (Subroutine (Ident "$display") (Args ([(String "[%0t] Info: "), timeCall] ++ args) []))
    ]

convertStmt (Subroutine (Ident "$warning") (Args args [])) =
    Block Seq "" [] [
        (Subroutine (Ident "$display") (Args ([(String "[%0t] Warning: "), timeCall] ++ args) []))
    ]

convertStmt (Subroutine (Ident "$error") (Args args [])) =
    Block Seq "" [] [
        (Subroutine (Ident "$display") (Args ([(String "[%0t] Error: "), timeCall] ++ args) []))
    ]

convertStmt (Subroutine (Ident "$fatal") (Args [] [])) =
    Block Seq "" [] [
        (Subroutine (Ident "$display") (Args [(String "[%0t] Fatal: "), timeCall] [])),
        (Subroutine (Ident "$finish") (Args [] []))
    ]
convertStmt (Subroutine (Ident "$fatal") (Args (finishArgs:displayArgs) _)) =
    Block Seq "" [] [
        (Subroutine (Ident "$display") (Args ([(String "[%0t] Fatal: "), timeCall] ++ displayArgs) [])),
        (Subroutine (Ident "$finish") (Args [finishArgs] []))
    ]

convertStmt other = other
