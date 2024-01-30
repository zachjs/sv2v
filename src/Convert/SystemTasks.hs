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

convertModuleItem (ElabTask SeverityInfo args) =
    Initial (Block Seq "" [] [
        (Subroutine (Ident "$write") (Args [(String "Elaboration Info: ")] [])),
        (Subroutine (Ident "$display") args)
    ])

convertModuleItem (ElabTask SeverityWarning args) =
    Initial (Block Seq "" [] [
        (Subroutine (Ident "$write") (Args [(String "Elaboration Warning: ")] [])),
        (Subroutine (Ident "$display") args)
    ])

convertModuleItem (ElabTask SeverityError args) =
    Initial (Block Seq "" [] [
        (Subroutine (Ident "$write") (Args [(String "Elaboration Error: ")] [])),
        (Subroutine (Ident "$display") args)
    ])

convertModuleItem (ElabTask SeverityFatal (Args [] [])) =
    Initial (Block Seq "" [] [
        (Subroutine (Ident "$display") (Args [(String "Elaboration Fatal:")] [])),
        (Subroutine (Ident "$finish") (Args [] []))
    ])

convertModuleItem (ElabTask SeverityFatal (Args (finishArgs:displayArgs) _)) =
    Initial (Block Seq "" [] [
        (Subroutine (Ident "$write") (Args [(String "Elaboration Fatal: ")] [])),
        (Subroutine (Ident "$display") (Args displayArgs [])),
        (Subroutine (Ident "$finish") (Args [finishArgs] []))
    ])

convertModuleItem other =
    traverseStmts (traverseNestedStmts convertStmt) other

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
