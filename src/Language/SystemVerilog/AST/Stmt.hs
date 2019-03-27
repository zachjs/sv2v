{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog procedural statements
 -}

module Language.SystemVerilog.AST.Stmt
    ( Stmt   (..)
    , Timing (..)
    , Sense  (..)
    , CaseKW (..)
    , Case
    ) where

import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp (commas, indent, unlines', showPad, showCase)
import Language.SystemVerilog.AST.Attr (Attr)
import Language.SystemVerilog.AST.Decl (Decl)
import Language.SystemVerilog.AST.Expr (Expr)
import Language.SystemVerilog.AST.LHS (LHS)
import Language.SystemVerilog.AST.Op (AsgnOp)
import Language.SystemVerilog.AST.Type (Identifier)

data Stmt
    = StmtAttr Attr Stmt
    | Block   (Maybe Identifier) [Decl] [Stmt]
    | Case    Bool CaseKW Expr [Case] (Maybe Stmt)
    | For     [Either Decl (LHS, Expr)] (Maybe Expr) [(LHS, AsgnOp, Expr)] Stmt
    | AsgnBlk AsgnOp LHS Expr
    | Asgn    (Maybe Timing) LHS Expr
    | While   Expr Stmt
    | RepeatL Expr Stmt
    | DoWhile Expr Stmt
    | Forever Stmt
    | If      Expr Stmt Stmt
    | Timing  Timing Stmt
    | Return  Expr
    | Subroutine Identifier [Maybe Expr]
    | Trigger Identifier
    | Null
    deriving Eq

instance Show Stmt where
    show (StmtAttr attr stmt) = printf "%s\n%s" (show attr) (show stmt)
    show (Block name decls stmts) =
        printf "begin%s\n%s\nend" header body
        where
            header = maybe "" (" : " ++) name
            bodyLines = (map show decls) ++ (map show stmts)
            body = indent $ unlines' bodyLines
    show (Case u kw e cs def) =
        printf "%s%s (%s)\n%s%s\nendcase" uniqStr (show kw) (show e) bodyStr defStr
        where
            uniqStr = if u then "unique " else ""
            bodyStr = indent $ unlines' $ map showCase cs
            defStr = case def of
                Nothing -> ""
                Just c -> printf "\n\tdefault: %s" (show c)
    show (For inits mc assigns stmt) =
        printf "for (%s; %s; %s)\n%s"
            (commas $ map showInit inits)
            (maybe "" show mc)
            (commas $ map showAssign assigns)
            (indent $ show stmt)
        where
            showInit :: Either Decl (LHS, Expr) -> String
            showInit (Left d) = init $ show d
            showInit (Right (l, e)) = printf "%s = %s" (show l) (show e)
            showAssign :: (LHS, AsgnOp, Expr) -> String
            showAssign (l, op, e) = printf "%s %s %s" (show l) (show op) (show e)
    show (Subroutine x a) = printf "%s(%s);" x (commas $ map (maybe "" show) a)
    show (AsgnBlk o v e) = printf "%s %s %s;" (show v) (show o) (show e)
    show (Asgn    t v e) = printf "%s <= %s%s;" (show v) (maybe "" showPad t) (show e)
    show (While   e s) = printf  "while (%s) %s" (show e) (show s)
    show (RepeatL e s) = printf "repeat (%s) %s" (show e) (show s)
    show (DoWhile e s) = printf "do %s while (%s);" (show s) (show e)
    show (Forever s  ) = printf "forever %s" (show s)
    show (If a b Null) = printf "if (%s) %s"         (show a) (show b)
    show (If a b c   ) = printf "if (%s) %s\nelse %s" (show a) (show b) (show c)
    show (Return e   ) = printf "return %s;" (show e)
    show (Timing t s ) = printf "%s %s" (show t) (show s)
    show (Trigger x  ) = printf "-> %s;" x
    show (Null       ) = ";"

data CaseKW
    = CaseN
    | CaseZ
    | CaseX
    deriving Eq

instance Show CaseKW where
    show CaseN = "case"
    show CaseZ = "casez"
    show CaseX = "casex"

type Case = ([Expr], Stmt)

data Timing
    = Event Sense
    | Delay Expr
    | Cycle Expr
    deriving Eq

instance Show Timing where
    show (Event s) = printf  "@(%s)" (show s)
    show (Delay e) = printf  "#(%s)" (show e)
    show (Cycle e) = printf "##(%s)" (show e)

data Sense
    = Sense        LHS
    | SenseOr      Sense Sense
    | SensePosedge LHS
    | SenseNegedge LHS
    | SenseStar
    deriving Eq

instance Show Sense where
    show (Sense        a  ) = show a
    show (SenseOr      a b) = printf "%s or %s" (show a) (show b)
    show (SensePosedge a  ) = printf "posedge %s" (show a)
    show (SenseNegedge a  ) = printf "negedge %s" (show a)
    show (SenseStar       ) = "*"
