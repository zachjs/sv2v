{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog procedural statements
 -}

module Language.SystemVerilog.AST.Stmt
    ( Stmt   (..)
    , Timing (..)
    , Event        (..)
    , EventExpr    (..)
    , Edge         (..)
    , CaseKW (..)
    , Case
    , ActionBlock  (..)
    , PropExpr     (..)
    , SeqMatchItem (..)
    , SeqExpr      (..)
    , AssertionItem
    , Assertion    (..)
    , AssertionKind(..)
    , Deferral     (..)
    , PropertySpec (..)
    , ViolationCheck (..)
    , BlockKW (..)
    ) where

import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp (commas, indent, unlines', showPad, showBlock)
import Language.SystemVerilog.AST.Attr (Attr)
import Language.SystemVerilog.AST.Decl (Decl)
import Language.SystemVerilog.AST.Expr (Expr(Call, Ident, Nil), Args(..), Range, showRange, showAssignment)
import Language.SystemVerilog.AST.LHS (LHS)
import Language.SystemVerilog.AST.Op (AsgnOp(AsgnOpEq))
import Language.SystemVerilog.AST.Type (Identifier)

data Stmt
    = StmtAttr Attr Stmt
    | Block   BlockKW Identifier [Decl] [Stmt]
    | Case    ViolationCheck CaseKW Expr [Case]
    | For     [(LHS, Expr)] Expr [(LHS, AsgnOp, Expr)] Stmt
    | Asgn    AsgnOp (Maybe Timing) LHS Expr
    | While   Expr Stmt
    | RepeatL Expr Stmt
    | DoWhile Expr Stmt
    | Forever Stmt
    | Foreach Identifier [Identifier] Stmt
    | If      ViolationCheck Expr Stmt Stmt
    | Timing  Timing Stmt
    | Return  Expr
    | Subroutine Expr Args
    | Trigger Bool Identifier
    | Assertion Assertion
    | Force Bool LHS Expr
    | Continue
    | Break
    | Null
    | CommentStmt String
    deriving Eq

instance Show Stmt where
    showList l _ = unlines' $ map show l
    show (StmtAttr attr stmt) = printf "%s\n%s" (show attr) (show stmt)
    show (Block kw name decls stmts) =
        printf "%s%s\n%s\n%s" (show kw) header body (blockEndToken kw)
        where
            header = if null name then "" else " : " ++ name
            body = showBlock decls stmts
    show (Case u kw e cs) =
        printf "%s%s (%s)%s\n%s\nendcase" (showPad u) (show kw) (show e)
            insideStr bodyStr
        where
            insideStr = if kw == CaseInside then " inside" else ""
            bodyStr = indent $ unlines' $ map showCase cs
    show (For inits cond assigns stmt) =
        printf "for (%s; %s; %s)\n%s"
            (showInits inits)
            (show cond)
            (commas $ map showAssign assigns)
            (indent $ show stmt)
        where
            showInits :: [(LHS, Expr)] -> String
            showInits = commas . map showInit
                where showInit (l, e) = showAssign (l, AsgnOpEq, e)
    show (Subroutine e a) = printf "%s%s;" (show e) aStr
        where aStr = if a == Args [] [] then "" else show a
    show (Asgn  o t v e) = printf "%s %s %s%s;" (show v) (show o) tStr (show e)
        where tStr = maybe "" showPad t
    show (If u c s Null) = printf "%sif (%s)%s"         (showPad u) (show c) (showBranch s)
    show (If u c s1 s2 ) = printf "%sif (%s)%s\nelse%s" (showPad u) (show c) (showBlockedBranch s1) (showElseBranch s2)
    show (While     e s) = printf  "while (%s) %s" (show e) (show s)
    show (RepeatL   e s) = printf "repeat (%s) %s" (show e) (show s)
    show (DoWhile   e s) = printf "do %s while (%s);" (show s) (show e)
    show (Forever     s) = printf "forever %s" (show s)
    show (Foreach x i s) = printf "foreach (%s [ %s ]) %s" x (commas i) (show s)
    show (Return    e  ) = printf "return %s;" (show e)
    show (Timing    t s) = printf "%s%s" (show t) (showShortBranch s)
    show (Trigger   b x) = printf "->%s %s;" (if b then "" else ">") x
    show (Assertion   a) = show a
    show (Force  kw l e) = printf "%s %s%s;" kwStr (show l) (showAssignment e)
        where
            kwStr = case (kw, e /= Nil) of
                (True , True ) -> "force"
                (True , False) -> "release"
                (False, True ) -> "assign"
                (False, False) -> "deassign"
    show (Continue     ) = "continue;"
    show (Break        ) = "break;"
    show (Null         ) = ";"
    show (CommentStmt c) = "// " ++ c

showAssign :: (LHS, AsgnOp, Expr) -> String
showAssign (l, op, e) = (showPad l) ++ (showPad op) ++ (show e)

showBranch :: Stmt -> String
showBranch (Block Seq "" [] stmts@[CommentStmt{}, _]) =
    '\n' : (indent $ show stmts)
showBranch block@Block{} = ' ' : show block
showBranch stmt = '\n' : (indent $ show stmt)

showBlockedBranch :: Stmt -> String
showBlockedBranch stmt =
    showBranch $
    if isControl stmt
        then Block Seq "" [] [stmt]
        else stmt
    where
        isControl s = case s of
            If{} -> True
            For{} -> True
            While{} -> True
            RepeatL{} -> True
            DoWhile{} -> True
            Forever{} -> True
            Foreach{} -> True
            Timing _ subStmt -> isControl subStmt
            Block Seq "" [] [CommentStmt{}, subStmt] -> isControl subStmt
            _ -> False

showElseBranch :: Stmt -> String
showElseBranch stmt@If{} = ' ' : show stmt
showElseBranch stmt = showBranch stmt

showShortBranch :: Stmt -> String
showShortBranch stmt@Asgn{} = ' ' : show stmt
showShortBranch stmt = showBranch stmt

showCase :: Case -> String
showCase (a, b) = printf "%s:%s" exprStr (showShortBranch b)
    where
        exprStr = case a of
            [] -> "default"
            _ -> commas $ map show a

data CaseKW
    = CaseN
    | CaseZ
    | CaseX
    | CaseInside
    deriving Eq

instance Show CaseKW where
    show CaseN = "case"
    show CaseZ = "casez"
    show CaseX = "casex"
    show CaseInside = "case"

type Case = ([Expr], Stmt)

data Timing
    = Event Event
    | Delay Expr
    | Cycle Expr
    deriving Eq

instance Show Timing where
    show (Event e) = printf  "@(%s)" (show e)
    show (Delay e) = printf  "#(%s)" (show e)
    show (Cycle e) = printf "##(%s)" (show e)

data Event
    = EventStar
    | EventExpr EventExpr
    deriving Eq

instance Show Event where
    show EventStar = "*"
    show (EventExpr e) = show e

data EventExpr
    = EventExprEdge Edge Expr
    | EventExprOr EventExpr EventExpr
    deriving Eq

instance Show EventExpr where
    show (EventExprEdge g e) = printf "%s%s" (showPad g) (show e)
    show (EventExprOr   a b) = printf "%s or %s" (show a) (show b)

data Edge
    = Posedge
    | Negedge
    | Edge
    | NoEdge
    deriving Eq

instance Show Edge where
    show Posedge = "posedge"
    show Negedge = "negedge"
    show Edge    = "edge"
    show NoEdge  = ""

data ActionBlock
    = ActionBlock Stmt Stmt
    deriving Eq
instance Show ActionBlock where
    show (ActionBlock s Null) = printf " %s" (show s)
    show (ActionBlock Null s) = printf " else %s" (show s)
    show (ActionBlock s1  s2) = printf " %s else %s" (show s1) (show s2)

data PropExpr
    = PropExpr SeqExpr
    | PropExprImpliesO  SeqExpr PropExpr
    | PropExprImpliesNO SeqExpr PropExpr
    | PropExprFollowsO  SeqExpr PropExpr
    | PropExprFollowsNO SeqExpr PropExpr
    | PropExprIff PropExpr PropExpr
    deriving Eq
instance Show PropExpr where
    show (PropExpr se) = show se
    show (PropExprImpliesO  a b) = printf "(%s |-> %s)" (show a) (show b)
    show (PropExprImpliesNO a b) = printf "(%s |=> %s)" (show a) (show b)
    show (PropExprFollowsO  a b) = printf "(%s #-# %s)" (show a) (show b)
    show (PropExprFollowsNO a b) = printf "(%s #=# %s)" (show a) (show b)
    show (PropExprIff a b) = printf "(%s iff %s)" (show a) (show b)
data SeqMatchItem
    = SeqMatchAsgn (LHS, AsgnOp, Expr)
    | SeqMatchCall Identifier Args
    deriving Eq
instance Show SeqMatchItem where
    show (SeqMatchAsgn asgn) = showAssign asgn
    show (SeqMatchCall ident args) = show $ Call (Ident ident) args
data SeqExpr
    = SeqExpr Expr
    | SeqExprAnd        SeqExpr SeqExpr
    | SeqExprOr         SeqExpr SeqExpr
    | SeqExprIntersect  SeqExpr SeqExpr
    | SeqExprThroughout Expr    SeqExpr
    | SeqExprWithin     SeqExpr SeqExpr
    | SeqExprDelay (Maybe SeqExpr) Range SeqExpr
    | SeqExprFirstMatch SeqExpr [SeqMatchItem]
    deriving Eq
instance Show SeqExpr where
    show (SeqExpr           a  ) = show a
    show (SeqExprAnd        a b) = printf "(%s %s %s)" (show a) "and"        (show b)
    show (SeqExprOr         a b) = printf "(%s %s %s)" (show a) "or"         (show b)
    show (SeqExprIntersect  a b) = printf "(%s %s %s)" (show a) "intersect"  (show b)
    show (SeqExprThroughout a b) = printf "(%s %s %s)" (show a) "throughout" (show b)
    show (SeqExprWithin     a b) = printf "(%s %s %s)" (show a) "within"     (show b)
    show (SeqExprDelay   me r s) = printf "%s##%s %s" (maybe "" showPad me) (showCycleDelayRange r) (show s)
    show (SeqExprFirstMatch e a) = printf "first_match(%s, %s)" (show e) (commas $ map show a)

showCycleDelayRange :: Range -> String
showCycleDelayRange (Nil, e) = printf "(%s)" (show e)
showCycleDelayRange (e, Nil) = printf "[%s:$]" (show e)
showCycleDelayRange r = showRange r

type AssertionItem = (Identifier, Assertion)

data Assertion
    = Assert AssertionKind ActionBlock
    | Assume AssertionKind ActionBlock
    | Cover  AssertionKind Stmt
    deriving Eq
instance Show Assertion where
    show (Assert k a) = printf "assert %s%s" (show k) (show a)
    show (Assume k a) = printf "assume %s%s" (show k) (show a)
    show (Cover  k a) = printf  "cover %s%s" (show k) (show a)

data AssertionKind
    = Concurrent PropertySpec
    | Immediate Deferral Expr
    deriving Eq
instance Show AssertionKind where
    show (Concurrent e) = printf "property (%s\n)" (show e)
    show (Immediate d e) = printf "%s(%s)" (showPad d) (show e)

data Deferral
    = NotDeferred
    | ObservedDeferred
    | FinalDeferred
    deriving Eq
instance Show Deferral where
    show NotDeferred = ""
    show ObservedDeferred = "#0"
    show FinalDeferred = "final"

data PropertySpec
    = PropertySpec (Maybe EventExpr) Expr PropExpr
    deriving Eq
instance Show PropertySpec where
    show (PropertySpec mv e pe) =
        printf "%s%s\n\t%s" mvStr eStr (show pe)
        where
            mvStr = case mv of
                Nothing -> ""
                Just v -> printf "@(%s) " (show v)
            eStr = case e of
                Nil -> ""
                _ -> printf "disable iff (%s)" (show e)

data ViolationCheck
    = Unique
    | Unique0
    | Priority
    | NoCheck
    deriving Eq

instance Show ViolationCheck where
    show Unique   = "unique"
    show Unique0  = "unique0"
    show Priority = "priority"
    show NoCheck  = ""

data BlockKW
    = Seq
    | Par
    deriving Eq

instance Show BlockKW where
    show Seq = "begin"
    show Par = "fork"

blockEndToken :: BlockKW -> Identifier
blockEndToken Seq = "end"
blockEndToken Par = "join"
