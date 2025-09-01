{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog `module` items
 -}

module Language.SystemVerilog.AST.ModuleItem
    ( ModuleItem    (..)
    , PortBinding
    , ModportDecl
    , AlwaysKW      (..)
    , NInputGateKW  (..)
    , NOutputGateKW (..)
    , AssignOption  (..)
    , AssertionItem (..)
    ) where

import Data.List (intercalate)
import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp

import Language.SystemVerilog.AST.Attr (Attr)
import Language.SystemVerilog.AST.Decl (Direction)
import Language.SystemVerilog.AST.Description (PackageItem)
import Language.SystemVerilog.AST.Expr (Expr(Nil), pattern Ident, Range, showRanges, ParamBinding, showParams, Args(Args))
import Language.SystemVerilog.AST.GenItem (GenItem)
import Language.SystemVerilog.AST.LHS (LHS)
import Language.SystemVerilog.AST.Stmt (Stmt, Assertion, Severity, Timing(Delay), PropertySpec, SeqExpr)
import Language.SystemVerilog.AST.Type (Identifier, Strength0, Strength1)

data ModuleItem
    = MIAttr     Attr ModuleItem
    | AlwaysC    AlwaysKW Stmt
    | Assign     AssignOption LHS Expr
    | Defparam   LHS Expr
    | Instance   Identifier [ParamBinding] Identifier [Range] [PortBinding]
    | Genvar     Identifier
    | Generate   [GenItem]
    | Modport    Identifier [ModportDecl]
    | Initial    Stmt
    | Final      Stmt
    | ElabTask   Severity [Expr]
    | MIPackageItem PackageItem
    | NInputGate  NInputGateKW  Expr Identifier [Range]  LHS [Expr]
    | NOutputGate NOutputGateKW Expr Identifier [Range] [LHS] Expr
    | AssertionItem AssertionItem
    deriving Eq

instance Show ModuleItem where
    show (MIPackageItem i) = show i
    show (MIAttr attr mi ) = printf "%s %s" (show attr) (show mi)
    show (AlwaysC     k b) = printf "%s %s" (show k) (show b)
    show (Assign    o a b) = printf "assign %s%s = %s;" (showPad o) (show a) (show b)
    show (Defparam    a b) = printf "defparam %s = %s;" (show a) (show b)
    show (Genvar      x  ) = printf "genvar %s;" x
    show (Generate    b  ) = printf "generate\n%s\nendgenerate" (indent $ show b)
    show (Modport     x l) = printf "modport %s(\n%s\n);" x (indent $ intercalate ",\n" $ map showModportDecl l)
    show (Initial     s  ) = printf "initial %s" (show s)
    show (Final       s  ) = printf   "final %s" (show s)
    show (ElabTask    s a) = printf "%s%s;" (show s) (show $ Args a [])
    show (NInputGate  kw d x rs lhs exprs) =
        showGate kw d x rs $ show lhs : map show exprs
    show (NOutputGate kw d x rs lhss expr) =
        showGate kw d x rs $ (map show lhss) ++ [show expr]
    show (AssertionItem i) = show i
    show (Instance   m params i rs ports) =
        if null params
            then printf "%s %s%s%s;"     m                     i rsStr (showPorts ports)
            else printf "%s #%s %s%s%s;" m (showParams params) i rsStr (showPorts ports)
        where rsStr = if null rs then "" else tail $ showRanges rs

showPorts :: [PortBinding] -> String
showPorts ports = indentedParenList $ map showPort ports

showPort :: PortBinding -> String
showPort ("*", Nil) = ".*"
showPort (i, arg) =
    if i == ""
        then show arg
        else printf ".%s(%s)" i (show arg)

showGate :: Show k => k -> Expr -> Identifier -> [Range] -> [String] -> String
showGate kw d x rs args =
    printf "%s %s%s%s(%s);" (show kw) delayStr nameStr rsStr (commas args)
    where
        delayStr = if d == Nil then "" else showPad $ Delay d
        nameStr = showPad $ Ident x
        rsStr = if null rs then "" else tail $ showRanges rs

showModportDecl :: ModportDecl -> String
showModportDecl (dir, ident, e) =
    if e == Ident ident
        then printf "%s %s" (show dir) ident
        else printf "%s .%s(%s)" (show dir) ident (show e)

type PortBinding = (Identifier, Expr)

type ModportDecl = (Direction, Identifier, Expr)

data AlwaysKW
    = Always
    | AlwaysComb
    | AlwaysFF
    | AlwaysLatch
    deriving Eq

instance Show AlwaysKW where
    show Always      = "always"
    show AlwaysComb  = "always_comb"
    show AlwaysFF    = "always_ff"
    show AlwaysLatch = "always_latch"

data NInputGateKW
    = GateAnd
    | GateBufif0
    | GateNand
    | GateOr
    | GateNor
    | GateRpmos
    | GateXor
    | GateXnor
    deriving Eq

instance Show NInputGateKW where
    show GateAnd  = "and"
    show GateBufif0  = "bufif0"
    show GateNand = "nand"
    show GateOr   = "or"
    show GateNor  = "nor"
    show GateRpmos  = "rpmos"
    show GateXor  = "xor"
    show GateXnor = "xnor"

data NOutputGateKW
    = GateBuf
    | GateNot
    deriving Eq

instance Show NOutputGateKW where
    show GateBuf  = "buf"
    show GateNot  = "not"

data AssignOption
    = AssignOptionNone
    | AssignOptionDelay Expr
    | AssignOptionDrive Strength0 Strength1
    deriving Eq

instance Show AssignOption where
    show AssignOptionNone = ""
    show (AssignOptionDelay de) = printf "#(%s)" (show de)
    show (AssignOptionDrive s0 s1) = printf "(%s, %s)" (show s0) (show s1)

data AssertionItem
    = MIAssertion  Identifier Assertion
    | PropertyDecl Identifier PropertySpec
    | SequenceDecl Identifier SeqExpr
    deriving Eq

instance Show AssertionItem where
    show (MIAssertion x a)
        | null x = show a
        | otherwise = printf "%s : %s" x (show a)
    show (PropertyDecl  x p) = printf "property %s;\n%s\nendproperty" x (indent $ show p)
    show (SequenceDecl  x e) = printf "sequence %s;\n%s\nendsequence" x (indent $ show e)
