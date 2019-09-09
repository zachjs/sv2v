{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog `module` items
 -}

module Language.SystemVerilog.AST.ModuleItem
    ( ModuleItem    (..)
    , PortBinding
    , ParamBinding
    , ModportDecl
    , AlwaysKW      (..)
    , NInputGateKW  (..)
    , NOutputGateKW (..)
    ) where

import Data.List (intercalate)
import Data.Maybe (maybe, fromJust, isJust)
import Data.Either (either)
import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp

import Language.SystemVerilog.AST.Attr (Attr)
import Language.SystemVerilog.AST.Decl (Direction)
import Language.SystemVerilog.AST.Description (PackageItem)
import Language.SystemVerilog.AST.Expr (Expr(Ident, Nil), Range, TypeOrExpr, showRanges)
import Language.SystemVerilog.AST.GenItem (GenItem)
import Language.SystemVerilog.AST.LHS (LHS)
import Language.SystemVerilog.AST.Stmt (Stmt, AssertionItem)
import Language.SystemVerilog.AST.Type (Identifier)

data ModuleItem
    = MIAttr     Attr ModuleItem
    | AlwaysC    AlwaysKW Stmt
    | Assign     (Maybe Expr) LHS Expr
    | Defparam   LHS Expr
    | Instance   Identifier [ParamBinding] Identifier (Maybe Range) [PortBinding]
    | Genvar     Identifier
    | Generate   [GenItem]
    | Modport    Identifier [ModportDecl]
    | Initial    Stmt
    | MIPackageItem PackageItem
    | NInputGate  NInputGateKW  (Maybe Identifier)  LHS [Expr]
    | NOutputGate NOutputGateKW (Maybe Identifier) [LHS] Expr
    | AssertionItem AssertionItem
    deriving Eq

instance Show ModuleItem where
    show (MIPackageItem i) = show i
    show (MIAttr attr mi ) = printf "%s %s" (show attr) (show mi)
    show (AlwaysC     k b) = printf "%s %s" (show k) (show b)
    show (Defparam    a b) = printf "defparam %s = %s;" (show a) (show b)
    show (Genvar      x  ) = printf "genvar %s;" x
    show (Generate    b  ) = printf "generate\n%s\nendgenerate" (indent $ unlines' $ map show b)
    show (Modport     x l) = printf "modport %s(\n%s\n);" x (indent $ intercalate ",\n" $ map showModportDecl l)
    show (Initial     s  ) = printf "initial %s" (show s)
    show (NInputGate  kw x lhs exprs) = printf "%s%s (%s, %s);" (show kw) (maybe "" (" " ++) x) (show lhs) (commas $ map show exprs)
    show (NOutputGate kw x lhss expr) = printf "%s%s (%s, %s);" (show kw) (maybe "" (" " ++) x) (commas $ map show lhss) (show expr)
    show (Assign d a b) =
        printf "assign %s%s = %s;" delayStr (show a) (show b)
        where delayStr = maybe "" (\e -> "#(" ++ show e ++ ") ") d
    show (AssertionItem (mx, a)) =
        if mx == Nothing
            then show a
            else printf "%s : %s" (fromJust mx) (show a)
    show (Instance   m params i r ports) =
        if null params
            then printf "%s %s%s%s;"     m                     i rStr (showPorts ports)
            else printf "%s #%s %s%s%s;" m (showParams params) i rStr (showPorts ports)
        where rStr = maybe "" (\a -> showRanges [a] ++ " ") r

showPorts :: [PortBinding] -> String
showPorts ports = indentedParenList $ map showPort ports

showPort :: PortBinding -> String
showPort ("*", Nothing) = ".*"
showPort (i, arg) =
    if i == ""
        then show (fromJust arg)
        else printf ".%s(%s)" i (if isJust arg then show $ fromJust arg else "")

showParams :: [ParamBinding] -> String
showParams params = indentedParenList $ map showParam params

showParam :: ParamBinding -> String
showParam ("*", Right Nil) = ".*"
showParam (i, arg) =
    printf fmt i (either show show arg)
    where fmt = if i == "" then "%s%s" else ".%s(%s)"

showModportDecl :: ModportDecl -> String
showModportDecl (dir, ident, me) =
    if me == Just (Ident ident)
        then printf "%s %s" (show dir) ident
        else printf "%s .%s(%s)" (show dir) ident (maybe "" show me)

type PortBinding = (Identifier, Maybe Expr)

type ParamBinding = (Identifier, TypeOrExpr)

type ModportDecl = (Direction, Identifier, Maybe Expr)

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
    | GateNand
    | GateOr
    | GateNor
    | GateXor
    | GateXnor
    deriving Eq

instance Show NInputGateKW where
    show GateAnd  = "and"
    show GateNand = "nand"
    show GateOr   = "or"
    show GateNor  = "nor"
    show GateXor  = "xor"
    show GateXnor = "xnor"

data NOutputGateKW
    = GateBuf
    | GateNot
    deriving Eq

instance Show NOutputGateKW where
    show GateBuf  = "buf"
    show GateNot  = "not"
