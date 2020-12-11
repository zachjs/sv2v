{-# LANGUAGE PatternSynonyms #-}
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
    ) where

import Data.List (intercalate)
import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp

import Language.SystemVerilog.AST.Attr (Attr)
import Language.SystemVerilog.AST.Decl (Direction)
import Language.SystemVerilog.AST.Description (PackageItem)
import Language.SystemVerilog.AST.Expr (Expr(Nil), pattern Ident, Range, showRanges, ParamBinding, showParams)
import Language.SystemVerilog.AST.GenItem (GenItem)
import Language.SystemVerilog.AST.LHS (LHS)
import Language.SystemVerilog.AST.Stmt (Stmt, AssertionItem, Timing(Delay))
import Language.SystemVerilog.AST.Type (Identifier, DriveStrength)

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
    | MIPackageItem PackageItem
    | NInputGate  NInputGateKW  Expr Identifier  LHS [Expr]
    | NOutputGate NOutputGateKW Expr Identifier [LHS] Expr
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
    show (NInputGate  kw d x lhs exprs) =
        showGate kw d x $ show lhs : map show exprs
    show (NOutputGate kw d x lhss expr) =
        showGate kw d x $ (map show lhss) ++ [show expr]
    show (AssertionItem (x, a)) =
        if null x
            then show a
            else printf "%s : %s" x (show a)
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

showGate :: Show k => k -> Expr -> Identifier -> [String] -> String
showGate kw d x args =
    printf "%s %s%s(%s);" (show kw) delayStr nameStr (commas args)
    where
        delayStr = if d == Nil then "" else showPad $ Delay d
        nameStr = showPad $ Ident x

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

data AssignOption
    = AssignOptionNone
    | AssignOptionDelay  Expr
    | AssignOptionDrive  DriveStrength
    deriving Eq

instance Show AssignOption where
    show AssignOptionNone = ""
    show (AssignOptionDelay de) = printf "#(%s)" (show de)
    show (AssignOptionDrive ds) = show ds
