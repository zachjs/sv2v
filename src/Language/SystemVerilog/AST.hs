{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - This AST allows for the representation of many syntactically invalid things,
 - like input regs or modport declarations inside a module. Representing only
 - syntactically valid files would make working with the AST a nightmare. We
 - have placed an emphasis on making the conversion procedures in this project
 - more easier to write, interpret, and maintain.
 -
 - In the future, we may want to have a utility which performs some basic
 - invariant checks. I want to avoid making a full type-checker though, as we
 - should only be given valid SystemVerilog input files.
 -}

module Language.SystemVerilog.AST
    ( Description(..)
    , PackageItem(..)
    , ModuleItem (..)
    , Direction  (..)
    , GenItem    (..)
    , AlwaysKW   (..)
    , CaseKW     (..)
    , PartKW     (..)
    , Lifetime   (..)
    , NInputGateKW  (..)
    , NOutputGateKW (..)
    , AST
    , PortBinding
    , ModportDecl
    , GenCase
    , simplify
    , rangeSize
    , module Attr
    , module Decl
    , module Expr
    , module LHS
    , module Op
    , module Stmt
    , module Type
    ) where

import Data.List (intercalate)
import Data.Maybe (maybe, fromJust, isJust)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Language.SystemVerilog.AST.Attr as Attr
import Language.SystemVerilog.AST.Decl as Decl
import Language.SystemVerilog.AST.Expr as Expr
import Language.SystemVerilog.AST.LHS as LHS
import Language.SystemVerilog.AST.Op as Op
import Language.SystemVerilog.AST.Stmt as Stmt
import Language.SystemVerilog.AST.Type as Type

import Language.SystemVerilog.AST.ShowHelp

-- Note: Verilog allows modules to be declared with either a simple list of
-- ports _identifiers_, or a list of port _declarations_. If only the
-- identifiers are used, they must be declared with a type and direction
-- (potentially separately!) within the module itself.

type AST = [Description]

data PackageItem
  = Typedef Type Identifier
  | Function (Maybe Lifetime) Type Identifier [Decl] [Stmt]
  | Task     (Maybe Lifetime)      Identifier [Decl] [Stmt]
  | Comment String
  deriving Eq

instance Show PackageItem where
  show (Typedef t x) = printf "typedef %s %s;" (show t) x
  show (Function ml t x i b) =
    printf "function %s%s%s;\n%s\n%s\nendfunction"
      (showLifetime ml) (showPad t) x (indent $ show i)
      (indent $ unlines' $ map show b)
  show (Task ml x i b) =
    printf "task %s%s;\n%s\n%s\nendtask"
      (showLifetime ml) x (indent $ show i)
      (indent $ unlines' $ map show b)
  show (Comment c) = "// " ++ c

data Description
  = Part Bool PartKW (Maybe Lifetime) Identifier [Identifier] [ModuleItem]
  | PackageItem PackageItem
  | Directive String
  deriving Eq

instance Show Description where
  showList descriptions _ = intercalate "\n" $ map show descriptions
  show (Part True  kw lifetime name _ items) =
    printf "extern %s %s%s %s;"
      (show kw) (showLifetime lifetime) name (indentedParenList itemStrs)
    where itemStrs = map (\(MIDecl a) -> init $ show a) items
  show (Part False kw lifetime name ports items) =
    printf "%s %s%s%s;\n%s\nend%s"
      (show kw) (showLifetime lifetime) name portsStr bodyStr (show kw)
    where
      portsStr =
        if null ports
          then ""
          else " " ++ indentedParenList ports
      bodyStr = indent $ unlines' $ map show items
  show (PackageItem i) = show i
  show (Directive str) = str

data PartKW
  = Module
  | Interface
  deriving Eq

instance Show PartKW where
  show Module    = "module"
  show Interface = "interface"

data ModuleItem
  = MIAttr     Attr ModuleItem
  | MIDecl     Decl
  | AlwaysC    AlwaysKW Stmt
  | Assign     (Maybe Expr) LHS Expr
  | Defparam   LHS Expr
  | Instance   Identifier [PortBinding] Identifier (Maybe Range) [PortBinding]
  | Genvar     Identifier
  | Generate   [GenItem]
  | Modport    Identifier [ModportDecl]
  | Initial    Stmt
  | MIPackageItem PackageItem
  | NInputGate  NInputGateKW  (Maybe Identifier)  LHS [Expr]
  | NOutputGate NOutputGateKW (Maybe Identifier) [LHS] Expr
  | AssertionItem AssertionItem
  deriving Eq

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

type PortBinding = (Identifier, Maybe Expr)
type ModportDecl = (Direction, Identifier, Maybe Expr)

instance Show ModuleItem where
  show thing = case thing of
    MIAttr attr mi -> printf "%s %s" (show attr) (show mi)
    MIDecl     nest  -> show nest
    AlwaysC    k b   -> printf "%s %s" (show k) (show b)
    Assign     d a b -> printf "assign %s%s = %s;" delayStr (show a) (show b)
      where delayStr = maybe "" (\e -> "#(" ++ show e ++ ") ") d
    Defparam   a b   -> printf "defparam %s = %s;" (show a) (show b)
    Instance   m params i r ports
      | null params -> printf "%s %s%s%s;"     m                    i rStr (showPorts ports)
      | otherwise   -> printf "%s #%s %s%s%s;" m (showPorts params) i rStr (showPorts ports)
      where rStr = maybe "" (\a -> showRanges [a] ++ " ") r
    Genvar     x -> printf "genvar %s;" x
    Generate   b -> printf "generate\n%s\nendgenerate" (indent $ unlines' $ map show b)
    Modport    x l   -> printf "modport %s(\n%s\n);" x (indent $ intercalate ",\n" $ map showModportDecl l)
    Initial    s     -> printf "initial %s" (show s)
    MIPackageItem i  -> show i
    NInputGate  kw x lhs  exprs -> printf "%s%s (%s, %s);" (show kw) (maybe "" (" " ++) x) (show lhs) (commas $ map show exprs)
    NOutputGate kw x lhss expr  -> printf "%s%s (%s, %s);" (show kw) (maybe "" (" " ++) x) (commas $ map show lhss) (show expr)
    AssertionItem (Nothing, a) -> show a
    AssertionItem (Just x, a) -> printf "%s : %s" x (show a)
    where
    showPorts :: [PortBinding] -> String
    showPorts ports = indentedParenList $ map showPort ports
    showPort :: PortBinding -> String
    showPort ("*", Nothing) = ".*"
    showPort (i, arg) =
      if i == ""
        then show (fromJust arg)
        else printf ".%s(%s)" i (if isJust arg then show $ fromJust arg else "")
    showModportDecl :: ModportDecl -> String
    showModportDecl (dir, ident, me) =
      if me == Just (Ident ident)
        then printf "%s %s" (show dir) ident
        else printf "%s .%s(%s)" (show dir) ident (maybe "" show me)

data NInputGateKW
  = GateAnd
  | GateNand
  | GateOr
  | GateNor
  | GateXor
  | GateXnor
  deriving Eq
data NOutputGateKW
  = GateBuf
  | GateNot
  deriving Eq

instance Show NInputGateKW where
  show GateAnd  = "and"
  show GateNand = "nand"
  show GateOr   = "or"
  show GateNor  = "nor"
  show GateXor  = "xor"
  show GateXnor = "xnor"
instance Show NOutputGateKW where
  show GateBuf  = "buf"
  show GateNot  = "not"

type GenCase = ([Expr], GenItem)

data GenItem
  = GenBlock (Maybe Identifier) [GenItem]
  | GenCase  Expr [GenCase] (Maybe GenItem)
  | GenFor   (Identifier, Expr) Expr (Identifier, AsgnOp, Expr) (Maybe Identifier) [GenItem]
  | GenIf    Expr GenItem GenItem
  | GenNull
  | GenModuleItem ModuleItem
  deriving Eq

instance Show GenItem where
  showList i _ = unlines' $ map show i
  show (GenBlock Nothing  i)  = printf "begin\n%s\nend"        (indent $ unlines' $ map show i)
  show (GenBlock (Just x) i)  = printf "begin : %s\n%s\nend" x (indent $ unlines' $ map show i)
  show (GenCase e c Nothing ) = printf "case (%s)\n%s\nendcase"                 (show e) (indent $ unlines' $ map showCase c)
  show (GenCase e c (Just d)) = printf "case (%s)\n%s\n\tdefault:\n%s\nendcase" (show e) (indent $ unlines' $ map showCase c) (indent $ indent $ show d)
  show (GenIf e a GenNull)    = printf "if (%s) %s"          (show e) (show a)
  show (GenIf e a b      )    = printf "if (%s) %s\nelse %s" (show e) (show a) (show b)
  show (GenFor (x1, e1) c (x2, o2, e2) mx is) = printf "for (%s = %s; %s; %s %s %s) %s" x1 (show e1) (show c) x2 (show o2) (show e2) (show $ GenBlock mx is)
  show GenNull = ";"
  show (GenModuleItem item) = show item

data Lifetime
  = Static
  | Automatic
  deriving (Eq, Ord)

instance Show Lifetime where
  show Static    = "static"
  show Automatic = "automatic"

showLifetime :: Maybe Lifetime -> String
showLifetime Nothing = ""
showLifetime (Just l) = show l ++ " "

-- basic expression simplfication utility to help us generate nicer code in the
-- common case of ranges like `[FOO-1:0]`
simplify :: Expr -> Expr
simplify (BinOp op e1 e2) =
    case (op, e1', e2') of
        (Add, Number "0", e) -> e
        (Add, e, Number "0") -> e
        (Sub, e, Number "0") -> e
        (Add, BinOp Sub e (Number "1"), Number "1") -> e
        (Add, e, BinOp Sub (Number "0") (Number "1")) -> BinOp Sub e (Number "1")
        (_  , Number a, Number b) ->
            case (op, readMaybe a :: Maybe Int, readMaybe b :: Maybe Int) of
                (Add, Just x, Just y) -> Number $ show (x + y)
                (Sub, Just x, Just y) -> Number $ show (x - y)
                (Mul, Just x, Just y) -> Number $ show (x * y)
                _ -> BinOp op e1' e2'
        _ -> BinOp op e1' e2'
    where
        e1' = simplify e1
        e2' = simplify e2
simplify other = other

rangeSize :: Range -> Expr
rangeSize (s, e) =
    simplify $ BinOp Add (BinOp Sub s e) (Number "1")
