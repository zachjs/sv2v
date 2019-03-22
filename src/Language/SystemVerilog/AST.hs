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
    , Stmt       (..)
    , LHS        (..)
    , Expr       (..)
    , Sense      (..)
    , Timing     (..)
    , GenItem    (..)
    , AlwaysKW   (..)
    , CaseKW     (..)
    , PartKW     (..)
    , Decl       (..)
    , Lifetime   (..)
    , NInputGateKW  (..)
    , NOutputGateKW (..)
    , AST
    , PortBinding
    , ModportDecl
    , Case
    , GenCase
    , simplify
    , rangeSize
    , module Expr
    , module Op
    , module Type
    ) where

import Data.List (intercalate)
import Data.Maybe (maybe, fromJust, isJust)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Language.SystemVerilog.AST.Expr as Expr
import Language.SystemVerilog.AST.Op as Op
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
  = Part PartKW Identifier [Identifier] [ModuleItem]
  | PackageItem PackageItem
  | Directive String
  deriving Eq

instance Show Description where
  showList descriptions _ = intercalate "\n" $ map show descriptions
  show (Part kw name ports items) = unlines
    [ (show kw) ++ " " ++ name ++ portsStr ++ ";"
    , indent $ unlines' $ map show items
    , "end" ++ (show kw) ]
    where
      portsStr =
        if null ports
          then ""
          else indentedParenList ports
  show (PackageItem i) = show i
  show (Directive str) = str

data PartKW
  = Module
  | Interface
  deriving Eq

instance Show PartKW where
  show Module    = "module"
  show Interface = "interface"

data Direction
  = Input
  | Output
  | Inout
  | Local
  deriving Eq

instance Show Direction where
  show Input  = "input"
  show Output = "output"
  show Inout  = "inout"
  show Local  = ""

data Decl
  = Parameter            Type Identifier Expr
  | Localparam           Type Identifier Expr
  | Variable   Direction Type Identifier [Range] (Maybe Expr)
  deriving Eq

instance Show Decl where
  showList l _ = unlines' $ map show l
  show (Parameter  t x e) = printf  "parameter %s%s = %s;" (showPad t) x (show e)
  show (Localparam t x e) = printf "localparam %s%s = %s;" (showPad t) x (show e)
  show (Variable d t x a me) = printf "%s%s %s%s%s;" (showPad d) (show t) x (showRanges a) (showAssignment me)

data ModuleItem
  = MIDecl     Decl
  | AlwaysC    AlwaysKW Stmt
  | Assign     LHS Expr
  | Defparam   LHS Expr
  | Instance   Identifier [PortBinding] Identifier [PortBinding]
  | Genvar     Identifier
  | Generate   [GenItem]
  | Modport    Identifier [ModportDecl]
  | Initial    Stmt
  | MIPackageItem PackageItem
  | NInputGate  NInputGateKW  (Maybe Identifier)  LHS [Expr]
  | NOutputGate NOutputGateKW (Maybe Identifier) [LHS] Expr
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
    MIDecl     nest  -> show nest
    AlwaysC    k b   -> printf "%s %s" (show k) (show b)
    Assign     a b   -> printf "assign %s = %s;" (show a) (show b)
    Defparam   a b   -> printf "defparam %s = %s;" (show a) (show b)
    Instance   m params i ports
      | null params -> printf "%s %s%s;"     m                    i (showPorts ports)
      | otherwise   -> printf "%s #%s %s%s;" m (showPorts params) i (showPorts ports)
    Genvar     x -> printf "genvar %s;" x
    Generate   b -> printf "generate\n%s\nendgenerate" (indent $ unlines' $ map show b)
    Modport    x l   -> printf "modport %s(\n%s\n);" x (indent $ intercalate ",\n" $ map showModportDecl l)
    Initial    s     -> printf "initial %s" (show s)
    MIPackageItem i  -> show i
    NInputGate  kw x lhs  exprs -> printf "%s%s (%s, %s);" (show kw) (maybe "" (" " ++) x) (show lhs) (commas $ map show exprs)
    NOutputGate kw x lhss expr  -> printf "%s%s (%s, %s);" (show kw) (maybe "" (" " ++) x) (commas $ map show lhss) (show expr)
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

data LHS
  = LHSIdent  Identifier
  | LHSBit    LHS Expr
  | LHSRange  LHS Range
  | LHSDot    LHS Identifier
  | LHSConcat [LHS]
  deriving Eq

instance Show LHS where
  show (LHSIdent   x       ) = x
  show (LHSBit     l e     ) = printf "%s[%s]"    (show l) (show e)
  show (LHSRange   l (a, b)) = printf "%s[%s:%s]" (show l) (show a) (show b)
  show (LHSDot     l x     ) = printf "%s.%s"     (show l) x
  show (LHSConcat  lhss    ) = printf "{%s}" (commas $ map show lhss)

data CaseKW
  = CaseN
  | CaseZ
  | CaseX
  deriving Eq

instance Show CaseKW where
  show CaseN = "case"
  show CaseZ = "casez"
  show CaseX = "casex"

data Stmt
  = Block   (Maybe Identifier) [Decl] [Stmt]
  | Case    Bool CaseKW Expr [Case] (Maybe Stmt)
  | For     (Identifier, Expr) Expr (Identifier, Expr) Stmt
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
  | Null
  deriving Eq

instance Show Stmt where
  show (Block name decls stmts) =
    printf "begin%s\n%s\n%s\nend" header (block decls) (block stmts)
    where
      header = maybe "" (" : " ++) name
      block :: Show t => [t] -> String
      block = indent . unlines' . map show
  show (Case u kw e cs def) =
    printf "%s%s (%s)\n%s%s\nendcase" uniqStr (show kw) (show e) (indent $ unlines' $ map showCase cs) defStr
    where
      uniqStr = if u then "unique " else ""
      defStr = case def of
        Nothing -> ""
        Just c -> printf "\n\tdefault: %s" (show c)
  show (For (a,b) c (d,e) f) = printf "for (%s = %s; %s; %s = %s)\n%s" a (show b) (show c) d (show e) $ indent $ show f
  show (AsgnBlk o v e) = printf "%s %s %s;" (show v) (show o) (show e)
  show (Asgn    t v e) = printf "%s <= %s%s;" (show v) (maybe "" showPad t) (show e)
  show (While   e s) = printf  "while (%s) %s" (show e) (show s)
  show (RepeatL e s) = printf "repeat (%s) %s" (show e) (show s)
  show (DoWhile e s) = printf "do %s while (%s);" (show s) (show e)
  show (Forever s  ) = printf "forever %s" (show s)
  show (If a b Null) = printf "if (%s) %s"         (show a) (show b)
  show (If a b c   ) = printf "if (%s) %s\nelse %s" (show a) (show b) (show c)
  show (Return e   ) = printf "return %s;" (show e)
  show (Subroutine x a) = printf "%s(%s);" x (commas $ map (maybe "" show) a)
  show (Timing t s ) = printf "%s%s" (show t) rest
    where
      rest = case s of
        Null -> ";"
        Block _ _ _ -> " " ++   (show s)
        _ -> "\n" ++ (indent $ show s)
  show (Null       ) = ";"

type Case = ([Expr], Stmt)

showCase :: (Show x, Show y) => ([x], y) -> String
showCase (a, b) = printf "%s: %s" (commas $ map show a) (show b)

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
