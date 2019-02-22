module Language.SystemVerilog.AST
  ( Identifier
  , Description(..)
  , ModuleItem (..)
  , Direction  (..)
  , Type       (..)
  , Stmt       (..)
  , LHS        (..)
  , Expr       (..)
  , UniOp      (..)
  , BinOp      (..)
  , Sense      (..)
  , BlockItemDeclaration (..)
  , Parameter  (..)
  , Localparam (..)
  , IntegerV   (..)
  , GenItem    (..)
  , AlwaysKW (..)
  , AST
  , PortBinding
  , Case
  , Range
  , GenCase
  , RangesOrAssignment
  ) where

import Data.List
import Data.Maybe
import Text.Printf

type Identifier = String

-- Note: Verilog allows modules to be declared with either a simple list of
-- ports _identifiers_, or a list of port _declarations_. If only the
-- identifiers are used, they must be declared with a type and direction
-- (potentially separately!) within the module itself.

-- Note: This AST will allow for the representation of syntactically invalid
-- things, like input regs. We might want to have a function for doing some
-- basing invariant checks. I want to avoid making a full type-checker though,
-- as we should only be given valid SystemVerilog input files.

type AST = [Description]

data Description
  = Module Identifier [Identifier] [ModuleItem]
  | Typedef Type Identifier
  deriving Eq

instance Show Description where
  showList descriptions _ = intercalate "\n" $ map show descriptions
  show (Module name ports items) = unlines
    [ "module " ++ name ++ portsStr ++ ";"
    , indent $ unlines' $ map show items
    , "endmodule" ]
    where
      portsStr =
        if null ports
          then ""
          else indentedParenList ports
  show (Typedef t x) = printf "typedef %s %s;" (show t) x

data Direction
  = Input
  | Output
  | Inout
  deriving Eq

instance Show Direction where
  show Input  = "input"
  show Output = "output"
  show Inout  = "inout"

data Type
  = Reg   [Range]
  | Wire  [Range]
  | Logic [Range]
  | Alias String [Range]
  deriving Eq

instance Show Type where
  show (Reg     r) = "reg "   ++ (showRanges r)
  show (Wire    r) = "wire "  ++ (showRanges r)
  show (Logic   r) = "logic " ++ (showRanges r)
  show (Alias t r) = t ++ " " ++ (showRanges r)

data ModuleItem
  = Comment    String
  | MIParameter  Parameter
  | MILocalparam Localparam
  | MIIntegerV   IntegerV
  | PortDecl   Direction [Range] Identifier
  | LocalNet   Type Identifier RangesOrAssignment
  | AlwaysC    AlwaysKW Stmt
  | Assign     LHS Expr
  | Instance   Identifier [PortBinding] Identifier (Maybe [PortBinding]) -- `Nothing` represents `.*`
  | Function   (Maybe FuncRet) Identifier [(Bool, BlockItemDeclaration)] Stmt
  | Genvar     Identifier
  | Generate   [GenItem]
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

-- "function inputs and outputs are inferred to be of type reg if no internal
-- data types for the ports are declared"

type PortBinding = (Identifier, Maybe Expr)

data Parameter  = Parameter  (Maybe Range) Identifier Expr deriving Eq
instance Show Parameter where
    show (Parameter  r n e) = printf "parameter %s%s = %s;"  (showRange r) n (show e)

data Localparam = Localparam (Maybe Range) Identifier Expr deriving Eq
instance Show Localparam where
    show (Localparam r n e) = printf "localparam %s%s = %s;" (showRange r) n (show e)

data IntegerV   = IntegerV   Identifier RangesOrAssignment deriving Eq
instance Show IntegerV where
    show (IntegerV   x v  ) = printf "integer %s%s;" x (showRangesOrAssignment v)

instance Show ModuleItem where
  show thing = case thing of
    Comment    c     -> "// " ++ c
    MIParameter  nest -> show nest
    MILocalparam nest -> show nest
    MIIntegerV   nest -> show nest
    PortDecl   d r x -> printf "%s %s%s;" (show d) (showRanges r) x
    LocalNet   t x v -> printf "%s%s%s;" (show t) x (showRangesOrAssignment v)
    AlwaysC    k b   -> printf "%s %s" (show k) (show b)
    Assign     a b   -> printf "assign %s = %s;" (show a) (show b)
    Instance   m params i ports
      | null params -> printf "%s %s%s;"     m                    i (showMaybePorts ports)
      | otherwise   -> printf "%s #%s %s%s;" m (showPorts params) i (showMaybePorts ports)
    Function   t x i b -> printf "function %s%s;\n%s\n%s\nendfunction" (showFuncRet t) x (indent $ unlines' $ map showFunctionItem i) (indent $ show b)
    Genvar     x -> printf "genvar %s;" x
    Generate   b -> printf "generate\n%s\nendgenerate" (indent $ unlines' $ map show b)
    where
    showMaybePorts :: Maybe [(Identifier, Maybe Expr)] -> String
    showMaybePorts Nothing = "(.*)"
    showMaybePorts (Just ports) = showPorts ports
    showPorts :: [(Identifier, Maybe Expr)] -> String
    showPorts ports = indentedParenList [ if i == "" then show (fromJust arg) else printf ".%s(%s)" i (if isJust arg then show $ fromJust arg else "") | (i, arg) <- ports ]
    showFunctionItem :: (Bool, BlockItemDeclaration) -> String
    showFunctionItem (b, item) = prefix ++ (show item)
      where prefix = if b then "input " else ""

type FuncRet = Either Range ()

showFuncRet :: Maybe FuncRet -> String
showFuncRet Nothing = ""
showFuncRet (Just (Left r)) = showRange $ Just r
showFuncRet (Just (Right ())) = "integer "

type RangesOrAssignment = Either [Range] (Maybe Expr)

showRangesOrAssignment :: Either [Range] (Maybe Expr) -> String
showRangesOrAssignment (Left ranges) = showRanges ranges
showRangesOrAssignment (Right val) = showAssignment val

showAssignment :: Maybe Expr -> String
showAssignment Nothing = ""
showAssignment (Just val) = " = " ++ show val

showRanges :: [Range] -> String
showRanges = concat . (map rangeToString)
  where rangeToString d = (showRange $ Just d) ++ "\b"

showRange :: Maybe Range -> String
showRange Nothing = ""
showRange (Just (h, l)) = printf "[%s:%s] " (show h) (show l)

indent :: String -> String
indent a = '\t' : f a
  where
  f [] = []
  f (x : xs)
    | x == '\n' = "\n\t" ++ f xs
    | otherwise = x : f xs

unlines' :: [String] -> String
unlines' = intercalate "\n"

data Expr
  = String     String
  | Number     String
  | ConstBool  Bool
  | Ident      Identifier
  | IdentRange Identifier Range
  | IdentBit   Identifier Expr
  | Repeat     Expr [Expr]
  | Concat     [Expr]
  | Call       Identifier [Expr]
  | UniOp      UniOp Expr
  | BinOp      BinOp Expr Expr
  | Mux        Expr Expr Expr
  | Bit        Expr Int
  deriving Eq

data UniOp
  = Not
  | BWNot
  | UAdd
  | USub
  | RedAnd
  | RedNand
  | RedOr
  | RedNor
  | RedXor
  | RedXnor
  deriving Eq

instance Show UniOp where
  show Not     = "!"
  show BWNot   = "~"
  show UAdd    = "+"
  show USub    = "-"
  show RedAnd  = "&"
  show RedNand = "~&"
  show RedOr   = "|"
  show RedNor  = "~|"
  show RedXor  = "^"
  show RedXnor = "~^"

data BinOp
  = And
  | Or
  | BWAnd
  | BWXor
  | BWOr
  | Mul
  | Div
  | Mod
  | Add
  | Sub
  | ShiftL
  | ShiftR
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | ShiftAL
  | ShiftAR
  deriving Eq

instance Show BinOp where
  show a = case a of
    And    -> "&&"
    Or     -> "||"
    BWAnd  -> "&"
    BWXor  -> "^"
    BWOr   -> "|"
    Mul    -> "*"
    Div    -> "/"
    Mod    -> "%"
    Add    -> "+"
    Sub    -> "-"
    ShiftL -> "<<"
    ShiftR -> ">>"
    Eq     -> "=="
    Ne     -> "!="
    Lt     -> "<"
    Le     -> "<="
    Gt     -> ">"
    Ge     -> ">="
    ShiftAL -> "<<<"
    ShiftAR -> ">>>"

instance Show Expr where
  show x = case x of
    String     a        -> printf "\"%s\"" a
    Number     a        -> a
    ConstBool  a        -> printf "1'b%s" (if a then "1" else "0")
    Ident      a        -> a
    IdentBit   a b      -> printf "%s[%s]"    a (show b)
    IdentRange a (b, c) -> printf "%s[%s:%s]" a (show b) (show c)
    Repeat     a b      -> printf "{%s {%s}}" (show a) (commas $ map show b)
    Concat     a        -> printf "{%s}" (commas $ map show a)
    Call       a b      -> printf "%s(%s)" a (commas $ map show b)
    UniOp      a b      -> printf "(%s %s)" (show a) (show b)
    BinOp      a b c    -> printf "(%s %s %s)" (show b) (show a) (show c)
    Mux        a b c    -> printf "(%s ? %s : %s)" (show a) (show b) (show c)
    Bit        a b      -> printf "(%s [%d])" (show a) b

data LHS
  = LHS       Identifier
  | LHSBit    Identifier Expr
  | LHSRange  Identifier Range
  | LHSConcat [LHS]
  deriving Eq

instance Show LHS where
  show (LHS        a       ) = a
  show (LHSBit     a b     ) = printf "%s[%s]"    a (show b)
  show (LHSRange   a (b, c)) = printf "%s[%s:%s]" a (show b) (show c)
  show (LHSConcat  a       ) = printf "{%s}" (commas $ map show a)

data Stmt
  = Block                 (Maybe (Identifier, [BlockItemDeclaration])) [Stmt]
  | Case                  Expr [Case] (Maybe Stmt)
  | BlockingAssignment    LHS Expr
  | NonBlockingAssignment LHS Expr
  | For                   (Identifier, Expr) Expr (Identifier, Expr) Stmt
  | If                    Expr Stmt Stmt
  | Timing                Sense Stmt
  | Null
  deriving Eq

commas :: [String] -> String
commas = intercalate ", "

instance Show Stmt where
  show (Block                 Nothing       b  ) = printf "begin\n%s\nend" $ indent $ unlines' $ map show b
  show (Block                 (Just (a, i)) b  ) = printf "begin : %s\n%s\nend" a $ indent $ unlines' $ (map show i ++ map show b)
  show (Case                  a b Nothing      ) = printf "case (%s)\n%s\nendcase"                 (show a) (indent $ unlines' $ map showCase b)
  show (Case                  a b (Just c)     ) = printf "case (%s)\n%s\n\tdefault:\n%s\nendcase" (show a) (indent $ unlines' $ map showCase b) (indent $ indent $ show c)
  show (BlockingAssignment    a b              ) = printf "%s = %s;" (show a) (show b)
  show (NonBlockingAssignment a b              ) = printf "%s <= %s;" (show a) (show b)
  show (For                   (a, b) c (d, e) f) = printf "for (%s = %s; %s; %s = %s)\n%s" a (show b) (show c) d (show e) $ indent $ show f
  show (If                    a b Null         ) = printf "if (%s)\n%s"           (show a) (indent $ show b)
  show (If                    a b c            ) = printf "if (%s)\n%s\nelse\n%s" (show a) (indent $ show b) (indent $ show c)
  show (Timing                t s              ) = printf "@(%s) %s" (show t) (show s)
  show (Null                                   ) = ";"

data BlockItemDeclaration
  -- TODO: Maybe BIDReg should use [Range] for the first arg as well, but it's
  -- really not clear to me what *useful* purpose this would have.
  = BIDReg        (Maybe Range) Identifier [Range]
  | BIDParameter  Parameter
  | BIDLocalparam Localparam
  | BIDIntegerV   IntegerV
  deriving Eq

instance Show BlockItemDeclaration where
  show (BIDReg     mr x rs) = printf "reg %s%s%s;" (showRange mr) x (showRanges rs)
  show (BIDParameter  nest) = show nest
  show (BIDLocalparam nest) = show nest
  show (BIDIntegerV   nest) = show nest

type Case = ([Expr], Stmt)

showCase :: (Show x, Show y) => ([x], y) -> String
showCase (a, b) = printf "%s:\n%s" (commas $ map show a) (indent $ show b)

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

type Range = (Expr, Expr)

indentedParenList :: [String] -> String
indentedParenList [] = "()"
indentedParenList [x] = "(" ++ x ++ ")"
indentedParenList l =
  "(\n" ++ (indent $ intercalate ",\n" l) ++ "\n)"

type GenCase = ([Expr], GenItem)

data GenItem
  = GenBlock (Maybe Identifier) [GenItem]
  | GenCase  Expr [GenCase] (Maybe GenItem)
  | GenFor   (Identifier, Expr) Expr (Identifier, Expr) Identifier [GenItem]
  | GenIf    Expr GenItem GenItem
  | GenNull
  | GenModuleItem ModuleItem
  deriving Eq

instance Show GenItem where
  showList i _ = unlines' $ map show i
  show (GenBlock Nothing  i)  = printf "begin\n%s\nend"          (indent $ unlines' $ map show i)
  show (GenBlock (Just x) i)  = printf "begin : %s\n%s\nend" x (indent $ unlines' $ map show i)
  show (GenCase e c Nothing ) = printf "case (%s)\n%s\nendcase"                 (show e) (indent $ unlines' $ map showCase c)
  show (GenCase e c (Just d)) = printf "case (%s)\n%s\n\tdefault:\n%s\nendcase" (show e) (indent $ unlines' $ map showCase c) (indent $ indent $ show d)
  show (GenIf e a GenNull)    = printf "if (%s)\n%s"           (show e) (indent $ show a)
  show (GenIf e a b      )    = printf "if (%s)\n%s\nelse\n%s" (show e) (indent $ show a) (indent $ show b)
  show (GenFor (x1, e1) c (x2, e2) x is) = printf "for (%s = %s; %s; %s = %s) %s" x1 (show e1) (show c) x2 (show e2) (show $ GenBlock (Just x) is)
  show GenNull = ";"
  show (GenModuleItem item) = show item
