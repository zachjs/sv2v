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
  , GenItem    (..)
  , AlwaysKW   (..)
  , CaseKW     (..)
  , Decl       (..)
  , AST
  , PortBinding
  , Case
  , Range
  , GenCase
  , typeRanges
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
  | Local
  deriving Eq

instance Show Direction where
  show Input  = "input"
  show Output = "output"
  show Inout  = "inout"
  show Local  = ""

data Type
  = Reg              [Range]
  | Wire             [Range]
  | Logic            [Range]
  | Alias Identifier [Range]
  | Implicit         [Range]
  | IntegerT
  | Enum (Maybe Type) [(Identifier, Maybe Expr)] [Range]
  deriving Eq

instance Show Type where
  show (Reg      r) = "reg"   ++ (showRanges r)
  show (Wire     r) = "wire"  ++ (showRanges r)
  show (Logic    r) = "logic" ++ (showRanges r)
  show (Alias t  r) = t       ++ (showRanges r)
  show (Implicit r) =            (showRanges r)
  show (IntegerT  ) = "integer"
  show (Enum mt vals r) = printf "enum %s{%s}%s" tStr (commas $ map showVal vals) (showRanges r)
    where
      tStr = maybe "" showPad mt
      showVal :: (Identifier, Maybe Expr) -> String
      showVal (x, e) = x ++ (showAssignment e)

typeRanges :: Type -> ([Range] -> Type, [Range])
typeRanges (Reg      r) = (Reg     , r)
typeRanges (Wire     r) = (Wire    , r)
typeRanges (Logic    r) = (Logic   , r)
typeRanges (Alias  t r) = (Alias  t, r)
typeRanges (Implicit r) = (Implicit, r)
typeRanges (IntegerT  ) = (error "ranges cannot be applied to IntegerT", [])
typeRanges (Enum t v r) = (Enum t v, r)

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
  = Comment    String
  | MIDecl     Decl
  | AlwaysC    AlwaysKW Stmt
  | Assign     LHS Expr
  | Instance   Identifier [PortBinding] Identifier (Maybe [PortBinding]) -- `Nothing` represents `.*`
  | Function   Type Identifier [Decl] Stmt
  | Genvar     Identifier
  | Generate   [GenItem]
  deriving Eq

-- "function inputs and outputs are inferred to be of type reg if no internal
-- data types for the ports are declared"

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

instance Show ModuleItem where
  show thing = case thing of
    Comment    c     -> "// " ++ c
    MIDecl     nest  -> show nest
    AlwaysC    k b   -> printf "%s %s" (show k) (show b)
    Assign     a b   -> printf "assign %s = %s;" (show a) (show b)
    Instance   m params i ports
      | null params -> printf "%s %s%s;"     m                    i (showMaybePorts ports)
      | otherwise   -> printf "%s #%s %s%s;" m (showPorts params) i (showMaybePorts ports)
    Function   t x i b -> printf "function %s%s;\n%s\n%s\nendfunction" (showPad t) x (indent $ show i) (indent $ show b)
    Genvar     x -> printf "genvar %s;" x
    Generate   b -> printf "generate\n%s\nendgenerate" (indent $ unlines' $ map show b)
    where
    showMaybePorts = maybe "(.*)" showPorts
    showPorts :: [PortBinding] -> String
    showPorts ports = indentedParenList $ map showPort ports
    showPort :: PortBinding -> String
    showPort (i, arg) =
      if i == ""
        then show (fromJust arg)
        else printf ".%s(%s)" i (if isJust arg then show $ fromJust arg else "")

showAssignment :: Maybe Expr -> String
showAssignment Nothing = ""
showAssignment (Just val) = " = " ++ show val

showRanges :: [Range] -> String
showRanges [] = ""
showRanges l = " " ++ (concat $ map rangeToString l)
  where rangeToString d = init $ showRange $ Just d

showRange :: Maybe Range -> String
showRange Nothing = ""
showRange (Just (h, l)) = printf "[%s:%s] " (show h) (show l)

showPad :: Show t => t -> String
showPad x =
    if str == ""
      then ""
      else str ++ " "
    where str = show x

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
  | Cast       Type Expr
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
    Cast       a b      -> printf "%s'(%s)" (show a) (show b)

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
  = Block   (Maybe (Identifier, [Decl])) [Stmt]
  | Case    CaseKW Expr [Case] (Maybe Stmt)
  | For     (Identifier, Expr) Expr (Identifier, Expr) Stmt
  | AsgnBlk LHS Expr
  | Asgn    LHS Expr
  | If      Expr Stmt Stmt
  | Timing  Sense Stmt
  | Null
  deriving Eq

commas :: [String] -> String
commas = intercalate ", "

instance Show Stmt where
  show (Block header stmts) =
    printf "begin%s\n%s\nend" extra (block stmts)
    where
      block :: Show t => [t] -> String
      block = indent . unlines' . map show
      extra = case header of
        Nothing -> ""
        Just (x, i) -> printf " : %s\n%s" x (block i)
  show (Case  kw e cs def) =
    printf "%s (%s)\n%s%s\nendcase" (show kw) (show e) (indent $ unlines' $ map showCase cs) defStr
    where
    defStr = case def of
      Nothing -> ""
      Just c -> printf "\n\tdefault:\n%s" (indent $ indent $ show c)
  show (For (a,b) c (d,e) f) = printf "for (%s = %s; %s; %s = %s)\n%s" a (show b) (show c) d (show e) $ indent $ show f
  show (AsgnBlk v e) = printf "%s = %s;"  (show v) (show e)
  show (Asgn    v e) = printf "%s <= %s;" (show v) (show e)
  show (If a b Null) = printf "if (%s)\n%s"         (show a) (show b)
  show (If a b c   ) = printf "if (%s) %s\nelse %s" (show a) (show b) (show c)
  show (Timing t s ) = printf "@(%s)%s" (show t) rest
    where
      rest = case s of
        Block _ _ -> " " ++   (show s)
        _ -> "\n" ++ (indent $ show s)
  show (Null       ) = ";"

type Case = ([Expr], Stmt)

showCase :: (Show x, Show y) => ([x], y) -> String
showCase (a, b) = printf "%s: %s" (commas $ map show a) (show b)

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
  show (GenBlock Nothing  i)  = printf "begin\n%s\nend"        (indent $ unlines' $ map show i)
  show (GenBlock (Just x) i)  = printf "begin : %s\n%s\nend" x (indent $ unlines' $ map show i)
  show (GenCase e c Nothing ) = printf "case (%s)\n%s\nendcase"                 (show e) (indent $ unlines' $ map showCase c)
  show (GenCase e c (Just d)) = printf "case (%s)\n%s\n\tdefault:\n%s\nendcase" (show e) (indent $ unlines' $ map showCase c) (indent $ indent $ show d)
  show (GenIf e a GenNull)    = printf "if (%s) %s"          (show e) (show a)
  show (GenIf e a b      )    = printf "if (%s) %s\nelse %s" (show e) (show a) (show b)
  show (GenFor (x1, e1) c (x2, e2) x is) = printf "for (%s = %s; %s; %s = %s) %s" x1 (show e1) (show c) x2 (show e2) (show $ GenBlock (Just x) is)
  show GenNull = ";"
  show (GenModuleItem item) = show item
