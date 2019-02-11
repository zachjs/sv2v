module Language.SystemVerilog.AST
  ( Identifier
  , Module     (..)
  , ModuleItem (..)
  , Direction  (..)
  , Type       (..)
  , Stmt       (..)
  , LHS        (..)
  , Expr       (..)
  , UniOp      (..)
  , BinOp      (..)
  , Sense      (..)
  , Call       (..)
  , PortBinding
  , Case
  , Range
  ) where

import Data.Bits
import Data.List
import Data.Maybe
import Text.Printf

import Data.BitVec

type Identifier = String

-- Note: Verilog allows modules to be declared with either a simple list of
-- ports _identifiers_, or a list of port _declarations_. If only the
-- identifiers are used, they must be declared with a type and direction
-- (potentially separately!) within the module itself.

-- Note: This AST will allow for the representation of syntactically invalid
-- things, like input regs. We might want to have a function for doing some
-- basing invariant checks. I want to avoid making a full type-checker though,
-- as we should only be given valid SystemVerilog input files.

data Module
  = Module Identifier [Identifier] [ModuleItem]
  deriving Eq

instance Show Module where
  showList modules _ = intercalate "\n" $ map show modules
  show (Module name ports items) = unlines
    [ "module " ++ name ++ portsStr ++ ";"
    , indent $ unlines' $ map show items
    , "endmodule" ]
    where
      portsStr =
        if null ports
          then ""
          else indentedParenList ports

data Direction
  = Input
  | Output
  | Inout
  deriving Eq

instance Show Direction where
  show Input  = "input"
  show Output = "output"
  show Inout  = "inout"

-- TODO: Support for arrays (multi-dimensional, too!)
data Type
  = Reg  (Maybe Range)
  | Wire (Maybe Range)
  deriving Eq

instance Show Type where
  show (Reg  r) = "reg " ++ (showRange r)
  show (Wire r) = "wire " ++ (showRange r)

data ModuleItem
  = Comment    String
  | Parameter  (Maybe Range) Identifier Expr
  | Localparam (Maybe Range) Identifier Expr
  | PortDecl   Direction (Maybe Range) Identifier
  | LocalNet   Type Identifier (Either [Range] (Maybe Expr))
  | Integer    [Identifier]
  | Always     (Maybe Sense) Stmt
  | Assign     LHS Expr
  | Instance   Identifier [PortBinding] Identifier [PortBinding]
  deriving Eq

type PortBinding = (Identifier, Maybe Expr)

instance Show ModuleItem where
  show thing = case thing of
    Comment    c     -> "// " ++ c
    Parameter  r n e -> printf "parameter %s%s = %s;"  (showRange r) n (showExprConst e)
    Localparam r n e -> printf "localparam %s%s = %s;" (showRange r) n (showExprConst e)
    PortDecl   d r x -> printf "%s %s%s;" (show d) (showRange r) x
    LocalNet   t x v -> printf "%s%s%s;" (show t) x extra
      where
        extra =
          case v of
            Left ranges -> (intercalate "\b" $ map (showRange . Just) ranges) ++ "\b"
            Right Nothing -> ""
            Right (Just val) -> " = " ++ show val
    Integer      a   -> printf "integer %s;"  $ commas a
    Always     Nothing  b -> printf "always\n%s" $ indent $ show b
    Always     (Just a) b -> printf "always @(%s)\n%s" (show a) $ indent $ show b
    Assign     a b   -> printf "assign %s = %s;" (show a) (show b)
    Instance   m params i ports
      | null params -> printf "%s %s %s;"     m                                  i (showPorts show ports)
      | otherwise   -> printf "%s #%s %s %s;" m (showPorts showExprConst params) i (showPorts show ports)
    where
    showPorts :: (Expr -> String) -> [(Identifier, Maybe Expr)] -> String
    showPorts s ports = indentedParenList [ if i == "" then show (fromJust arg) else printf ".%s(%s)" i (if isJust arg then s $ fromJust arg else "") | (i, arg) <- ports ]

showRange :: Maybe Range -> String
showRange Nothing = ""
showRange (Just (h, l)) = printf "[%s:%s] " (showExprConst h) (showExprConst l)

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
  | Number     BitVec
  | ConstBool  Bool
  | Ident      Identifier
  | IdentRange Identifier Range
  | IdentBit   Identifier Expr
  | Repeat     Expr [Expr]
  | Concat     [Expr]
  | ExprCall   Call
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

showBitVecDefault :: BitVec -> String
showBitVecDefault a = printf "%d'h%x" (width a) (value a)

showBitVecConst :: BitVec -> String
showBitVecConst a = show $ value a

instance Show Expr where show = showExpr showBitVecDefault

showExprConst :: Expr -> String
showExprConst = showExpr showBitVecConst

showExpr :: (BitVec -> String) -> Expr -> String
showExpr bv x = case x of
  String     a        -> printf "\"%s\"" a
  Number     a        -> bv a
  ConstBool  a        -> printf "1'b%s" (if a then "1" else "0")
  Ident      a        -> a
  IdentBit   a b      -> printf "%s[%s]"    a (showExprConst b)
  IdentRange a (b, c) -> printf "%s[%s:%s]" a (showExprConst b) (showExprConst c)
  Repeat     a b      -> printf "{%s {%s}}" (showExprConst a) (commas $ map s b)
  Concat     a        -> printf "{%s}" (commas $ map show a)
  ExprCall   a        -> show a
  UniOp      a b      -> printf "(%s %s)" (show a) (s b)
  BinOp      a b c    -> printf "(%s %s %s)" (s b) (show a) (s c)
  Mux        a b c    -> printf "(%s ? %s : %s)" (s a) (s b) (s c)
  Bit        a b      -> printf "(%s [%d])" (s a) b
  where
  s = showExpr bv

instance Num Expr where
  (+) = BinOp Add
  (-) = BinOp Sub
  (*) = BinOp Mul
  negate = UniOp USub
  abs = undefined
  signum = undefined
  fromInteger = Number . fromInteger

instance Bits Expr where
  (.&.) = BinOp BWAnd
  (.|.) = BinOp BWOr
  xor   = BinOp BWXor
  complement = UniOp BWNot
  isSigned _ = False
  shift        = error "Not supported: shift"
  rotate       = error "Not supported: rotate"
  bitSize      = error "Not supported: bitSize"
  bitSizeMaybe = error "Not supported: bitSizeMaybe"
  testBit      = error "Not supported: testBit"
  bit          = error "Not supported: bit"
  popCount     = error "Not supported: popCount"

instance Semigroup Expr where
  (<>) = mappend

instance Monoid Expr where
  mempty      = 0
  mappend a b = mconcat [a, b]
  mconcat     = Concat

data LHS
  = LHS       Identifier
  | LHSBit    Identifier Expr
  | LHSRange  Identifier Range
  | LHSConcat [LHS]
  deriving Eq

instance Show LHS where
  show (LHS        a       ) = a
  show (LHSBit     a b     ) = printf "%s[%s]"    a (showExprConst b)
  show (LHSRange   a (b, c)) = printf "%s[%s:%s]" a (showExprConst b) (showExprConst c)
  show (LHSConcat  a       ) = printf "{%s}" (commas $ map show a)

data Stmt
  = Block                 (Maybe Identifier) [Stmt]
  | StmtReg               (Maybe Range) [(Identifier, Maybe Range)]
  | StmtInteger           [Identifier]
  | Case                  Expr [Case] (Maybe Stmt)
  | BlockingAssignment    LHS Expr
  | NonBlockingAssignment LHS Expr
  | For                   (Identifier, Expr) Expr (Identifier, Expr) Stmt
  | If                    Expr Stmt Stmt
  | StmtCall              Call
  | Null
  deriving Eq

commas :: [String] -> String
commas = intercalate ", "

instance Show Stmt where
  show (Block                 Nothing  b       ) = printf "begin\n%s\nend" $ indent $ unlines' $ map show b
  show (Block                 (Just a) b       ) = printf "begin : %s\n%s\nend" a $ indent $ unlines' $ map show b
  show (StmtReg               a b              ) = printf "reg    %s%s;" (showRange a) (commas [ x ++ showRange  r | (x, r) <- b ])
  show (StmtInteger           a                ) = printf "integer %s;" $ commas a
  show (Case                  a b Nothing      ) = printf "case (%s)\n%s\nendcase"                 (show a) (indent $ unlines' $ map showCase b)
  show (Case                  a b (Just c)     ) = printf "case (%s)\n%s\n\tdefault:\n%s\nendcase" (show a) (indent $ unlines' $ map showCase b) (indent $ indent $ show c)
  show (BlockingAssignment    a b              ) = printf "%s = %s;" (show a) (show b)
  show (NonBlockingAssignment a b              ) = printf "%s <= %s;" (show a) (show b)
  show (For                   (a, b) c (d, e) f) = printf "for (%s = %s; %s; %s = %s)\n%s" a (show b) (show c) d (show e) $ indent $ show f
  show (If                    a b Null         ) = printf "if (%s)\n%s"           (show a) (indent $ show b)
  show (If                    a b c            ) = printf "if (%s)\n%s\nelse\n%s" (show a) (indent $ show b) (indent $ show c)
  show (StmtCall              a                ) = printf "%s;" (show a)
  show (Null                                   ) = ";"

type Case = ([Expr], Stmt)

showCase :: Case -> String
showCase (a, b) = printf "%s:\n%s" (commas $ map show a) (indent $ show b)

data Call = Call Identifier [Expr] deriving Eq

instance Show Call where
  show (Call a b) = printf "%s(%s)" a (commas $ map show b)

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
