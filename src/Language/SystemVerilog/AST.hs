{-# LANGUAGE FlexibleInstances #-}
module Language.SystemVerilog.AST
  ( Identifier
  , Description(..)
  , PackageItem(..)
  , ModuleItem (..)
  , Direction  (..)
  , Type       (..)
  , Stmt       (..)
  , LHS        (..)
  , Expr       (..)
  , UniOp      (..)
  , BinOp      (..)
  , AsgnOp     (..)
  , Sense      (..)
  , Timing     (..)
  , GenItem    (..)
  , AlwaysKW   (..)
  , CaseKW     (..)
  , PartKW     (..)
  , Decl       (..)
  , Lifetime   (..)
  , AST
  , PortBinding
  , ModportDecl
  , Case
  , Range
  , GenCase
  , typeRanges
  , simplify
  , rangeSize
  ) where

import Data.List
import Data.Maybe
import Text.Printf
import Text.Read (readMaybe)

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

data Type
  = Reg              [Range]
  | Wire             [Range]
  | Logic            [Range]
  | Alias Identifier [Range]
  | Implicit         [Range]
  | IntegerT
  | Enum (Maybe Type) [(Identifier, Maybe Expr)] [Range]
  | Struct Bool [(Type, Identifier)] [Range]
  | InterfaceT Identifier (Maybe Identifier) [Range]
  deriving (Eq, Ord)

instance Show Type where
  show (Reg      r) = "reg"   ++ (showRanges r)
  show (Wire     r) = "wire"  ++ (showRanges r)
  show (Logic    r) = "logic" ++ (showRanges r)
  show (Alias t  r) = t       ++ (showRanges r)
  show (Implicit r) =            (showRanges r)
  show (IntegerT  ) = "integer"
  show (InterfaceT x my r) = x ++ yStr ++ (showRanges r)
    where yStr = maybe "" ("."++) my
  show (Enum mt vals r) = printf "enum %s{%s}%s" tStr (commas $ map showVal vals) (showRanges r)
    where
      tStr = maybe "" showPad mt
      showVal :: (Identifier, Maybe Expr) -> String
      showVal (x, e) = x ++ (showAssignment e)
  show (Struct p items r) = printf "struct %s{\n%s\n}%s" packedStr itemsStr (showRanges r)
    where
      packedStr = if p then "packed " else ""
      itemsStr = indent $ unlines' $ map showItem items
      showItem (t, x) = printf "%s %s;" (show t) x

instance Show ([Range] -> Type) where
  show tf = show (tf [])

instance Eq ([Range] -> Type) where
  (==) tf1 tf2 = (tf1 []) == (tf2 [])

instance Ord ([Range] -> Type) where
  compare tf1 tf2 = compare (show tf1) (show tf2)

typeRanges :: Type -> ([Range] -> Type, [Range])
typeRanges (Reg      r) = (Reg     , r)
typeRanges (Wire     r) = (Wire    , r)
typeRanges (Logic    r) = (Logic   , r)
typeRanges (Alias  t r) = (Alias  t, r)
typeRanges (Implicit r) = (Implicit, r)
typeRanges (IntegerT  ) = (\[] -> IntegerT, [])
typeRanges (Enum t v r) = (Enum t v, r)
typeRanges (Struct p l r) = (Struct p l, r)
typeRanges (InterfaceT x my r) = (InterfaceT x my, r)

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
  | Instance   Identifier [PortBinding] Identifier [PortBinding]
  | Genvar     Identifier
  | Generate   [GenItem]
  | Modport    Identifier [ModportDecl]
  | Initial    Stmt
  | MIPackageItem PackageItem
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
    Instance   m params i ports
      | null params -> printf "%s %s%s;"     m                    i (showPorts ports)
      | otherwise   -> printf "%s #%s %s%s;" m (showPorts params) i (showPorts ports)
    Genvar     x -> printf "genvar %s;" x
    Generate   b -> printf "generate\n%s\nendgenerate" (indent $ unlines' $ map show b)
    Modport    x l   -> printf "modport %s(\n%s\n);" x (indent $ intercalate ",\n" $ map showModportDecl l)
    Initial    s     -> printf "initial %s" (show s)
    MIPackageItem i  -> show i
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
  | Range      Expr Range
  | Bit        Expr Expr
  | Repeat     Expr [Expr]
  | Concat     [Expr]
  | Call       Identifier [Expr]
  | UniOp      UniOp Expr
  | BinOp      BinOp Expr Expr
  | Mux        Expr Expr Expr
  | Cast       Type Expr
  | Access     Expr Identifier
  | Pattern    [(Maybe Identifier, Expr)]
  deriving (Eq, Ord)

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
  deriving (Eq, Ord)

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
  | Pow
  | ShiftAL
  | ShiftAR
  | TEq
  | TNe
  | WEq
  | WNe
  deriving (Eq, Ord)

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
    Pow    -> "**"
    ShiftAL -> "<<<"
    ShiftAR -> ">>>"
    TEq     -> "==="
    TNe     -> "!=="
    WEq     -> "==?"
    WNe     -> "!=?"

instance Show Expr where
  show x = case x of
    String     a        -> printf "\"%s\"" a
    Number     a        -> a
    ConstBool  a        -> printf "1'b%s" (if a then "1" else "0")
    Ident      a        -> a
    Bit        a b      -> printf "%s[%s]"    (show a) (show b)
    Range      a (b, c) -> printf "%s[%s:%s]" (show a) (show b) (show c)
    Repeat     a b      -> printf "{%s {%s}}" (show a) (commas $ map show b)
    Concat     a        -> printf "{%s}" (commas $ map show a)
    Call       a b      -> printf "%s(%s)" a (commas $ map show b)
    UniOp      a b      -> printf "(%s %s)" (show a) (show b)
    BinOp      a b c    -> printf "(%s %s %s)" (show b) (show a) (show c)
    Mux        a b c    -> printf "(%s ? %s : %s)" (show a) (show b) (show c)
    Cast       a b      -> printf "%s'(%s)" (show a) (show b)
    Access     e n      -> printf "%s.%s" (show e) n
    Pattern    l        -> printf "'{\n%s\n}" (showPatternItems l)
    where
      showPatternItems :: [(Maybe Identifier, Expr)] -> String
      showPatternItems l = indent $ intercalate ",\n" (map showPatternItem l)
      showPatternItem :: (Maybe Identifier, Expr) -> String
      showPatternItem (Nothing, e) = show e
      showPatternItem (Just n , e) = printf "%s: %s" n (show e)

data AsgnOp
  = AsgnOpEq
  | AsgnOp BinOp
  deriving Eq

instance Show AsgnOp where
  show AsgnOpEq = "="
  show (AsgnOp op) = (show op) ++ "="

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
  | Asgn    LHS Expr
  | While   Expr Stmt
  | RepeatL Expr Stmt
  | DoWhile Expr Stmt
  | Forever Stmt
  | If      Expr Stmt Stmt
  | Timing  Timing Stmt
  | Return  Expr
  | Subroutine Identifier [Expr]
  | Null
  deriving Eq

commas :: [String] -> String
commas = intercalate ", "

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
  show (Asgn      v e) = printf "%s <= %s;" (show v) (show e)
  show (While   e s) = printf  "while (%s) %s" (show e) (show s)
  show (RepeatL e s) = printf "repeat (%s) %s" (show e) (show s)
  show (DoWhile e s) = printf "do %s while (%s);" (show s) (show e)
  show (Forever s  ) = printf "forever %s" (show s)
  show (If a b Null) = printf "if (%s) %s"         (show a) (show b)
  show (If a b c   ) = printf "if (%s) %s\nelse %s" (show a) (show b) (show c)
  show (Return e   ) = printf "return %s;" (show e)
  show (Subroutine x a) = printf "%s(%s);" x (commas $ map show a)
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
  | GenFor   (Identifier, Expr) Expr (Identifier, AsgnOp, Expr) Identifier [GenItem]
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
  show (GenFor (x1, e1) c (x2, o2, e2) x is) = printf "for (%s = %s; %s; %s %s %s) %s" x1 (show e1) (show c) x2 (show o2) (show e2) (show $ GenBlock (Just x) is)
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
