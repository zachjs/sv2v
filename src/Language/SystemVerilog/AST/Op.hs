{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog operators (unary, binary, assignment, and stream)
 -}

module Language.SystemVerilog.AST.Op
    ( UniOp    (..)
    , BinOp    (..)
    , AsgnOp   (..)
    , StreamOp (..)
    ) where

data UniOp
    = LogNot
    | BitNot
    | UniAdd
    | UniSub
    | RedAnd
    | RedNand
    | RedOr
    | RedNor
    | RedXor
    | RedXnor
    deriving Eq

instance Show UniOp where
    show LogNot  = "!"
    show BitNot  = "~"
    show UniAdd  = "+"
    show UniSub  = "-"
    show RedAnd  = "&"
    show RedNand = "~&"
    show RedOr   = "|"
    show RedNor  = "~|"
    show RedXor  = "^"
    show RedXnor = "~^"

data BinOp
    = LogAnd
    | LogOr
    | LogImp
    | LogEq
    | BitAnd
    | BitXor
    | BitXnor
    | BitOr
    | Mul
    | Div
    | Mod
    | Add
    | Sub
    | Pow
    | ShiftL
    | ShiftR
    | ShiftAL
    | ShiftAR
    | Eq
    | Ne
    | TEq
    | TNe
    | WEq
    | WNe
    | Lt
    | Le
    | Gt
    | Ge
    deriving Eq

instance Show BinOp where
    show LogAnd  = "&&"
    show LogOr   = "||"
    show LogImp  = "->"
    show LogEq   = "<->"
    show BitAnd  = "&"
    show BitXor  = "^"
    show BitXnor = "~^"
    show BitOr   = "|"
    show Mul     = "*"
    show Div     = "/"
    show Mod     = "%"
    show Add     = "+"
    show Sub     = "-"
    show Pow     = "**"
    show ShiftL  = "<<"
    show ShiftR  = ">>"
    show ShiftAL = "<<<"
    show ShiftAR = ">>>"
    show Eq      = "=="
    show Ne      = "!="
    show TEq     = "==="
    show TNe     = "!=="
    show WEq     = "==?"
    show WNe     = "!=?"
    show Lt      = "<"
    show Le      = "<="
    show Gt      = ">"
    show Ge      = ">="

data AsgnOp
    = AsgnOpEq
    | AsgnOpNonBlocking
    | AsgnOp BinOp
    deriving Eq

instance Show AsgnOp where
    show AsgnOpEq = "="
    show AsgnOpNonBlocking = "<="
    show (AsgnOp op) = (show op) ++ "="

data StreamOp
    = StreamL
    | StreamR
    deriving Eq

instance Show StreamOp where
    show StreamL  = "<<"
    show StreamR  = ">>"
