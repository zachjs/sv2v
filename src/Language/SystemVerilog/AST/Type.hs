{-# LANGUAGE FlexibleInstances #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog types
 -}

module Language.SystemVerilog.AST.Type
    ( Identifier
    , Field
    , Type    (..)
    , Signing (..)
    , Packing (..)
    , NetType (..)
    , IntegerVectorType (..)
    , IntegerAtomType   (..)
    , NonIntegerType    (..)
    , typeRanges
    ) where

import Text.Printf (printf)

import Language.SystemVerilog.AST.Expr
import Language.SystemVerilog.AST.ShowHelp

type Identifier = String

type Item = (Identifier, Maybe Expr)
type Field = (Type, Identifier)

data Type
    = IntegerVector IntegerVectorType  Signing [Range]
    | IntegerAtom   IntegerAtomType    Signing
    | NonInteger    NonIntegerType
    | Net           NetType            Signing [Range]
    | Implicit                         Signing [Range]
    | Alias    (Maybe Identifier) Identifier   [Range]
    | Enum     (Maybe Type) [Item]             [Range]
    | Struct   Packing      [Field]            [Range]
    | Union    Packing      [Field]            [Range]
    | InterfaceT Identifier (Maybe Identifier) [Range]
    deriving (Eq, Ord)

instance Show Type where
    show (Alias      ps xx    rs) = printf "%s%s%s" (maybe "" (++ "::") ps)  xx  (showRanges rs)
    show (Net           kw sg rs) = printf "%s%s%s" (show kw) (showPadBefore sg) (showRanges rs)
    show (Implicit         sg rs) = printf "%s%s"             (showPad       sg) (dropWhile (== ' ') $ showRanges rs)
    show (IntegerVector kw sg rs) = printf "%s%s%s" (show kw) (showPadBefore sg) (showRanges rs)
    show (IntegerAtom   kw sg   ) = printf "%s%s"   (show kw) (showPadBefore sg)
    show (NonInteger    kw      ) = printf "%s"     (show kw)
    show (InterfaceT x my r) = x ++ yStr ++ (showRanges r)
        where yStr = maybe "" ("."++) my
    show (Enum mt vals r) = printf "enum %s{%s}%s" tStr (commas $ map showVal vals) (showRanges r)
        where
            tStr = maybe "" showPad mt
            showVal :: (Identifier, Maybe Expr) -> String
            showVal (x, e) = x ++ (showAssignment e)
    show (Struct p items r) = printf "struct %s{\n%s\n}%s" (showPad p) (showFields items) (showRanges r)
    show (Union  p items r) = printf  "union %s{\n%s\n}%s" (showPad p) (showFields items) (showRanges r)

showFields :: [Field] -> String
showFields items = itemsStr
    where
        itemsStr = indent $ unlines' $ map showItem items
        showItem (t, x) = printf "%s %s;" (show t) x

instance Show ([Range] -> Type) where
    show tf = show (tf [])
instance Eq ([Range] -> Type) where
    (==) tf1 tf2 = (tf1 []) == (tf2 [])
instance Ord ([Range] -> Type) where
    compare tf1 tf2 = compare (tf1 []) (tf2 [])

instance Show (Signing -> [Range] -> Type) where
    show tf = show (tf Unspecified)
instance Eq (Signing -> [Range] -> Type) where
    (==) tf1 tf2 = (tf1 Unspecified) == (tf2 Unspecified)
instance Ord (Signing -> [Range] -> Type) where
    compare tf1 tf2 = compare (tf1 Unspecified) (tf2 Unspecified)

typeRanges :: Type -> ([Range] -> Type, [Range])
typeRanges (Alias      ps xx    rs) = (Alias      ps xx   , rs)
typeRanges (Net           kw sg rs) = (Net           kw sg, rs)
typeRanges (Implicit         sg rs) = (Implicit         sg, rs)
typeRanges (IntegerVector kw sg rs) = (IntegerVector kw sg, rs)
typeRanges (IntegerAtom   kw sg   ) = (nullRange $ IntegerAtom kw sg, [])
typeRanges (NonInteger    kw      ) = (nullRange $ NonInteger  kw   , [])
typeRanges (Enum   t v r) = (Enum   t v, r)
typeRanges (Struct p l r) = (Struct p l, r)
typeRanges (Union  p l r) = (Union  p l, r)
typeRanges (InterfaceT x my r) = (InterfaceT x my, r)

nullRange :: Type -> ([Range] -> Type)
nullRange t [] = t
nullRange t [(Number "0", Number "0")] = t
nullRange (IntegerAtom TInteger sg) rs =
    -- integer arrays are allowed in SystemVerilog but not in Verilog
    IntegerVector TBit sg (rs ++ [(Number "31", Number "0")])
nullRange (IntegerAtom TInt sg) rs =
    -- int arrays are allowed in SystemVerilog but not in Verilog
    IntegerVector TBit sg (rs ++ [(Number "31", Number "0")])
nullRange t rs =
    error $ "non-vector type " ++ show t ++
        " cannot have a packed dimesions:" ++ show rs

data Signing
    = Unspecified
    | Signed
    | Unsigned
    deriving (Eq, Ord)

instance Show Signing where
    show Unspecified = ""
    show Signed = "signed"
    show Unsigned = "unsigned"

data NetType
    = TSupply0
    | TSupply1
    | TTri
    | TTriand
    | TTrior
    | TTrireg
    | TTri0
    | TTri1
    | TUwire
    | TWire
    | TWand
    | TWor
    deriving (Eq, Ord)
data IntegerVectorType
    = TBit
    | TLogic
    | TReg
    deriving (Eq, Ord)
data IntegerAtomType
    = TByte
    | TShortint
    | TInt
    | TLongint
    | TInteger
    | TTime
    deriving (Eq, Ord)
data NonIntegerType
    = TShortreal
    | TReal
    | TRealtime
    | TString
    deriving (Eq, Ord)

instance Show NetType where
    show TSupply0   = "supply0"
    show TSupply1   = "supply1"
    show TTri       = "tri"
    show TTriand    = "triand"
    show TTrior     = "trior"
    show TTrireg    = "trireg"
    show TTri0      = "tri0"
    show TTri1      = "tri1"
    show TUwire     = "uwire"
    show TWire      = "wire"
    show TWand      = "wand"
    show TWor       = "wor"
instance Show IntegerVectorType where
    show TBit       = "bit"
    show TLogic     = "logic"
    show TReg       = "reg"
instance Show IntegerAtomType where
    show TByte      = "byte"
    show TShortint  = "shortint"
    show TInt       = "int"
    show TLongint   = "longint"
    show TInteger   = "integer"
    show TTime      = "time"
instance Show NonIntegerType where
    show TShortreal = "shortreal"
    show TReal      = "real"
    show TRealtime  = "realtime"
    show TString    = "string"

data Packing
    = Unpacked
    | Packed Signing
    deriving (Eq, Ord)

instance Show Packing where
    show (Unpacked) = ""
    show (Packed s) = "packed" ++ (showPadBefore s)
