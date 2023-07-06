module Language.SystemVerilog.AST.Attr
    ( Attr
    , showsAttrs
    ) where

data Attr
instance Eq Attr
instance Show Attr

showsAttrs :: [Attr] -> ShowS
