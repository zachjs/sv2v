module Language.SystemVerilog.AST.Attr
    ( Attr
    , showsAttrs
    ) where

data Attr
instance Eq Attr

showsAttrs :: [Attr] -> ShowS
