module Language.SystemVerilog.AST.Type
    ( Identifier
    , Type
    ) where

type Identifier = String

data Type
instance Eq Type
instance Show Type
