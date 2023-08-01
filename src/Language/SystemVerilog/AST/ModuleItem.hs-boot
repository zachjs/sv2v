module Language.SystemVerilog.AST.ModuleItem
    ( ModuleItem
    , showGenModuleItem
    ) where

data ModuleItem
instance Eq ModuleItem
instance Show ModuleItem

showGenModuleItem :: ModuleItem -> String
