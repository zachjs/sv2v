{-# LANGUAGE FlexibleInstances #-}
module Language.SystemVerilog.AST.Attr
    ( Attr
    ) where

data Attr
instance Eq Attr
instance Show Attr
instance {-# OVERLAPPING #-} Show [Attr]
