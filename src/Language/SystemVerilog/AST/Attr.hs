{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Initial Verilog AST Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - SystemVerilog attribute instances
 -}

module Language.SystemVerilog.AST.Attr
    ( Attr (..)
    , AttrSpec
    ) where

import Text.Printf (printf)

import Language.SystemVerilog.AST.ShowHelp (commas)
import Language.SystemVerilog.AST.Expr (Expr, showAssignment)
import Language.SystemVerilog.AST.Type (Identifier)

data Attr
    = Attr [AttrSpec]
    deriving Eq

type AttrSpec = (Identifier, Expr)

instance Show Attr where
    show (Attr specs) = printf "(* %s *)" $ commas $ map showSpec specs

showSpec :: AttrSpec -> String
showSpec (x, e) = x ++ showAssignment e
