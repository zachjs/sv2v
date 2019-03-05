{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for binary assignment operators, which only appear in generate for
 - loops. We simply elaborate them in the obvious manner.
 -}

module Convert.AsgnOp (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert =
    traverseDescriptions $
    traverseModuleItems $
    traverseGenItems convertGenItem

convertGenItem :: GenItem -> GenItem
convertGenItem (GenFor a b (ident, AsgnOp op, expr) c d) =
    GenFor a b (ident, AsgnOpEq, BinOp op (Ident ident) expr) c d
convertGenItem other = other
