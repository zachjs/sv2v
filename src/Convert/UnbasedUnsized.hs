{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for unbased, unsized literals ('0, '1, 'z, 'x)
 -
 - We convert the literals to be signed to enable sign extension, and give them
 - a size of 1 and a binary base. These values implicitly cast as desired in
 - Verilog-2005.
 -}

module Convert.UnbasedUnsized (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $
    traverseDescriptions $ traverseModuleItems $
    traverseExprs $ traverseNestedExprs convertExpr

digits :: [Char]
digits = ['0', '1', 'x', 'z', 'X', 'Z']

convertExpr :: Expr -> Expr
convertExpr (Number ['\'', ch]) =
    if elem ch digits
        then Number ("1'sb" ++ [ch])
        else error $ "unexpected unbased-unsized digit: " ++ [ch]
convertExpr other = other
