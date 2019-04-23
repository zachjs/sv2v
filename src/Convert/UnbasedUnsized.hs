{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for unbased, unsized literals ('0, '1, 'z, 'x)
 -
 - Maintaining the unsized-ness of the literals is critical, but those digits
 - are all equivalent regardless of base. We simply convert them to all use a
 - binary base for compatibility with Verilog-2005.
 -}

module Convert.UnbasedUnsized (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert =
    traverseDescriptions $ traverseModuleItems $
    traverseExprs $ traverseNestedExprs convertExpr

digits :: [Char]
digits = ['0', '1', 'x', 'z', 'X', 'Z']

convertExpr :: Expr -> Expr
convertExpr (Number ['\'', ch]) =
    if elem ch digits
        then Number ("'b" ++ [ch])
        else error $ "unexpected unbased-unsized digit: " ++ [ch]
convertExpr other = other
