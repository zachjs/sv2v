{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `unsigned` types.
 -
 - Verilog-2005 makes `reg`, `wire`, etc. unsigned by default. Further, it does
 - not have the `unsigned` keyword. This conversion ensures we either mark a
 - data type as `signed` or leave the signing unspecified.
 -}

module Convert.Unsigned (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $
    traverseDescriptions $
    traverseModuleItems $
    traverseTypes convertType

convertType :: Type -> Type
convertType (IntegerVector t Unsigned rs) = IntegerVector t Unspecified rs
convertType (Net           t Unsigned rs) = Net           t Unspecified rs
convertType other = other
