{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `bit`, `int`, `shortint`, `longint`, and `byte`
 -}

module Convert.IntTypes (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $
    traverseDescriptions $
    traverseModuleItems $
    traverseTypes convertType

convertType :: Type -> Type
convertType (IntegerAtom kw sg) = elaborateIntegerAtom $ IntegerAtom kw sg
convertType (IntegerVector TBit sg rs) = IntegerVector TLogic sg rs
convertType other = other
