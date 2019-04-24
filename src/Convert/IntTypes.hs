{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `int`, `shortint`, `longint`, and `byte`
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
convertType (IntegerAtom TInt      sg) = baseType sg Signed      32
convertType (IntegerAtom TShortint sg) = baseType sg Signed      16
convertType (IntegerAtom TLongint  sg) = baseType sg Signed      64
convertType (IntegerAtom TByte     sg) = baseType sg Unspecified  8
convertType other = other

-- makes a integer "compatible" type with the given signing, base signing and
-- size; if not unspecified, the first signing overrides the second
baseType :: Signing -> Signing -> Int -> Type
baseType sgOverride sgBase size =
    IntegerVector TReg sg [(Number hi, Number "0")]
    where
        hi = show (size - 1)
        sg = if sgOverride /= Unspecified
                then sgOverride
                else sgBase
