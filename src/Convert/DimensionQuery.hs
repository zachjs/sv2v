{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Elaboration of the "expression size system function" ($bits) and the "array
 - query functions" ($dimensions, $unpacked_dimensions, $left, $right, $low,
 - $high, $increment, and $size).
 -
 - Some tools support $bits and some of the other functions in Verilog, but it
 - is not part of the specification, so we have to convert them ourselves.
 -
 - Functions on types are trivially elaborated based on the dimensions of that
 - type, so long as it has been resolved to a primitive type.
 -
 - Functions on expressions relies on the `type` operator and the `TypeOf`
 - conversion to determine the underlying type of expression. The conversion of
 - `$bits` on expressions recursively breaks the expression into its subtypes
 - and finds their sizes.
 -}

module Convert.DimensionQuery (convert) where

import Data.List (elemIndex)

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert files =
    if files == files'
        then files
        else convert files'
    where files' = map (traverseDescriptions convertDescription) files

convertDescription :: Description -> Description
convertDescription =
    traverseModuleItems $ traverseExprs $ traverseNestedExprs convertExpr

-- elaborate integer atom types to have explicit dimensions
elaborateType :: Type -> Type
elaborateType (IntegerAtom t sg) =
    IntegerVector TLogic sg [(hi, lo)]
    where
        size = atomSize t
        hi = Number $ show (size - 1)
        lo = Number "0"
        atomSize :: IntegerAtomType -> Int
        atomSize TByte = 8
        atomSize TShortint = 16
        atomSize TInt = 32
        atomSize TLongint = 64
        atomSize TInteger = 32
        atomSize TTime = 64
elaborateType other = other

convertExpr :: Expr -> Expr

-- conversion for array dimensions functions
convertExpr (DimsFn FnBits v) =
    convertBits v
convertExpr (DimsFn fn (Right e)) =
    DimsFn fn $ Left $ TypeOf e
convertExpr (DimFn fn (Right e) d) =
    DimFn fn (Left $ TypeOf e) d
convertExpr (orig @ (DimsFn FnUnpackedDimensions (Left t))) =
    case t of
        UnpackedType _ rs -> Number $ show $ length rs
        TypeOf{} -> orig
        _ -> Number "0"
convertExpr (orig @ (DimsFn FnDimensions (Left t))) =
    case t of
        IntegerAtom{} -> Number "1"
        Alias{} -> orig
        TypeOf{} -> orig
        UnpackedType t' rs ->
            BinOp Add
                (Number $ show $ length rs)
                (DimsFn FnDimensions $ Left t')
        _ -> Number $ show $ length $ snd $ typeRanges t

-- conversion for array dimension functions on types
convertExpr (DimFn f (Left t) (Number str)) =
    if dm == Nothing || isUnresolved t then
        DimFn f (Left t) (Number str)
    else if d <= 0 || d > length rs then
        Number "'x"
    else case f of
        FnLeft -> fst r
        FnRight -> snd r
        FnIncrement -> endianCondExpr r (Number "1") (Number "-1")
        FnLow -> endianCondExpr r (snd r) (fst r)
        FnHigh -> endianCondExpr r (fst r) (snd r)
        FnSize -> rangeSize r
    where
        rs = case elaborateType t of
            UnpackedType tInner rsOuter ->
                rsOuter ++ (snd $ typeRanges $ elaborateType tInner)
            _ -> snd $ typeRanges $ elaborateType t
        dm = readNumber str
        Just d = dm
        r = rs !! (d - 1)
        isUnresolved :: Type -> Bool
        isUnresolved (Alias{}) = True
        isUnresolved (TypeOf{}) = True
        isUnresolved _ = False
convertExpr (DimFn f (Left t) d) =
    DimFn f (Left t) d

convertExpr other = other

-- simplify a bits expression
convertBits :: TypeOrExpr -> Expr
convertBits (Left t) =
    case elaborateType t of
        IntegerVector _ _ rs -> dimensionsSize rs
        Implicit        _ rs -> dimensionsSize rs
        Net           _ _ rs -> dimensionsSize rs
        Struct   _ fields rs ->
            BinOp Mul
                (dimensionsSize rs)
                (foldl (BinOp Add) (Number "0") fieldSizes)
            where fieldSizes = map (DimsFn FnBits . Left . fst) fields
        UnpackedType  t'  rs ->
            BinOp Mul
                (dimensionsSize rs)
                (DimsFn FnBits $ Left t')
        _ -> DimsFn FnBits $ Left t
convertBits (Right e) =
    case e of
        Concat exprs ->
            foldl (BinOp Add) (Number "0") $
            map (convertBits . Right) $
            exprs
        Stream _ _ exprs -> convertBits $ Right $ Concat exprs
        Number n ->
            case elemIndex '\'' n of
                Nothing -> Number "32"
                Just idx -> Number $ take idx n
        _ -> DimsFn FnBits $ Left $ TypeOf e
