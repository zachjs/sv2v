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
 - Functions on expressions requires a scoped traversal to determine the
 - underlying type of expression. The conversion of `$bits` on expressions
 - recursively breaks the expression into its subtypes and finds their sizes.
 -}

module Convert.DimensionQuery (convert) where

import Control.Monad.State
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Info = Map.Map Identifier (Type, [Range])

convert :: [AST] -> [AST]
convert files =
    if files == files'
        then files
        else convert files'
    where files' = map (traverseDescriptions convertDescription) files

convertDescription :: Description -> Description
convertDescription =
    scopedConversion traverseDeclM traverseModuleItemM traverseStmtM Map.empty

traverseDeclM :: Decl -> State Info Decl
traverseDeclM decl = do
    case decl of
        Variable _ t ident a _ -> modify $ Map.insert ident (elaborateType t, a)
        Param    _ t ident   _ -> modify $ Map.insert ident (elaborateType t, [])
        ParamType    _     _ _ -> return ()
    item <- traverseModuleItemM (MIPackageItem $ Decl decl)
    let MIPackageItem (Decl decl') = item
    return decl'

traverseModuleItemM :: ModuleItem -> State Info ModuleItem
traverseModuleItemM item = traverseExprsM traverseExprM item

traverseStmtM :: Stmt -> State Info Stmt
traverseStmtM stmt = traverseStmtExprsM traverseExprM stmt

traverseExprM :: Expr -> State Info Expr
traverseExprM = traverseNestedExprsM $ stately converter
    where converter a b = simplify $ (traverseNestedExprs (convertExpr a) b)

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

convertExpr :: Info -> Expr -> Expr

-- conversion for array dimensions functions
convertExpr info (DimsFn FnBits v) =
    convertBits info v
convertExpr _ (DimsFn FnUnpackedDimensions (Left _)) =
    Number "0"
convertExpr _ (DimsFn FnDimensions (Left t)) =
    Number $ show $
    case t of
        IntegerAtom _ _ -> 1
        _ -> length $ snd $ typeRanges t
convertExpr info (DimsFn FnUnpackedDimensions (Right (Ident x))) =
    case Map.lookup x info of
        Nothing -> DimsFn FnUnpackedDimensions $ Right $ Ident x
        Just (_, rs) -> Number $ show $ length rs
convertExpr info (DimsFn FnDimensions (Right (Ident x))) =
    case Map.lookup x info of
        Nothing -> DimsFn FnDimensions $ Right $ Ident x
        Just (t, rs) -> DimsFn FnDimensions $ Left $ tf rsCombined
            where
                (tf, trs) = typeRanges t
                rsCombined = rs ++ trs

-- conversion for array dimension functions on types
convertExpr _ (DimFn f (Left t) (Number str)) =
    if dm == Nothing || isAlias t then
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
        (_, rs) = typeRanges $ elaborateType t
        dm = readNumber str
        Just d = dm
        r = rs !! (d - 1)
        isAlias :: Type -> Bool
        isAlias (Alias _ _ _) = True
        isAlias _ = False
convertExpr _ (DimFn f (Left t) d) =
    DimFn f (Left t) d

-- conversion for array dimension functions on expression
convertExpr info (DimFn f (Right (Ident x)) d) =
    case Map.lookup x info of
        Nothing -> DimFn f (Right (Ident x)) d
        Just (t, rs) -> DimFn f (Left $ tf rsCombined) d
            where
                (tf, trs) = typeRanges t
                rsCombined = rs ++ trs
convertExpr info (DimFn f (Right (Bit (Ident x) idx)) d) =
    case Map.lookup x info of
        Nothing -> DimFn f (Right $ Bit (Ident x) idx) d
        Just (t, rs) -> DimFn f (Left t') d
            where t' = popRange t rs
convertExpr _ (DimFn f (Right e) d) =
    DimFn f (Right e) d

convertExpr _ other = other

-- simplify a bits expression given scoped type information
convertBits :: Info -> TypeOrExpr -> Expr
convertBits _ (Left t) =
    case elaborateType t of
        IntegerVector _ _ rs -> dimensionsSize rs
        Implicit        _ rs -> dimensionsSize rs
        Net             _ rs -> dimensionsSize rs
        _ -> DimsFn FnBits $ Left t
convertBits info (Right e) =
    case e of
        Ident x ->
            case Map.lookup x info of
                Nothing -> DimsFn FnBits $ Right e
                Just (t, rs) -> simplify $ BinOp Mul
                        (dimensionsSize rs)
                        (convertBits info $ Left t)
        Concat exprs ->
            foldl (BinOp Add) (Number "0") $
            map (convertBits info . Right) $
            exprs
        Range expr mode range ->
            simplify $ BinOp Mul size
                (convertBits info $ Right $ Bit expr (Number "0"))
            where
                size = case mode of
                    NonIndexed   -> rangeSize range
                    IndexedPlus  -> snd range
                    IndexedMinus -> snd range
        Bit (Ident x) idx ->
            case Map.lookup x info of
                Nothing -> DimsFn FnBits $ Right $ Bit (Ident x) idx
                Just (t, rs) ->
                    convertBits info $ Left t'
                    where t' = popRange t rs
        Stream _ _ exprs -> convertBits info $ Right $ Concat exprs
        Number n ->
            case elemIndex '\'' n of
                Nothing -> Number "32"
                Just idx -> Number $ take idx n
        _ -> DimsFn FnBits $ Right e

-- combines the given type and dimensions and returns a new type with the
-- innermost range removed
popRange :: Type -> [Range] -> Type
popRange t rs =
    tf $ tail rsCombined
    where
        (tf, trs) = typeRanges t
        rsCombined = rs ++ trs
