{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for flattening variables with multiple packed dimensions
 -
 - This removes one packed dimension per identifier per pass. This works fine
 - because this conversion is repeatedly applied.
 -
 - We previously had a very complex conversion which used `generate` to make
 - flattened and unflattened versions of the array as necessary. This has now
 - been "simplified" to always flatten the array, and then rewrite all usages of
 - the array as appropriate.
 -
 - A previous iteration of this conversion aggressively flattened all dimensions
 - (even if unpacked) in any multidimensional data declaration. This had the
 - unfortunate side effect of packing memories, which could hinder efficient
 - synthesis. Now this conversion only flattens packed dimensions and leaves the
 - (only potentially necessary) movement of dimensions from unpacked to packed
 - to the separate UnpackedArray conversion.
 -
 - Note that the ranges being combined may not be of the form [hi:lo], and need
 - not even be the same direction! Because of this, we have to flip around the
 - indices of certain accesses.
 -}

module Convert.MultiplePacked (convert) where

import Convert.ExprUtils
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Tuple (swap)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type TypeInfo = (Type, [Range])

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription description@(Part _ _ Module _ _ _ _) =
    partScoper traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM
    description
convertDescription other = other

-- collects and converts declarations with multiple packed dimensions
traverseDeclM :: Decl -> Scoper TypeInfo Decl
traverseDeclM (Variable dir t ident a e) = do
    t' <- traverseTypeM t a ident
    traverseDeclExprsM traverseExprM $ Variable dir t' ident a e
traverseDeclM net@Net{} =
    traverseNetAsVarM traverseDeclM net
traverseDeclM (Param s t ident e) = do
    t' <- traverseTypeM t [] ident
    traverseDeclExprsM traverseExprM $ Param s t' ident e
traverseDeclM other = traverseDeclExprsM traverseExprM other

-- write down the given declaration and then flatten it
traverseTypeM :: Type -> [Range] -> Identifier -> Scoper TypeInfo Type
traverseTypeM t a ident = do
    tScoped <- scopeType t
    insertElem ident (tScoped, a)
    return $ flattenType t

-- flatten the innermost dimension of the given type, and any types it contains
flattenType :: Type -> Type
flattenType t =
    tf $ if length ranges <= 1
        then ranges
        else rangesFlat
    where
        (tf, ranges) = case t of
            Struct pk fields rs ->
                (Struct pk fields', rs)
                where fields' = flattenFields fields
            Union  pk fields rs ->
                (Union  pk fields', rs)
                where fields' = flattenFields fields
            _ -> typeRanges t
        r1 : r2 : rest = ranges
        rangesFlat = combineRanges r1 r2 : rest

-- flatten the types in a given list of struct/union fields
flattenFields :: [Field] -> [Field]
flattenFields = map $ first flattenType

traverseModuleItemM :: ModuleItem -> Scoper TypeInfo ModuleItem
traverseModuleItemM (Instance m p x rs l) = do
    -- converts multi-dimensional instances
    rs' <- if length rs <= 1
        then return rs
        else do
            let t = Implicit Unspecified rs
            tScoped <- scopeType t
            insertElem x (tScoped, [])
            let r1 : r2 : rest = rs
            return $ (combineRanges r1 r2) : rest
    traverseExprsM traverseExprM $ Instance m p x rs' l
traverseModuleItemM item =
    traverseLHSsM  traverseLHSM  item >>=
    traverseExprsM traverseExprM

-- combines two ranges into one flattened range
combineRanges :: Range -> Range -> Range
combineRanges r1 r2 = r
    where
        rYY = combine r1 r2
        rYN = combine r1 (swap r2)
        rNY = combine (swap r1) r2
        rNN = combine (swap r1) (swap r2)
        rY = endianCondRange r2 rYY rYN
        rN = endianCondRange r2 rNY rNN
        r = endianCondRange r1 rY rN

        combine :: Range -> Range -> Range
        combine (s1, e1) (s2, e2) =
            (simplify upper, simplify lower)
            where
                size1 = rangeSizeHiLo (s1, e1)
                size2 = rangeSizeHiLo (s2, e2)
                lower = binOp Add e2 (binOp Mul e1 size2)
                upper = binOp Add (binOp Mul size1 size2)
                            (binOp Sub lower (RawNum 1))

traverseStmtM :: Stmt -> Scoper TypeInfo Stmt
traverseStmtM =
    traverseStmtLHSsM  traverseLHSM  >=>
    traverseStmtExprsM traverseExprM

traverseExprM :: Expr -> Scoper TypeInfo Expr
traverseExprM = traverseNestedExprsM convertExprM

traverseGenItemM :: GenItem -> Scoper TypeInfo GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

-- LHSs need to be converted too. Rather than duplicating the procedures, we
-- turn LHSs into expressions temporarily and use the expression conversion.
traverseLHSM :: LHS -> Scoper TypeInfo LHS
traverseLHSM = traverseNestedLHSsM traverseLHSSingleM
    where
        -- We can't use traverseExprM directly because that would cause Exprs
        -- inside of LHSs to be converted twice in a single cycle!
        traverseLHSSingleM :: LHS -> Scoper TypeInfo LHS
        traverseLHSSingleM lhs = do
            let expr = lhsToExpr lhs
            expr' <- convertExprM expr
            let Just lhs' = exprToLHS expr'
            return lhs'

convertExprM :: Expr -> Scoper TypeInfo Expr
convertExprM = embedScopes convertExpr

convertExpr :: Scopes TypeInfo -> Expr -> Expr
convertExpr scopes =
    rewriteExpr
    where
        -- removes the innermost dimensions of the given type information, and
        -- applies the given transformation to the expression
        dropLevel :: TypeInfo -> TypeInfo
        dropLevel (t, a) =
            (tf rs', a')
            where
                (tf, rs) = typeRanges t
                (rs', a') = case (rs, a) of
                    ([], []) -> ([], [])
                    (packed, []) -> (tail packed, [])
                    (packed, unpacked) -> (packed, tail unpacked)

        -- given an expression, returns its type information, if possible
        levels :: Expr -> Maybe TypeInfo
        levels (Bit expr a) =
            case levels expr of
                Just info -> Just $ dropLevel info
                Nothing -> fallbackLevels $ Bit expr a
        levels (Range expr _ _) =
            fmap dropLevel $ levels expr
        levels (Dot expr x) =
            case levels expr of
                Just (Struct _ fields [], []) -> dropDot fields
                Just (Union  _ fields [], []) -> dropDot fields
                _ -> fallbackLevels $ Dot expr x
            where
                dropDot :: [Field] -> Maybe TypeInfo
                dropDot fields =
                    if Map.member x fieldMap
                        then Just (fieldType, [])
                        else Nothing
                    where
                        fieldMap = Map.fromList $ map swap fields
                        fieldType = fieldMap Map.! x
        levels expr = fallbackLevels expr

        fallbackLevels :: Expr -> Maybe TypeInfo
        fallbackLevels expr =
            fmap thd3 res
            where
                res = lookupElem scopes expr
                thd3 (_, _, c) = c

        -- given an expression, returns the two most significant (innermost,
        -- leftmost) packed dimensions
        dims :: Expr -> Maybe (Range, Range)
        dims expr =
            case levels expr of
                Just (t, []) ->
                    case snd $ typeRanges t of
                        dimInner : dimOuter : _ ->
                            Just (dimInner, dimOuter)
                        _ -> Nothing
                _ -> Nothing

        -- if the given range is flipped, the result will flip around the given
        -- indexing expression
        orientIdx :: Range -> Expr -> Expr
        orientIdx r e =
            endianCondExpr r e eSwapped
            where
                eSwapped = binOp Sub (snd r) (binOp Sub e (fst r))

        -- Converted idents are prefixed with an invalid character to ensure
        -- that are not converted twice when the traversal steps downward. When
        -- the prefixed identifier is encountered at the lowest level, it is
        -- removed.

        rewriteExpr :: Expr -> Expr
        rewriteExpr expr@Ident{} = expr
        rewriteExpr orig@(Bit (Bit expr idxInner) idxOuter) =
            if isJust maybeDims && expr == rewriteExpr expr
                then Bit expr idx'
                else rewriteExprLowPrec orig
            where
                maybeDims = dims expr
                Just (dimInner, dimOuter) = maybeDims
                idxInner' = orientIdx dimInner idxInner
                idxOuter' = orientIdx dimOuter idxOuter
                base = binOp Mul idxInner' (rangeSize dimOuter)
                idx' = simplify $ binOp Add base idxOuter'
        rewriteExpr orig@(Range (Bit expr idxInner) NonIndexed rangeOuter) =
            if isJust maybeDims && expr == rewriteExpr expr
                then rewriteExpr $ Range exprOuter IndexedMinus range
                else rewriteExprLowPrec orig
            where
                maybeDims = dims expr
                exprOuter = Bit expr idxInner
                baseDec = fst rangeOuter
                baseInc = binOp Sub (binOp Add baseDec len) (RawNum 1)
                base = endianCondExpr rangeOuter baseDec baseInc
                len = rangeSize rangeOuter
                range = (base, len)
        rewriteExpr orig@(Range (Bit expr idxInner) modeOuter rangeOuter) =
            if isJust maybeDims && expr == rewriteExpr expr
                then Range expr modeOuter range'
                else rewriteExprLowPrec orig
            where
                maybeDims = dims expr
                Just (dimInner, dimOuter) = maybeDims
                idxInner' = orientIdx dimInner idxInner
                (baseOuter, lenOuter) = rangeOuter
                baseOuter' = orientIdx dimOuter baseOuter
                start = binOp Mul idxInner' (rangeSize dimOuter)
                baseDec = binOp Add start baseOuter'
                baseInc = if modeOuter == IndexedPlus
                    then binOp Add (binOp Sub baseDec len) one
                    else binOp Sub (binOp Add baseDec len) one
                base = endianCondExpr dimOuter baseDec baseInc
                len = lenOuter
                range' = (base, len)
                one = RawNum 1
        rewriteExpr (Cast (Left t) expr) =
            Cast (Left $ flattenType t) expr
        rewriteExpr other =
            rewriteExprLowPrec other

        rewriteExprLowPrec :: Expr -> Expr
        rewriteExprLowPrec orig@(Bit expr idx) =
            if isJust maybeDims && expr == rewriteExpr expr
                then Range expr mode' range'
                else orig
            where
                maybeDims = dims expr
                Just (dimInner, dimOuter) = maybeDims
                mode' = IndexedPlus
                idx' = orientIdx dimInner idx
                len = rangeSize dimOuter
                base = binOp Add (endianCondExpr dimOuter (snd dimOuter) (fst dimOuter)) (binOp Mul idx' len)
                range' = (simplify base, simplify len)
        rewriteExprLowPrec orig@(Range expr NonIndexed range) =
            if isJust maybeDims && expr == rewriteExpr expr
                then rewriteExpr $ Range expr IndexedMinus range'
                else orig
            where
                maybeDims = dims expr
                baseDec = fst range
                baseInc = binOp Sub (binOp Add baseDec len) (RawNum 1)
                base = endianCondExpr range baseDec baseInc
                len = rangeSize range
                range' = (base, len)
        rewriteExprLowPrec orig@(Range expr mode range) =
            if isJust maybeDims && expr == rewriteExpr expr
                then Range expr mode' range'
                else orig
            where
                maybeDims = dims expr
                Just (dimInner, dimOuter) = maybeDims
                sizeOuter = rangeSize dimOuter
                offsetOuter = uncurry (endianCondExpr dimOuter) $ swap dimOuter
                (baseOrig, lenOrig) = range
                lenOrigMinusOne = binOp Sub lenOrig (RawNum 1)
                baseSwapped =
                    orientIdx dimInner $
                    if mode == IndexedPlus
                        then
                            endianCondExpr dimInner
                            baseOrig
                            (binOp Add baseOrig lenOrigMinusOne)
                        else
                            endianCondExpr dimInner
                            (binOp Sub baseOrig lenOrigMinusOne)
                            baseOrig
                base = binOp Add offsetOuter (binOp Mul sizeOuter baseSwapped)
                mode' = IndexedPlus
                len = binOp Mul sizeOuter lenOrig
                range' = (base, len)
        rewriteExprLowPrec other = other

-- Traditional identity operations like `+ 0` and `* 1` are not no-ops in
-- SystemVerilog because they may implicitly extend the width of the other
-- operand. Encouraged by the official language specifications (e.g., Section
-- 11.6.2 of IEEE 1800-2017), these operations are used in real designs as
-- workarounds for the standard expression evaluation semantics.
--
-- The process of flattening arrays in this conversion can naturally lead to
-- unnecessary identity operations. Previously, `simplifyStep` was responsible
-- for cleaning up the below unnecessary operations produced by this conversion,
-- but it inadvertently changed the behavior of legitimate input designs.
--
-- Rather than applying these specific simplifications to all expressions, they
-- are now only considered when constructing a new binary operation expression
-- as part of array flattening.
binOp :: BinOp -> Expr -> Expr -> Expr
binOp Add (RawNum 0) e = e
binOp Add e (RawNum 0) = e
binOp Mul (RawNum 1) e = e
binOp Mul e (RawNum 1) = e
binOp op a b = BinOp op a b
