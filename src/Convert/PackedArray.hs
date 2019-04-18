{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for flattening multi-dimensional packed arrays
 -
 - This removes one dimension per identifier at a time. This works fine because
 - the conversions are repeatedly applied.
 -
 - We previously had a very complex conversion which used `generate` to make
 - flattened and unflattened versions of the array as necessary. This has now
 - been "simplified" to always flatten the array, and then rewrite all usages of
 - the array as appropriate.
 -
 - Note that the ranges being combined may not be of the form [hi:lo], and need
 - not even be the same direction! Because of this, we have to flip around the
 - indices of certain accesses.
 -}

module Convert.PackedArray (convert) where

import Control.Monad.State
import Data.Tuple (swap)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type DimMap = Map.Map Identifier [Range]

data Info = Info
    { sTypeDims :: DimMap
    } deriving (Eq, Show)

convert :: AST -> AST
convert = traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ (Part _ _ _ _ _ _)) =
    evalState
        (initialTraverse description >>= scopedTraverse)
        (Info Map.empty)
    where
        initialTraverse = traverseModuleItemsM traverseMIDecl
        scopedTraverse = traverseModuleItemsM $
            traverseScopesM traverseDeclM traverseModuleItemM traverseStmtM
        traverseMIDecl :: ModuleItem -> State Info ModuleItem
        traverseMIDecl (MIDecl decl) =
            traverseDeclM decl >>= return . MIDecl
        traverseMIDecl other = return other
convertDescription description = description

-- collects and converts multi-dimensional packed-array declarations
traverseDeclM :: Decl -> State Info Decl
traverseDeclM (origDecl @ (Variable dir t ident a me)) = do
    Info typeDims <- get
    let (tf, rs) = typeRanges t
    if length rs <= 1
        then do
            put $ Info $ Map.delete ident typeDims
            return origDecl
        else do
            put $ Info $ Map.insert ident rs typeDims
            let r1 : r2 : rest = rs
            let rs' = (combineRanges r1 r2) : rest
            return $ Variable dir (tf rs') ident a me
traverseDeclM other = return other

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
                size1 = rangeSize (s1, e1)
                size2 = rangeSize (s2, e2)
                lower = BinOp Add e2 (BinOp Mul e1 size2)
                upper = BinOp Add (BinOp Mul size1 size2)
                            (BinOp Sub lower (Number "1"))

traverseModuleItemM :: ModuleItem -> State Info ModuleItem
traverseModuleItemM item =
    traverseLHSsM  traverseLHSM  item >>=
    traverseExprsM traverseExprM

traverseStmtM :: Stmt -> State Info Stmt
traverseStmtM stmt =
    traverseStmtLHSsM  traverseLHSM  stmt >>=
    traverseStmtExprsM traverseExprM

traverseExprM :: Expr -> State Info Expr
traverseExprM = traverseNestedExprsM $ stately traverseExpr

traverseLHSM :: LHS -> State Info LHS
traverseLHSM = traverseNestedLHSsM $ stately traverseLHS

traverseExpr :: Info -> Expr -> Expr
traverseExpr info =
    rewriteExpr
    where
        typeDims = sTypeDims info

        dims :: Identifier -> (Range, Range)
        dims x =
            (dimInner, dimOuter)
            where
                dimInner : dimOuter : _ = typeDims Map.! x

        -- if the given range is flipped, the result will flip around the given
        -- indexing expression
        orientIdx :: Range -> Expr -> Expr
        orientIdx r e =
            endianCondExpr r e eSwapped
            where
                eSwapped = BinOp Sub (snd r) (BinOp Sub e (fst r))

        -- Converted idents are prefixed with an invalid character to ensure
        -- that are not converted twice when the traversal steps downward. When
        -- the prefixed identifier is encountered at the lowest level, it is
        -- removed.

        tag = ':'

        rewriteExpr :: Expr -> Expr
        rewriteExpr (Ident x) =
            if head x == tag
                then Ident $ tail x
                else Ident x
        rewriteExpr (orig @ (Bit (Bit (Ident x) idxInner) idxOuter)) =
            if Map.member x typeDims
                then Bit (Ident x') idx'
                else orig
            where
                (dimInner, dimOuter) = dims x
                x' = tag : x
                idxInner' = orientIdx dimInner idxInner
                idxOuter' = orientIdx dimOuter idxOuter
                base = BinOp Mul idxInner' (rangeSize dimOuter)
                idx' = simplify $ BinOp Add base idxOuter'
        rewriteExpr (orig @ (Bit (Ident x) idx)) =
            if Map.member x typeDims
                then Range (Ident x') mode' range'
                else orig
            where
                (dimInner, dimOuter) = dims x
                x' = tag : x
                mode' = IndexedPlus
                idx' = orientIdx dimInner idx
                len = rangeSize dimOuter
                base = BinOp Add (endianCondExpr dimOuter (snd dimOuter) (fst dimOuter)) (BinOp Mul idx' len)
                range' = (simplify base, simplify len)
        rewriteExpr (orig @ (Range (Ident x) mode range)) =
            if Map.member x typeDims
                then Range (Ident x') mode' range'
                else orig
            where
                (_, dimOuter) = dims x
                x' = tag : x
                mode' = mode
                size = rangeSize dimOuter
                base = endianCondExpr dimOuter (snd dimOuter) (fst dimOuter)
                range' =
                    case mode of
                        NonIndexed   ->
                            (simplify hi, simplify lo)
                            where
                                lo = BinOp Mul size (snd range)
                                hi = BinOp Sub (BinOp Add lo (BinOp Mul (rangeSize range) size)) (Number "1")
                        IndexedPlus  -> (BinOp Add (BinOp Mul size (fst range)) base, BinOp Mul size (snd range))
                        IndexedMinus -> (BinOp Add (BinOp Mul size (fst range)) base, BinOp Mul size (snd range))
        rewriteExpr (orig @ (Range (Bit (Ident x) idxInner) modeOuter rangeOuter)) =
            if Map.member x typeDims
                then Range (Ident x') mode' range'
                else orig
            where
                (dimInner, dimOuter) = dims x
                x' = tag : x
                mode' = IndexedPlus
                idxInner' = orientIdx dimInner idxInner
                rangeOuterReverseIndexed =
                    (BinOp Add (fst rangeOuter) (BinOp Sub (snd rangeOuter)
                    (Number "1")), snd rangeOuter)
                (baseOuter, lenOuter) =
                    case modeOuter of
                        IndexedPlus ->
                            endianCondRange dimOuter rangeOuter rangeOuterReverseIndexed
                        IndexedMinus ->
                            endianCondRange dimOuter rangeOuterReverseIndexed rangeOuter
                        NonIndexed ->
                            (endianCondExpr dimOuter (snd rangeOuter) (fst rangeOuter), rangeSize rangeOuter)
                idxOuter' = orientIdx dimOuter baseOuter
                start = BinOp Mul idxInner' (rangeSize dimOuter)
                base = simplify $ BinOp Add start idxOuter'
                len = lenOuter
                range' = (base, len)
        rewriteExpr other = other

-- LHSs need to be converted too. Rather than duplicating the procedures, we
-- turn the relevant LHSs into expressions temporarily and use the expression
-- conversion written above.
traverseLHS :: Info -> LHS -> LHS
traverseLHS info =
    rewriteLHS
    where
        typeDims = sTypeDims info
        rewriteExpr = traverseExpr info

        rewriteLHS :: LHS -> LHS
        rewriteLHS (LHSIdent x) =
            LHSIdent x'
            where Ident x' = rewriteExpr (Ident x)
        rewriteLHS (orig @ (LHSBit (LHSBit (LHSIdent x) idxInner) idxOuter)) =
            if Map.member x typeDims
                then LHSBit (LHSIdent x') idx'
                else orig
            where Bit (Ident x') idx' =
                    rewriteExpr (Bit (Bit (Ident x) idxInner) idxOuter)
        rewriteLHS (orig @ (LHSBit (LHSRange (LHSIdent x) modeInner rangeInner) idxOuter)) =
            if Map.member x typeDims
                then LHSRange (LHSIdent x') mode' range'
                else orig
            where Range (Ident x') mode' range' =
                    rewriteExpr (Bit (Range (Ident x) modeInner rangeInner) idxOuter)
        rewriteLHS (orig @ (LHSBit (LHSIdent x) idx)) =
            if Map.member x typeDims
                then LHSRange (LHSIdent x') mode' range'
                else orig
            where Range (Ident x') mode' range' = rewriteExpr (Bit (Ident x) idx)
        rewriteLHS (orig @ (LHSRange (LHSIdent x) mode range)) =
            if Map.member x typeDims
                then LHSRange (LHSIdent x') mode' range'
                else orig
            where Range (Ident x') mode' range' =
                    rewriteExpr (Range (Ident x) mode range)
        rewriteLHS (orig @ (LHSRange (LHSBit (LHSIdent x) idxInner) modeOuter rangeOuter)) =
            if Map.member x typeDims
                then LHSRange (LHSIdent x') mode' range'
                else orig
            where Range (Ident x') mode' range' =
                    rewriteExpr (Range (Bit (Ident x) idxInner) modeOuter rangeOuter)
        rewriteLHS other = other
