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
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type DimMap = Map.Map Identifier [Range]
type IdentSet = Set.Set Identifier

data Info = Info
    { sTypeDims :: DimMap
    , sIdents :: IdentSet
    } deriving Show

defaultInfo :: Info
defaultInfo = Info Map.empty Set.empty

convert :: AST -> AST
convert = traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ (Part _ _ _ _ _ _)) =
    traverseModuleItems (convertModuleItem info) description
    where
        collector = collectModuleItemsM $ collectDeclsM' ExcludeTFs collectDecl
        info = execState (collector description) defaultInfo
convertDescription description = description

-- collects packed-array dimension and variable existing info into the state
collectDecl :: Decl -> State Info ()
collectDecl (Variable _ t ident _ _) = do
    Info typeDims idents <- get
    let (_, rs) = typeRanges t
    let typeDims' =
            if not (isImplicit t) && length rs > 1
            then Map.insert ident rs typeDims
            else typeDims
    let idents' =
            if not (isImplicit t)
            then
                if Set.member ident idents
                then error $ "unsupported complex shadowing of " ++ show ident
                else Set.insert ident idents
            else idents
    put $ Info typeDims' idents'
    where
        isImplicit :: Type -> Bool
        isImplicit (Implicit _ _) = True
        isImplicit _ = False
collectDecl _ = return ()

-- shadows the latter info with the former
combineInfo :: Info -> Info -> Info
combineInfo local global =
    Info typeDims idents
    where
        Info globalTypeDims globalIdents = global
        Info localTypeDims  localIdents  = local
        idents = Set.union globalIdents localIdents
        typeDims = Map.union localTypeDims $
            Map.withoutKeys globalTypeDims localIdents

-- Convert the multi-dimensional packed arrays within the given module item.
-- This function must ensure that function/task level shadowing is respected.
convertModuleItem :: Info -> ModuleItem -> ModuleItem
convertModuleItem globalInfo (orig @ (MIPackageItem (Function ml t x decls stmts))) =
    rewrite info $
    MIPackageItem $ Function ml t' x decls stmts
    where
        localInfo =
            execState (collectDecl $ Variable Local t x [] Nothing) $
            execState (collectDeclsM collectDecl orig) $
            defaultInfo
        info = combineInfo localInfo globalInfo
        -- rewrite the return type of this function
        Variable Local t' _ [] Nothing =
            flattenDecl info $ Variable Local t x [] Nothing
convertModuleItem globalInfo (orig @ (MIPackageItem (Task     ml   x decls stmts))) =
    rewrite info $
    MIPackageItem $ Task ml x decls stmts
    where
        localInfo =
            execState (collectDeclsM collectDecl orig) $
            defaultInfo
        info = combineInfo localInfo globalInfo
convertModuleItem info other =
    rewrite info other

-- combine the leading two packed ranges of a declaration
flattenDecl :: Info -> Decl -> Decl
flattenDecl info (origDecl @ (Variable dir t ident a me)) =
    if Map.notMember ident typeDims
        then origDecl
        else flatDecl
    where
        typeDims = sTypeDims info
        (tf, rs) = typeRanges t
        r1 : r2 : rest = rs
        rs' = (combineRanges r1 r2) : rest
        flatDecl = Variable dir (tf rs') ident a me
flattenDecl _ other = other

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

-- rewrite the declarations, expressions, and lvals in a module item to remove
-- the packed array dimensions captured in the given info
rewrite :: Info -> ModuleItem -> ModuleItem
rewrite info =
    traverseDecls (flattenDecl info) .
    traverseLHSs  (traverseNestedLHSs  rewriteLHS ) .
    traverseExprs (traverseNestedExprs rewriteExpr)
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

        -- LHSs need to be converted too. Rather than duplicating the
        -- procedures, we turn the relevant LHSs into expressions temporarily
        -- and use the expression conversion written above.
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
