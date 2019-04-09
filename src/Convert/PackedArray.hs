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
 - not even be the same direction! Because of this, we have to flip arround
 - the indices of certain accesses.
 -
 - TODO: Name conflicts between functions/tasks and the description that
 - contains them likely breaks this conversion.
 -}

module Convert.PackedArray (convert) where

import Control.Monad.State
import Data.Tuple (swap)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type DimMap = Map.Map Identifier (Type, Range)

data Info = Info
    { sTypeDims :: DimMap
    } deriving Show

convert :: AST -> AST
convert = traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ (Part _ _ _ _ _ _)) =
    traverseModuleItems (flattenModuleItem info . rewriteModuleItem info) description
    where
        -- collect all possible information info our Info structure
        info =
            execState (collectModuleItemsM collectDecl description) $
            execState (collectModuleItemsM collectTF   description) $
            (Info Map.empty)
convertDescription description = description

-- collects port direction and packed-array dimension info into the state
collectDecl :: ModuleItem -> State Info ()
collectDecl (MIDecl (Variable _ t ident _ _)) = do
    let (tf, rs) = typeRanges t
    if not (typeIsImplicit t) && length rs > 1
        then
            let dets = (tf $ tail rs, head rs) in
            modify $ \s -> s { sTypeDims = Map.insert ident dets (sTypeDims s) }
        else return ()
collectDecl _ = return ()

-- collects task and function info into the state
collectTF :: ModuleItem -> State Info ()
collectTF (MIPackageItem (Function _ t x decls _)) = do
    collectDecl (MIDecl $ Variable Local t x [] Nothing)
    _ <- mapM collectDecl $ map MIDecl decls
    return ()
collectTF (MIPackageItem (Task     _   _ decls _)) = do
    _ <- mapM collectDecl $ map MIDecl decls
    return ()
collectTF _ = return ()

-- rewrite a module item if it contains a declaration to flatten
flattenModuleItem :: Info -> ModuleItem -> ModuleItem
flattenModuleItem info (MIPackageItem (Function ml t x decls stmts)) =
    MIPackageItem $ Function ml t' x decls' stmts
    where
        MIPackageItem (Task _ _ decls' _) =
            flattenModuleItem info $ MIPackageItem $ Task ml x decls stmts
        MIDecl (Variable Local t' _ [] Nothing) =
            flattenModuleItem info $ MIDecl (Variable Local t x [] Nothing)
flattenModuleItem info (MIPackageItem (Task     ml   x decls stmts)) =
    MIPackageItem $ Task ml x decls' stmts
    where
        decls' = map mapDecl decls
        mapDecl :: Decl -> Decl
        mapDecl decl = decl'
            where MIDecl decl' = flattenModuleItem info $ MIDecl decl
flattenModuleItem info (origDecl @ (MIDecl (Variable dir t ident a me))) =
    if Map.notMember ident typeDims
        then origDecl
        else flatDecl
    where
        Info typeDims = info
        (tf, rs) = typeRanges t
        flatDecl = MIDecl $ Variable dir (tf $ flattenRanges rs) ident a me
flattenModuleItem _ other = other

typeIsImplicit :: Type -> Bool
typeIsImplicit (Implicit _ _) = True
typeIsImplicit _ = False

-- combines (flattens) the bottom two ranges in the given list of ranges
flattenRanges :: [Range] -> [Range]
flattenRanges rs =
    if length rs >= 2
        then rs'
        else error $ "flattenRanges on too small list: " ++ (show rs)
    where
        r1 = head rs
        r2 = head $ tail rs
        rYY = flattenRangesHelp r1 r2
        rYN = flattenRangesHelp r1 (swap r2)
        rNY = flattenRangesHelp (swap r1) r2
        rNN = flattenRangesHelp (swap r1) (swap r2)
        rY = endianCondRange r2 rYY rYN
        rN = endianCondRange r2 rNY rNN
        r = endianCondRange r1 rY rN
        rs' = (tail $ tail rs) ++ [r]

flattenRangesHelp :: Range -> Range -> Range
flattenRangesHelp (s1, e1) (s2, e2) =
    (simplify upper, simplify lower)
    where
        size1 = rangeSize (s1, e1)
        size2 = rangeSize (s2, e2)
        lower = BinOp Add e2 (BinOp Mul e1 size2)
        upper = BinOp Add (BinOp Mul size1 size2) (BinOp Sub lower (Number "1"))

rewriteModuleItem :: Info -> ModuleItem -> ModuleItem
rewriteModuleItem info =
    traverseLHSs  (traverseNestedLHSs  rewriteLHS ) .
    traverseExprs (traverseNestedExprs rewriteExpr)
    where
        Info typeDims = info

        dims :: Identifier -> (Range, Range)
        dims x =
            (dimInner, dimOuter)
            where
                (t, r) = typeDims Map.! x
                dimInner = r
                dimOuter = head $ snd $ typeRanges t

        orientIdx :: Range -> Expr -> Expr
        orientIdx r e =
            endianCondExpr r e eSwapped
            where
                eSwapped = BinOp Sub (snd r) (BinOp Sub e (fst r))

        rewriteExpr :: Expr -> Expr
        rewriteExpr (Ident x) =
            if head x == ':'
                then Ident $ tail x
                else Ident x
        rewriteExpr (orig @ (Bit (Bit (Ident x) idxInner) idxOuter)) =
            if Map.member x typeDims
                then Bit (Ident x') idx'
                else orig
            where
                (dimInner, dimOuter) = dims x
                x' = ':' : x
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
                x' = ':' : x
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
                x' = ':' : x
                mode' = mode
                size = rangeSize dimOuter
                range' =
                    case mode of
                        NonIndexed   ->
                            (simplify hi, simplify lo)
                            where
                                lo = BinOp Mul size (snd range)
                                hi = BinOp Sub (BinOp Add lo (BinOp Mul (rangeSize range) size)) (Number "1")
                        IndexedPlus  -> (BinOp Mul size (fst range), BinOp Mul size (snd range))
                        IndexedMinus -> (BinOp Mul size (fst range), BinOp Mul size (snd range))
        ---- TODO: I'm not sure how these should be handled yet.
        ----rewriteExpr (orig @ (Range (Bit (Ident x) idxInner) modeOuter rangeOuter)) =
        ----    if Map.member x typeDims
        ----        then Range (Ident x') mode' range'
        ----        else orig
        ----    where
        ----        (dimInner, dimOuter) = dims x
        ----        x' = ':' : x
        ----        mode' =
        ----        range' =
        rewriteExpr other = other

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
