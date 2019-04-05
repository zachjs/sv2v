{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for flattening multi-dimensional packed arrays
 -
 - This removes one dimension per identifier at a time. This works fine because
 - the conversions are repeatedly applied.
 -
 - Packed arrays can be used in any of the following ways: A) as a whole,
 - including as a port; B) with an index (`foo[0]`); or C) with a range
 - (`foo[10:0]`). The rules for this conversion are:
 -    1. If used with an index, then we must have an unflattened/unpacked
 -    version of that array after the conversion, so that we may get at the
 -    packed sub-arrays.
 -    2. If used as a whole or with a range, then we must have a flattened
 -    version of that array after the conversion, so that we may get at a
 -    contiguous sequence of elements.
 -    3. If both 1 and 2 apply, then we will make a fancy generate block to
 -    derive one from the other. The derivation direction is decided based on
 -    which version, if any, is exposed directly as a port.
 -
 - Note: We don't count usages with an index in expressions as such, as those
 - usages could be equivalently converted to range accesses with some added in
 - multiplication.
 -}

module Convert.PackedArray (convert) where

import Control.Monad.State
import Data.List (partition)
import Data.Tuple (swap)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type DirMap = Map.Map Identifier Direction
type DimMap = Map.Map Identifier (Type, Range)
type IdentSet = Set.Set Identifier

data Info = Info
    { sTypeDims :: DimMap
    , sPortDirs :: DirMap
    , sIdxUses :: IdentSet
    , sSeqUses :: IdentSet }
    deriving Show

convert :: AST -> AST
convert = traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ (Part _ _ _ _ _ _)) =
    hoistPortDecls $
    traverseModuleItems (flattenModuleItem info . rewriteModuleItem info) description
    where
        -- collect all possible information info our Info structure
        rawInfo =
            execState (collectModuleItemsM (collectLHSsM   collectLHS) description) $
            execState (collectModuleItemsM (collectExprsM collectExpr) description) $
            execState (collectModuleItemsM collectDecl description) $
            execState (collectModuleItemsM collectTF   description) $
            (Info Map.empty Map.empty Set.empty Set.empty)
        relevantIdents = Map.keysSet $ sTypeDims rawInfo
        -- restrict the sets/maps to only contain keys which need transformation
        info = rawInfo
            { sPortDirs = Map.restrictKeys (sPortDirs rawInfo) relevantIdents
            , sIdxUses  = Set.intersection (sIdxUses  rawInfo) relevantIdents
            , sSeqUses  = Set.intersection (sSeqUses  rawInfo) relevantIdents }
convertDescription description = description

-- collects port direction and packed-array dimension info into the state
collectDecl :: ModuleItem -> State Info ()
collectDecl (MIDecl (Variable dir t ident _ _)) = do
    let (tf, rs) = typeRanges t
    if not (typeIsImplicit t) && length rs > 1
        then
            let dets = (tf $ tail rs, head rs) in
            modify $ \s -> s { sTypeDims = Map.insert ident dets (sTypeDims s) }
        else return ()
    if dir /= Local
        then do
            () <- recordSeqUsage ident
            modify $ \s -> s { sPortDirs = Map.insert ident dir (sPortDirs s) }
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

-- collectors for identifier usage information
recordSeqUsage :: Identifier -> State Info ()
recordSeqUsage i = modify $ \s -> s { sSeqUses = Set.insert i $ sSeqUses s }
recordIdxUsage :: Identifier -> State Info ()
recordIdxUsage i = modify $ \s -> s { sIdxUses = Set.insert i $ sIdxUses s }
collectExpr :: Expr -> State Info ()
collectExpr (Ident i) = recordSeqUsage i
collectExpr other = collectNestedExprsM collectNestedExpr other
collectNestedExpr :: Expr -> State Info ()
collectNestedExpr (Range (Ident i) _ _) = recordSeqUsage i
collectNestedExpr _ = return ()
collectLHS :: LHS -> State Info ()
collectLHS (LHSIdent i) = recordSeqUsage i
collectLHS other = collectNestedLHSsM collectNestedLHS other
collectNestedLHS :: LHS -> State Info ()
collectNestedLHS (LHSRange (LHSIdent i) _ _) = recordSeqUsage i
collectNestedLHS (LHSBit   (LHSIdent i)   _) = recordIdxUsage i
collectNestedLHS _ = return ()

-- VCS doesn't like port declarations inside of `generate` blocks, so we hoist
-- them out with this function. This obviously isn't ideal, but it's a
-- relatively straightforward transformation, and testing in VCS is important.
hoistPortDecls :: Description -> Description
hoistPortDecls (Part extern kw lifetime name ports items) =
    Part extern kw lifetime name ports (concat $ map explode items)
    where
        explode :: ModuleItem -> [ModuleItem]
        explode (Generate genItems) =
            if null rest
                then portDecls
                else portDecls ++ [Generate rest]
            where
                (wrappedPortDecls, rest) = partition isPortDecl genItems
                portDecls = map (\(GenModuleItem item) -> item) wrappedPortDecls
                isPortDecl :: GenItem -> Bool
                isPortDecl (GenModuleItem (MIDecl (Variable dir _ _ _ _))) =
                    dir /= Local
                isPortDecl _ = False
        explode other = [other]
hoistPortDecls other = other

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
    -- if it doesn't need any mapping, then skip it
    if Map.notMember ident typeDims then origDecl
    -- if it is never used as a sequence (whole or range), then move the packed
    -- dimension to the unpacked side
    else if Set.notMember ident seqUses then flipDecl
    -- if it is used as a sequence, but never indexed-into (sub-array), then
    -- flatten (combine) the ranges, leaving them packed
    else if Set.notMember ident duoUses then flatDecl
    -- if it is both used as a sequence and is indexed-into
    else
        -- if this is not the fully-typed declaration of this item, then flatten
        -- it, but don't make the `generate` block this time to avoid duplicates
        if typeIsImplicit t then flatDecl
        -- otherwise, flatten it, and also create an unflattened copy
        else Generate $ (GenModuleItem flatDecl) : genItems
    where
        Info typeDims portDirs idxUses seqUses = info
        duoUses = Set.intersection idxUses seqUses
        portDir = Map.lookup ident portDirs
        writeToFlatVariant = portDir == Just Output || portDir == Nothing
        genItems = unflattener writeToFlatVariant ident (typeDims Map.! ident)
        (tf, rs) = typeRanges t
        flipDecl = MIDecl $ Variable dir (tf $          tail rs) ident (a ++ [head rs]) me
        flatDecl = MIDecl $ Variable dir (tf $ flattenRanges rs) ident a                me
flattenModuleItem _ other = other

-- produces `generate` items for creating an unflattened copy of the given
-- flattened, packed array
unflattener :: Bool -> Identifier -> (Type, Range) -> [GenItem]
unflattener writeToFlatVariant arr (t, major @ (majorHi, majorLo)) =
        [ GenModuleItem $ MIPackageItem $ Comment $ "sv2v packed-array-flatten unflattener for " ++ arr
        , GenModuleItem $ MIDecl $ Variable Local t arrUnflat [(majorHi, majorLo)] Nothing
        , GenModuleItem $ Genvar index
        , GenModuleItem $ MIDecl $ Variable Local (IntegerAtom TInteger Unspecified) (arrUnflat ++ "_repeater_index") [] Nothing
        , GenFor
            (index, majorLo)
            (endianCondExpr major
                (BinOp Le (Ident index) majorHi)
                (BinOp Ge (Ident index) majorHi))
            (index, AsgnOp Add, endianCondExpr major (Number "1") (Number "-1"))
            (Just $ prefix "unflatten_" ++ arr)
            [ localparam startBit
                (simplify $ BinOp Add (endianCondExpr major majorLo majorHi)
                    (BinOp Mul (Ident index) size))
            , GenModuleItem $ (uncurry $ Assign Nothing) $
                if not writeToFlatVariant
                    then (LHSBit (LHSIdent arrUnflat) $ Ident index, Range (Ident arr) NonIndexed origRange)
                    else (LHSRange (LHSIdent arr) NonIndexed origRange, Bit (Ident arrUnflat) (Ident index))
            ]
        ]
    where
        startBit = prefix "_tmp_start_" ++ arr
        arrUnflat = prefix arr
        index = prefix "_tmp_index_" ++ arr
        minor = head $ snd $ typeRanges t
        size = rangeSize $ endianCondRange minor minor (swap minor)
        localparam :: Identifier -> Expr -> GenItem
        localparam x v = GenModuleItem $ MIDecl $ Localparam (Implicit Unspecified []) x v
        origRangeAg = ( (BinOp Add (Ident startBit)
                        (BinOp Sub size (Number "1")))
                      , Ident startBit )
        origRange = endianCondRange major origRangeAg (swap origRangeAg)

typeIsImplicit :: Type -> Bool
typeIsImplicit (Implicit _ _) = True
typeIsImplicit _ = False

-- prefix a string with a namespace of sorts
prefix :: Identifier -> Identifier
prefix ident = "_sv2v_" ++ ident

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
        rAg = endianCondRange r1 rY rN
        r = endianCondRange r1 rAg (swap rAg)
        rs' = (tail $ tail rs) ++ [r]

flattenRangesHelp :: Range -> Range -> Range
flattenRangesHelp (s1, e1) (s2, e2) =
    (simplify upper, simplify lower)
    where
        size1 = rangeSize (s1, e1)
        size2 = rangeSize (s2, e2)
        lower = BinOp Add e1 (BinOp Mul e2 size2)
        upper = BinOp Add (BinOp Mul size1 size2) (BinOp Sub lower (Number "1"))

rewriteModuleItem :: Info -> ModuleItem -> ModuleItem
rewriteModuleItem info =
    traverseStmts rewriteStmt .
    traverseExprs (traverseNestedExprs rewriteExpr)
    where
        Info typeDims _ idxUses seqUses = info
        duoUses = Set.intersection idxUses seqUses

        rewriteIdent :: Bool -> Identifier -> Identifier
        rewriteIdent isSeqUsage x =
            if Set.member x duoUses
                then
                    -- if an array is used both ways, then the original name is
                    -- the flattened version
                    if isSeqUsage
                        then x
                        else prefix x
                else x
        rewriteSeqIdent = rewriteIdent True
        rewriteIdxIdent = rewriteIdent False

        rewriteExpr :: Expr -> Expr
        rewriteExpr (Ident i) = Ident $ rewriteSeqIdent i
        rewriteExpr (Bit   (Ident i) e) =
            if Map.member i typeDims && Set.member i seqUses && Set.notMember i idxUses
                then Range (Ident $ rewriteSeqIdent i) NonIndexed (hi, lo)
                else Bit   (Ident $ rewriteIdxIdent i) e
            where
                r = head $ snd $ typeRanges $ fst $ typeDims Map.! i
                size = rangeSize r
                lo = simplify $ BinOp Mul e size
                hi = simplify $ BinOp Add lo (BinOp Sub size (Number "1"))
        rewriteExpr (Range (Ident i) m (r @ (s, e))) =
            if Map.member i typeDims
                then Range (Ident i) m r'
                else Range (Ident i) m r
            where
                (a, b) = head $ snd $ typeRanges $ fst $ typeDims Map.! i
                size = rangeSize (a, b)
                s' = BinOp Sub (BinOp Mul size (BinOp Add s (Number "1"))) (Number "1")
                e' = BinOp Mul size e
                r' = (simplify s', simplify e')
        rewriteExpr other = other

        rewriteLHS :: LHS -> LHS
        rewriteLHS (LHSIdent x    ) = LHSIdent (rewriteSeqIdent x)
        rewriteLHS (LHSBit (LHSIdent x) e) =
            LHSBit (LHSIdent $ rewriteIdxIdent x) e
        rewriteLHS (LHSBit   l e  ) = LHSBit   (rewriteLHS l) e
        rewriteLHS (LHSRange l m r) = LHSRange (rewriteLHS l) m r
        rewriteLHS (LHSDot   l x  ) = LHSDot   (rewriteLHS l) x
        rewriteLHS (LHSConcat ls  ) = LHSConcat $ map rewriteLHS ls

        rewriteStmt :: Stmt -> Stmt
        rewriteStmt (AsgnBlk op lhs expr) = convertAssignment (AsgnBlk op) lhs expr
        rewriteStmt (Asgn    mt lhs expr) = convertAssignment (Asgn    mt) lhs expr
        rewriteStmt other = other
        convertAssignment :: (LHS -> Expr -> Stmt) -> LHS -> Expr -> Stmt
        convertAssignment constructor (lhs @ (LHSIdent ident)) (expr @ (Repeat _ exprs)) =
            if Map.member ident typeDims
                then For inir chkr incr assign
                else constructor (rewriteLHS lhs) expr
            where
                (_, (a, b)) = typeDims Map.! ident
                index = prefix $ ident ++ "_repeater_index"
                assign = constructor
                    (LHSBit (LHSIdent $ prefix ident) (Ident index))
                    (Concat exprs)
                inir = [Right (LHSIdent index, b)]
                chkr = Just $ BinOp Le (Ident index) a
                incr = [(LHSIdent index, AsgnOp Add, Number "1")]
        convertAssignment constructor lhs expr =
            constructor (rewriteLHS lhs) expr
