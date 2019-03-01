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
 - TODO FIXME XXX: The Parser/AST don't yet support indexing into an identifier
 - twice, or indexing into an identifier, and then selecting a range.
 -
 - TODO: This assumes that the first range index is the upper bound. We could
 - probably get around this with some cleverness in the generate block. I don't
 - think it's urgent to have support for "backwards" ranges.
 -}

module Convert.PackedArray (convert) where

import Control.Monad.State
import Data.List (partition)
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
convertDescription (description @ (Module _ ports _)) =
    hoistPortDecls $
    traverseModuleItems (flattenModuleItem info . rewriteModuleItem info) description
    where
        -- collect all possible information info our Info structure
        rawInfo =
            execState (collectModuleItemsM (collectLHSsM   collectLHS) description) $
            execState (collectModuleItemsM (collectExprsM collectExpr) description) $
            execState (collectModuleItemsM collectDecl description) $
            (Info Map.empty Map.empty Set.empty (Set.fromList ports))
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
        then modify $ \s -> s { sPortDirs = Map.insert ident dir (sPortDirs s) }
        else return ()
collectDecl _ = return ()

-- collectors for identifier usage information
recordSeqUsage :: Identifier -> State Info ()
recordSeqUsage i = modify $ \s -> s { sSeqUses = Set.insert i $ sSeqUses s }
recordIdxUsage :: Identifier -> State Info ()
recordIdxUsage i = modify $ \s -> s { sIdxUses = Set.insert i $ sIdxUses s }
collectExpr :: Expr -> State Info ()
collectExpr (Ident      i  ) = recordSeqUsage i
collectExpr (IdentRange i _) = recordSeqUsage i
collectExpr (IdentBit   i _) = recordIdxUsage i
collectExpr _ = return ()
collectLHS :: LHS -> State Info ()
collectLHS (LHS       i  ) = recordSeqUsage i
collectLHS (LHSRange  i _) = recordSeqUsage i
collectLHS (LHSBit    i _) = recordIdxUsage i
collectLHS (LHSConcat lhss) = mapM collectLHS lhss >>= \_ -> return ()

-- VCS doesn't like port declarations inside of `generate` blocks, so we hoist
-- them out with this function. This obviously isn't ideal, but it's a
-- relatively straightforward transformation, and testing in VCS is important.
hoistPortDecls :: Description -> Description
hoistPortDecls (Module name ports items) =
    Module name ports (concat $ map explode items)
    where
        explode :: ModuleItem -> [ModuleItem]
        explode (Generate genItems) =
            portDecls ++ [Generate rest]
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
        writeToFlatVariant = Map.lookup ident portDirs == Just Output
        genItems = unflattener writeToFlatVariant ident (typeDims Map.! ident)
        (tf, rs) = typeRanges t
        flipDecl = MIDecl $ Variable dir (tf $          tail rs) ident (a ++ [head rs]) me
        flatDecl = MIDecl $ Variable dir (tf $ flattenRanges rs) ident a                me
flattenModuleItem _ other = other

-- produces `generate` items for creating an unflattened copy of the given
-- flattened, packed array
unflattener :: Bool -> Identifier -> (Type, Range) -> [GenItem]
unflattener writeToFlatVariant arr (t, (majorHi, majorLo)) =
        [ GenModuleItem $ Comment $ "sv2v packed-array-flatten unflattener for " ++ arr
        , GenModuleItem $ MIDecl $ Variable Local t arrUnflat [(majorHi, majorLo)] Nothing
        , GenModuleItem $ Genvar index
        , GenModuleItem $ MIDecl $ Variable Local IntegerT (arrUnflat ++ "_repeater_index") [] Nothing
        , GenFor
            (index, majorLo)
            (BinOp Le (Ident index) majorHi)
            (index, BinOp Add (Ident index) (Number "1"))
            (prefix "unflatten")
            [ localparam startBit
                (simplify $ BinOp Add majorLo
                    (BinOp Mul (Ident index) size))
            , GenModuleItem $ (uncurry Assign) $
                if not writeToFlatVariant
                    then (LHSBit arrUnflat $ Ident index, IdentRange arr origRange)
                    else (LHSRange arr origRange, IdentBit arrUnflat $ Ident index)
            ]
        ]
    where
        startBit = prefix "_tmp_start"
        arrUnflat = prefix arr
        index = prefix "_tmp_index"
        (minorHi, minorLo) = head $ snd $ typeRanges t
        size = simplify $ BinOp Add (BinOp Sub minorHi minorLo) (Number "1")
        localparam :: Identifier -> Expr -> GenItem
        localparam x v = GenModuleItem $ MIDecl $ Localparam (Implicit []) x v
        origRange = ( (BinOp Add (Ident startBit)
                        (BinOp Sub size (Number "1")))
                    , Ident startBit )

typeIsImplicit :: Type -> Bool
typeIsImplicit (Implicit _) = True
typeIsImplicit _ = False

-- basic expression simplfication utility to help us generate nicer code in the
-- common case of ranges like `[FOO-1:0]`
simplify :: Expr -> Expr
simplify (BinOp op e1 e2) =
    case (op, e1', e2') of
        (Add, Number "0", e) -> e
        (Add, e, Number "0") -> e
        (Sub, e, Number "0") -> e
        (Add, BinOp Sub e (Number "1"), Number "1") -> e
        (Add, e, BinOp Sub (Number "0") (Number "1")) -> BinOp Sub e (Number "1")
        _ -> BinOp op e1' e2'
    where
        e1' = simplify e1
        e2' = simplify e2
simplify other = other

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
        (s1, e1) = head rs
        (s2, e2) = head $ tail rs
        size1 = BinOp Add (BinOp Sub s1 e1) (Number "1")
        size2 = BinOp Add (BinOp Sub s2 e2) (Number "1")
        upper = BinOp Add (BinOp Mul size1 size2) (BinOp Sub e1 (Number "1"))
        r' = (simplify upper, e1)
        rs' = (tail $ tail rs) ++ [r']

rewriteModuleItem :: Info -> ModuleItem -> ModuleItem
rewriteModuleItem info =
    traverseStmts rewriteStmt .
    traverseExprs rewriteExpr
    where
        Info typeDims portDirs idxUses seqUses = info
        duoUses = Set.intersection idxUses seqUses

        rewriteIdent :: Bool -> Identifier -> Identifier
        rewriteIdent isAsgn x =
            if isDuod && (isOutputPort == isAsgn)
                then prefix x
                else x
            where
                isDuod = Set.member x duoUses
                isOutputPort = Map.lookup x portDirs == Just Output
        rewriteReadIdent = rewriteIdent False
        rewriteAsgnIdent = rewriteIdent True

        rewriteExpr :: Expr -> Expr
        rewriteExpr (Ident      i)   = Ident    (rewriteReadIdent i)
        rewriteExpr (IdentBit   i e) = IdentBit (rewriteReadIdent i) e
        rewriteExpr (IdentRange i (r @ (s, e))) =
            if Map.member i typeDims
                then IdentRange i r'
                else IdentRange i r
            where
                (a, b) = head $ snd $ typeRanges $ fst $ typeDims Map.! i
                size = BinOp Add (BinOp Sub a b) (Number "1")
                s' = BinOp Sub (BinOp Mul size (BinOp Add s (Number "1"))) (Number "1")
                e' = BinOp Mul size e
                r' = (simplify s', simplify e')
        rewriteExpr other = other

        rewriteLHS :: LHS -> LHS
        rewriteLHS (LHS      x  ) = LHS      (rewriteAsgnIdent x)
        rewriteLHS (LHSBit   x e) = LHSBit   (rewriteAsgnIdent x) e
        rewriteLHS (LHSRange x r) = LHSRange (rewriteAsgnIdent x) r
        rewriteLHS (LHSConcat ls) = LHSConcat $ map rewriteLHS ls

        rewriteStmt :: Stmt -> Stmt
        rewriteStmt (AsgnBlk lhs expr) = convertAssignment AsgnBlk lhs expr
        rewriteStmt (Asgn    lhs expr) = convertAssignment Asgn    lhs expr
        rewriteStmt other = other
        convertAssignment :: (LHS -> Expr -> Stmt) -> LHS -> Expr -> Stmt
        convertAssignment constructor (lhs @ (LHS ident)) (expr @ (Repeat _ exprs)) =
            if Map.member ident typeDims
                then For inir chkr incr assign
                else constructor (rewriteLHS lhs) expr
            where
                (_, (a, b)) = typeDims Map.! ident
                index = prefix $ ident ++ "_repeater_index"
                assign = constructor
                    (LHSBit (prefix ident) (Ident index))
                    (Concat exprs)
                inir = (index, b)
                chkr = BinOp Le (Ident index) a
                incr = (index, BinOp Add (Ident index) (Number "1"))
        convertAssignment constructor lhs expr =
            constructor (rewriteLHS lhs) expr
