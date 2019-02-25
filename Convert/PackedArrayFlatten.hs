{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for flattening multi-dimensional packed arrays
 -
 - To simplify the code, this only removes one dimension per identifier at a
 - time. Because the conversions are repeatedly applied until convergence, this
 - will eventually remove all the extra packed dimensions.
 -
 - TODO FIXME XXX: We don't actually have support for more than 2 dimensions
 - right now. I don't think the parser can even handle that. This isn't
 - something that should be too common, so maybe we can hold off on that for
 - now.
 -
 - TODO FIXME XXX: This does not yet identify and flatten candidates that are
 - themselves contained inside of generate blocks.
 -
 - TODO FIXME XXX: This actually assumes that the first range index is the upper
 - bound. We could get arround this with a generate block.
 -}

module Convert.PackedArrayFlatten (convert) where

-- Note that, for now, only wire/reg/logic/alias can have multiple packed
-- dimensions. This means all such transformations are for module items, though
-- we must of course change uses of these items in non-constant expressions,
-- which we, unfortunately, do not distinguish from constant expressions in the
-- AST.

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.SystemVerilog.AST

type DimMap = Map.Map Identifier (Type, Range)

convert :: AST -> AST
convert = map convertDescription

convertDescription :: Description -> Description
convertDescription (Module name ports items) =
    -- Insert the new items right after the Variable for the item to preserve
    -- declaration order, which some toolchains care about.
    Module name ports $ concat $ map addUnflattener items'
    where
        toFlatten = mapMaybe getExtraDims items
        dimMap = Map.fromList toFlatten
        items' = map (convertModuleItem dimMap) items
        outputs = Set.fromList $ mapMaybe getOutput items
        getOutput :: ModuleItem -> Maybe Identifier
        getOutput (MIDecl (Variable Output _ ident _ _)) = Just ident
        getOutput _ = Nothing
        getExtraDims :: ModuleItem -> Maybe (Identifier, (Type, Range))
        getExtraDims (MIDecl (Variable _ t ident _ _)) =
            if length rs > 1
                then Just (ident, (tf $ tail rs, head rs))
                else Nothing
            where (tf, rs) = typeDims t
        getExtraDims _ = Nothing
        addUnflattener :: ModuleItem -> [ModuleItem]
        addUnflattener (orig @ (MIDecl (Variable _ _ ident _ _))) =
            orig :
            case Map.lookup ident dimMap of
                Nothing -> []
                Just desc -> unflattener outputs (ident, desc)
        addUnflattener other = [other]
convertDescription other = other

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

unflattener :: Set.Set Identifier -> (Identifier, (Type, Range)) -> [ModuleItem]
unflattener outputs (arr, (t, (majorHi, majorLo))) =
    [ Comment $ "sv2v packed-array-flatten unflattener for " ++ arr
    , MIDecl $ Variable Local t arrUnflat [(majorHi, majorLo)] Nothing
    , Generate
        [ GenModuleItem $ Genvar index
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
                if Set.notMember arr outputs
                    then (LHSBit arrUnflat $ Ident index, IdentRange arr origRange)
                    else (LHSRange arr origRange, IdentBit arrUnflat $ Ident index)
            ]
        ]
    ]
    where
        startBit = prefix "_tmp_start"
        arrUnflat = prefix arr
        index = prefix "_tmp_index"
        (minorHi, minorLo) = head $ snd $ typeDims t
        size = simplify $ BinOp Add (BinOp Sub minorHi minorLo) (Number "1")
        localparam :: Identifier -> Expr -> GenItem
        localparam x v = GenModuleItem $ MIDecl $ Localparam (Implicit []) x v
        origRange = ( (BinOp Add (Ident startBit)
                        (BinOp Sub size (Number "1")))
                    , Ident startBit )

typeDims :: Type -> ([Range] -> Type, [Range])
typeDims (Reg      r) = (Reg     , r)
typeDims (Wire     r) = (Wire    , r)
typeDims (Logic    r) = (Logic   , r)
typeDims (Alias  t r) = (Alias  t, r)
typeDims (Implicit r) = (Implicit, r)
typeDims (IntegerT  ) = (error "ranges cannot be applied to IntegerT", [])
typeDims (Enum t v r) = (Enum t v, r)

prefix :: Identifier -> Identifier
prefix ident = "_sv2v_" ++ ident

rewriteRange :: DimMap -> Range -> Range
rewriteRange dimMap (a, b) = (r a, r b)
    where r = rewriteExpr dimMap

rewriteIdentifier :: DimMap -> Identifier -> Identifier
rewriteIdentifier dimMap x =
    if Map.member x dimMap
        then prefix x
        else x

rewriteExpr :: DimMap -> Expr -> Expr
rewriteExpr dimMap = rewriteExpr'
    where
        ri :: Identifier -> Identifier
        ri = rewriteIdentifier dimMap
        re = rewriteExpr'
        rewriteExpr' :: Expr -> Expr
        rewriteExpr' (String     s) = String    s
        rewriteExpr' (Number     s) = Number    s
        rewriteExpr' (ConstBool  b) = ConstBool b
        rewriteExpr' (Ident      i  ) = Ident      (ri i)
        rewriteExpr' (IdentRange i (r @ (s, e))) =
            case Map.lookup i dimMap of
                Nothing -> IdentRange (ri i) (rewriteRange dimMap r)
                Just (t, _) ->
                    IdentRange i (s', e')
                    where
                        (a, b) = head $ snd $ typeDims t
                        size = BinOp Add (BinOp Sub a b) (Number "1")
                        s' = BinOp Sub (BinOp Mul size (BinOp Add s (Number "1"))) (Number "1")
                        e' = BinOp Mul size e
        rewriteExpr' (IdentBit   i e) = IdentBit   (ri i) (re e)
        rewriteExpr' (Repeat     e l) = Repeat (re e) (map re l)
        rewriteExpr' (Concat     l  ) = Concat (map re l)
        rewriteExpr' (Call       f l) = Call f (map re l)
        rewriteExpr' (UniOp      o e) = UniOp o (re e)
        rewriteExpr' (BinOp      o e1 e2) = BinOp o (re e1) (re e2)
        rewriteExpr' (Mux        e1 e2 e3) = Mux (re e1) (re e2) (re e3)
        rewriteExpr' (Bit        e n) = Bit (re e) n
        rewriteExpr' (Cast       t e) = Cast t (re e)

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

rewriteLHS :: DimMap -> LHS -> LHS
rewriteLHS dimMap (LHS      x  ) = LHS      (rewriteIdentifier dimMap x)
rewriteLHS dimMap (LHSBit   x e) = LHSBit   (rewriteIdentifier dimMap x) (rewriteExpr  dimMap e)
rewriteLHS dimMap (LHSRange x r) = LHSRange (rewriteIdentifier dimMap x) (rewriteRange dimMap r)
rewriteLHS dimMap (LHSConcat ls) = LHSConcat $ map (rewriteLHS dimMap) ls

rewriteStmt :: DimMap -> Stmt -> Stmt
rewriteStmt dimMap orig = rs orig
    where
        rs :: Stmt -> Stmt
        rs (Block decls stmts) = Block decls (map rs stmts)
        rs (Case kw e cases def) = Case kw e' cases' def'
            where
                re :: Expr -> Expr
                re = rewriteExpr dimMap
                rc :: Case -> Case
                rc (exprs, stmt) = (map re exprs, rs stmt)
                e' = re e
                cases' = map rc cases
                def' = fmap rs def
        rs (AsgnBlk lhs expr) = convertAssignment AsgnBlk lhs expr
        rs (Asgn    lhs expr) = convertAssignment Asgn    lhs expr
        rs (For (x1, e1) cc (x2, e2) stmt) = For (x1, e1') cc' (x2, e2') (rs stmt)
            where
                e1' = rewriteExpr dimMap e1
                e2' = rewriteExpr dimMap e2
                cc' = rewriteExpr dimMap cc
        rs (If cc s1 s2) = If (rewriteExpr dimMap cc) (rs s1) (rs s2)
        rs (Timing sense stmt) = Timing sense (rs stmt)
        rs (Null) = Null
        convertAssignment :: (LHS -> Expr -> Stmt) -> LHS -> Expr -> Stmt
        convertAssignment constructor (lhs @ (LHS ident)) (expr @ (Repeat _ exprs)) =
            case Map.lookup ident dimMap of
                Nothing -> constructor (rewriteLHS dimMap lhs) (rewriteExpr dimMap expr)
                Just (_, (a, b)) ->
                    For inir chkr incr assign
                    where
                        index = prefix $ ident ++ "_repeater_index"
                        assign = constructor
                            (LHSBit (prefix ident) (Ident index))
                            (Concat exprs)
                        inir = (index, b)
                        chkr = BinOp Le (Ident index) a
                        incr = (index, BinOp Add (Ident index) (Number "1"))
        convertAssignment constructor lhs expr =
            constructor (rewriteLHS dimMap lhs) (rewriteExpr dimMap expr)

convertModuleItem :: DimMap -> ModuleItem -> ModuleItem
convertModuleItem dimMap (MIDecl (Variable d t x a me)) =
    if Map.member x dimMap
        then MIDecl $ Variable d t' x a' me'
        else MIDecl $ Variable d t  x a' me'
    where
        (tf, rs) = typeDims t
        t' = tf $ flattenRanges rs
        a' = map (rewriteRange dimMap) a
        me' = fmap (rewriteExpr dimMap) me
convertModuleItem dimMap (Generate items) =
    Generate $ map (convertGenItem dimMap) items
convertModuleItem dimMap (Assign lhs expr) =
    Assign (rewriteLHS dimMap lhs) (rewriteExpr dimMap expr)
convertModuleItem dimMap (AlwaysC kw stmt) =
    AlwaysC kw (rewriteStmt dimMap stmt)
convertModuleItem dimMap (Function ret f decls stmt) =
    Function ret f decls (rewriteStmt dimMap stmt)
convertModuleItem dimMap (Instance m params x ml) =
    Instance m params x $ fmap (map convertPortBinding) ml
    where
        convertPortBinding :: PortBinding -> PortBinding
        convertPortBinding (p, Nothing) = (p, Nothing)
        convertPortBinding (p, Just  e) = (p, Just $ rewriteExpr dimMap e)
convertModuleItem _ (Comment x) = Comment x
convertModuleItem _ (Genvar  x) = Genvar  x
convertModuleItem _ (MIDecl  x) = MIDecl  x

convertGenItem :: DimMap -> GenItem -> GenItem
convertGenItem dimMap item = convertGenItem' item
    where
        f :: ModuleItem -> ModuleItem
        f = convertModuleItem dimMap
        convertGenItem' :: GenItem -> GenItem
        convertGenItem' (GenBlock x items) = GenBlock x $ map convertGenItem' items
        convertGenItem' (GenFor a b c d items) = GenFor a b c d $ map convertGenItem' items
        convertGenItem' (GenIf e i1 i2) = GenIf e (convertGenItem' i1) (convertGenItem' i2)
        convertGenItem' (GenNull) = GenNull
        convertGenItem' (GenModuleItem moduleItem) = GenModuleItem $ f moduleItem
        convertGenItem' (GenCase e cases def) = GenCase e cases' def'
            where
                cases' = zip (map fst cases) (map (convertGenItem' . snd) cases)
                def' = fmap convertGenItem' def
