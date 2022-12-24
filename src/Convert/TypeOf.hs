{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for the `type` operator
 -
 - This conversion is responsible for explicit type resolution throughout sv2v.
 - It uses Scoper to resolve hierarchical expressions in a scope-aware manner.
 -
 - Some other conversions, such as the dimension query and streaming
 - concatenation conversions, defer the resolution of type information to this
 - conversion pass by producing nodes with the `type` operator during
 - elaboration.
 -
 - This conversion also elaborates sign and size casts to their primitive types.
 - Sign casts take on the size of the underlying expression. Size casts take on
 - the sign of the underlying expression. This conversion incorporates this
 - elaboration as the canonical source for type information. It also enables the
 - removal of unnecessary casts often resulting from struct literals or casts in
 - the source intended to appease certain lint rules.
 -}

module Convert.TypeOf (convert) where

import Control.Monad.State.Strict
import Data.Tuple (swap)

import Convert.ExprUtils (dimensionsSize, endianCondRange, simplify)
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ partScoper
    traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

-- single bit 4-state `logic` type
pattern UnitType :: Type
pattern UnitType = IntegerVector TLogic Unspecified []

type ST = Scoper Type

-- insert the given declaration into the scope, and convert an TypeOfs within
traverseDeclM :: Decl -> ST Decl
traverseDeclM decl@Net{} =
    traverseNetAsVarM traverseDeclM decl
traverseDeclM decl = do
    decl' <- traverseDeclNodesM traverseTypeM traverseExprM decl
    case decl' of
        Variable _ (Implicit sg rs) ident a _ ->
            -- implicit types, which are commonly found in function return
            -- types, are recast as logics to avoid outputting bare ranges
            insertType ident t' >> return decl'
            where t' = injectRanges (IntegerVector TLogic sg rs) a
        Variable d t ident a e -> do
            let t' = injectRanges t a
            insertType ident t'
            return $ case t' of
                UnpackedType t'' a' -> Variable d t'' ident a' e
                _ ->                   Variable d t'  ident [] e
        Param Parameter UnknownType ident String{} ->
            insertType ident (TypeOf $ Ident ident) >> return decl'
        Param _ UnknownType ident e ->
            typeof e >>= insertType ident >> return decl'
        Param _ (Implicit sg rs) ident _ ->
            insertType ident t' >> return decl'
            where t' = IntegerVector TLogic sg rs
        Param _ t ident _ ->
            insertType ident t >> return decl'
        _ -> return decl'

-- rewrite and store a non-genvar data declaration's type information
insertType :: Identifier -> Type -> ST ()
insertType ident typ = do
    -- hack to make this evaluation lazy
    typ' <- gets $ evalState $ scopeType typ
    insertElem ident typ'

-- convert TypeOf in a ModuleItem
traverseModuleItemM :: ModuleItem -> ST ModuleItem
traverseModuleItemM =
    traverseNodesM traverseExprM return traverseTypeM traverseLHSM return
    where traverseLHSM = traverseLHSExprsM traverseExprM

-- convert TypeOf in a GenItem
traverseGenItemM :: GenItem -> ST GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

-- convert TypeOf in a Stmt
traverseStmtM :: Stmt -> ST Stmt
traverseStmtM = traverseStmtExprsM traverseExprM

-- convert TypeOf in an Expr
traverseExprM :: Expr -> ST Expr
traverseExprM (Cast (Left (Implicit sg [])) expr) =
    -- `signed'(foo)` and `unsigned'(foo)` are syntactic sugar for the `$signed`
    -- and `$unsigned` system functions present in Verilog-2005
    traverseExprM $ Call (Ident fn) $ Args [expr] []
    where fn = if sg == Signed then "$signed" else "$unsigned"
traverseExprM (Cast (Left t) (Number (UnbasedUnsized bit))) =
    -- defer until this expression becomes explicit
    return $ Cast (Left t) (Number (UnbasedUnsized bit))
traverseExprM (Cast (Left t@(IntegerAtom TInteger _)) expr) =
    -- convert to cast to an integer vector type
    traverseExprM $ Cast (Left t') expr
    where
        (tf, []) = typeRanges t
        t' = tf [(RawNum 1, RawNum 1)]
traverseExprM (Cast (Left t1) expr) = do
    expr' <- traverseExprM expr
    t1' <- traverseTypeM t1
    t2 <- typeof expr'
    if typeCastUnneeded t1' t2
        then traverseExprM $ makeExplicit expr'
        else return $ Cast (Left t1') expr'
traverseExprM (Cast (Right (Ident x)) expr) = do
    expr' <- traverseExprM expr
    details <- lookupElemM x
    isGenvar <- isLoopVarM x
    if details == Nothing && not isGenvar
        then return $ Cast (Left $ Alias x []) expr'
        else elaborateSizeCast (Ident x) expr'
traverseExprM (Cast (Right size) expr) = do
    expr' <- traverseExprM expr
    size' <- traverseExprM size
    elaborateSizeCast size' expr'
traverseExprM orig@(Dot (Ident x) f) = do
    unneeded <- unneededModuleScope x f
    return $ if unneeded
        then Ident f
        else orig
traverseExprM other =
    traverseSinglyNestedExprsM traverseExprM other
    >>= traverseExprTypesM traverseTypeM

-- carry forward the signedness of the expression when cast to the given size
elaborateSizeCast :: Expr -> Expr -> ST Expr
elaborateSizeCast (Number size) _ | Just 0 == numberToInteger size =
    -- special case because zero-width ranges cannot be represented
    scopedErrorM $ "size cast width " ++ show size
        ++ " is not a positive integer"
elaborateSizeCast size value = do
    t <- typeof value
    force <- isStringParam value
    case (typeSignedness t, force) of
        (Unspecified, False)-> return $ Cast (Right size) value
        (sg, _) -> traverseExprM $ Cast (Left $ typeOfSize sg size) value

-- string params use a self-referential type to enable the string param
-- conversion to add a synthetic parameter if necessary; this check enables size
-- casts to assume a string parameter is unsigned regardless of its length
isStringParam :: Expr -> ST Bool
isStringParam (Ident x) = do
    details <- lookupElemM x
    return $ case details of
        Nothing -> False
        Just (_, _, typ) -> typ == TypeOf (Ident x)
isStringParam _ = return False

-- checks if referring to part.wire is needlessly explicit
unneededModuleScope :: Identifier -> Identifier -> ST Bool
unneededModuleScope part wire = do
    accessesLocal <- localAccessesM wire
    if accessesLocal == accessesTop then
        return True
    else if head accessesLocal == head accessesTop then do
        details <- lookupElemM wire
        return $ case details of
            Just (accessesFound, _, _) -> accessesTop == accessesFound
            _ -> False
    else
        return False
    where accessesTop = [Access part Nil, Access wire Nil]

-- convert TypeOf in a Type
traverseTypeM :: Type -> ST Type
traverseTypeM (TypeOf expr) =
    traverseExprM expr >>= typeof
traverseTypeM other =
    traverseSinglyNestedTypesM traverseTypeM other
    >>= traverseTypeExprsM traverseExprM

-- attempts to find the given (potentially hierarchical or generate-scoped)
-- expression in the available scope information
lookupTypeOf :: Expr -> ST Type
lookupTypeOf expr@(Ident x) = do
    details <- lookupElemM x
    loopVar <- loopVarDepthM x
    return $ case details of
        Nothing ->
            if loopVar == Nothing
                then TypeOf expr
                else IntegerAtom TInteger Unspecified
        Just (accesses, replacements, typ) ->
            if maybe True (length accesses >) loopVar
                then replaceInType replacements typ
                else IntegerAtom TInteger Unspecified
lookupTypeOf expr = do
    details <- lookupElemM expr
    return $ case details of
        Nothing -> TypeOf expr
        Just (_, replacements, typ) ->
            replaceInType replacements typ

-- determines the type of an expression based on the available scope information
-- according the semantics defined in IEEE 1800-2017, especially Section 11.6
typeof :: Expr -> ST Type
typeof (Number n) =
    return $ IntegerVector TLogic sg [r]
    where
        r = (RawNum $ size - 1, RawNum 0)
        size = numberBitLength n
        sg = if numberIsSigned n then Signed else Unspecified
typeof (Call (Ident x) args) = typeofCall x args
typeof orig@(Bit e _) = do
    t <- typeof e
    let t' = popRange t
    case t of
        TypeOf{} -> lookupTypeOf orig
        Alias{} -> return $ TypeOf orig
        _ -> return $ typeSignednessOverride t' Unsigned t'
typeof orig@(Range e NonIndexed r) = do
    t <- typeof e
    let t' = replaceRange r t
    return $ case t of
        TypeOf{} -> TypeOf orig
        Alias{} -> TypeOf orig
        _ -> typeSignednessOverride t' Unsigned t'
typeof (Range expr mode (base, len)) =
    typeof $ Range expr NonIndexed $
        endianCondRange index (base, end) (end, base)
    where
        index =
            if mode == IndexedPlus
                then (boundR, boundL)
                else (boundL, boundR)
        boundL = DimFn FnLeft  (Left $ TypeOf expr) (RawNum 1)
        boundR = DimFn FnRight (Left $ TypeOf expr) (RawNum 1)
        end =
            if mode == IndexedPlus
                then BinOp Sub (BinOp Add base len) (RawNum 1)
                else BinOp Add (BinOp Sub base len) (RawNum 1)
typeof orig@(Dot e x) = do
    t <- typeof e
    case t of
        Struct _ fields [] -> return $ fieldsType fields
        Union _  fields [] -> return $ fieldsType fields
        _ -> lookupTypeOf orig
    where
        fieldsType :: [Field] -> Type
        fieldsType fields =
            case lookup x $ map swap fields of
                Just typ -> typ
                Nothing -> TypeOf orig
typeof (Cast (Left t) _) = traverseTypeM t
typeof (UniOp op expr) = typeofUniOp op expr
typeof (BinOp op a b) = typeofBinOp op a b
typeof (Mux   _       a b) = largerSizeType a b
typeof (Concat      exprs) = return $ typeOfSize Unsigned $ concatSize exprs
typeof (Stream _ _  exprs) = return $ typeOfSize Unsigned $ concatSize exprs
typeof (Repeat reps exprs) = return $ typeOfSize Unsigned size
    where size = BinOp Mul reps (concatSize exprs)
typeof (String str) =
    return $ IntegerVector TBit Unspecified [r]
    where
        r = (RawNum $ len - 1, RawNum 0)
        len = if null str then 8 else 8 * unescapedLength str
typeof other = lookupTypeOf other

-- length of a string literal in characters
unescapedLength :: String -> Integer
unescapedLength [] = 0
unescapedLength ('\\' : _ : rest) = 1 + unescapedLength rest
unescapedLength (_        : rest) = 1 + unescapedLength rest

-- type of a standard (non-member) function call
typeofCall :: String -> Args -> ST Type
typeofCall "$unsigned" (Args [e] []) = return $ typeOfSize Unsigned $ sizeof e
typeofCall "$signed"   (Args [e] []) = return $ typeOfSize Signed   $ sizeof e
typeofCall "$clog2"    (Args [_] []) =
    return $ IntegerAtom TInteger Unspecified
typeofCall fnName _ = typeof $ Ident fnName

-- replaces the signing of a type if possible
typeSignednessOverride :: Type -> Signing -> Type -> Type
typeSignednessOverride fallback sg t =
    case t of
        IntegerVector base _ rs -> IntegerVector base sg rs
        IntegerAtom   base _    -> IntegerAtom   base sg
        _ -> fallback

-- type of a unary operator expression
typeofUniOp :: UniOp -> Expr -> ST Type
typeofUniOp UniAdd e = typeof e
typeofUniOp UniSub e = typeof e
typeofUniOp BitNot e = typeof e
typeofUniOp _ _ =
    -- unary reductions and logical negation
    return UnitType

-- type of a binary operator expression (Section 11.6.1)
typeofBinOp :: BinOp -> Expr -> Expr -> ST Type
typeofBinOp op a b =
    case op of
        LogAnd  -> unitType
        LogOr   -> unitType
        LogImp  -> unitType
        LogEq   -> unitType
        Eq      -> unitType
        Ne      -> unitType
        TEq     -> unitType
        TNe     -> unitType
        WEq     -> unitType
        WNe     -> unitType
        Lt      -> unitType
        Le      -> unitType
        Gt      -> unitType
        Ge      -> unitType
        Pow     -> typeof a
        ShiftL  -> typeof a
        ShiftR  -> typeof a
        ShiftAL -> typeof a
        ShiftAR -> typeof a
        Add     -> largerSizeType a b
        Sub     -> largerSizeType a b
        Mul     -> largerSizeType a b
        Div     -> largerSizeType a b
        Mod     -> largerSizeType a b
        BitAnd  -> largerSizeType a b
        BitXor  -> largerSizeType a b
        BitXnor -> largerSizeType a b
        BitOr   -> largerSizeType a b
    where unitType = return UnitType

-- produces a type large enough to hold either expression
largerSizeType :: Expr -> Expr -> ST Type
largerSizeType a (Number (Based 1 _ _ _ _)) = typeof a
largerSizeType a b = do
    t <- typeof a
    u <- typeof b
    let sg = binopSignedness (typeSignedness t) (typeSignedness u)
    return $
        if t == u then
            t
        else if sg == Unspecified then
            TypeOf $ BinOp Add a b
        else
            typeOfSize sg $ largerSizeOf a b

-- returns the signedness of a traditional arithmetic binop, if possible
binopSignedness :: Signing -> Signing -> Signing
binopSignedness Unspecified _ = Unspecified
binopSignedness _ Unspecified = Unspecified
binopSignedness Unsigned _ = Unsigned
binopSignedness _ Unsigned = Unsigned
binopSignedness Signed Signed = Signed

-- returns the signedness of the given type, if possible
typeSignedness :: Type -> Signing
typeSignedness (IntegerVector _ sg _) = signednessFallback Unsigned sg
typeSignedness (IntegerAtom   t sg  ) = signednessFallback fallback sg
    where fallback = if t == TTime then Unsigned else Signed
typeSignedness _ = Unspecified

-- helper for producing the former signing when the latter is unspecified
signednessFallback :: Signing -> Signing -> Signing
signednessFallback fallback Unspecified = fallback
signednessFallback _ sg = sg

-- returns the total size of concatenated list of expressions
concatSize :: [Expr] -> Expr
concatSize exprs =
    foldl (BinOp Add) (RawNum 0) $
    map sizeof exprs

-- returns the size of an expression, with the short-circuiting
sizeof :: Expr -> Expr
sizeof (Number n) = RawNum $ numberBitLength n
sizeof (Mux _ a b) = largerSizeOf a b
sizeof expr = DimsFn FnBits $ Left $ TypeOf expr

-- returns the maximum size of the two given expressions
largerSizeOf :: Expr -> Expr -> Expr
largerSizeOf a b =
    simplify $ Mux cond (sizeof a) (sizeof b)
    where cond = BinOp Ge (sizeof a) (sizeof b)

-- produces a generic type of the given size
typeOfSize :: Signing -> Expr -> Type
typeOfSize sg size =
    IntegerVector TLogic sg [(hi, RawNum 0)]
    where hi = simplify $ BinOp Sub size (RawNum 1)

-- combines a type with unpacked ranges
injectRanges :: Type -> [Range] -> Type
injectRanges t [] = t
injectRanges (UnpackedType t rs) unpacked = UnpackedType t $ unpacked ++ rs
injectRanges t unpacked = UnpackedType t unpacked

-- removes the most significant range of the given type
popRange :: Type -> Type
popRange (UnpackedType t [_]) = t
popRange (IntegerAtom TInteger sg) =
    IntegerVector TLogic sg []
popRange t =
    tf rs
    where (tf, _ : rs) = typeRanges t

-- replaces the most significant range of the given type
replaceRange :: Range -> Type -> Type
replaceRange r (UnpackedType t (_ : rs)) =
    UnpackedType t (r : rs)
replaceRange r (IntegerAtom TInteger sg) =
    IntegerVector TLogic sg [r]
replaceRange r t =
    tf (r : rs)
    where (tf, _ : rs) = typeRanges t

-- checks for a cast type which already trivially matches the expression type
typeCastUnneeded :: Type -> Type -> Bool
typeCastUnneeded t1 t2 =
    sg1 == sg2 && sz1 == sz2 && sz1 /= Nothing && sz2 /= Nothing
    where
        sg1 = typeSignedness t1
        sg2 = typeSignedness t2
        sz1 = typeSize t1
        sz2 = typeSize t2
        typeSize :: Type -> Maybe Expr
        typeSize (IntegerVector _ _ rs) = Just $ dimensionsSize rs
        typeSize t@IntegerAtom{} =
            typeSize $ tf [(RawNum 1, RawNum 1)]
            where (tf, []) = typeRanges t
        typeSize _ = Nothing

-- explicitly sizes top level numbers used in arithmetic expressions
makeExplicit :: Expr -> Expr
makeExplicit (Number n) =
    Number $ numberCast (numberIsSigned n) (fromIntegral $ numberBitLength n) n
makeExplicit (BinOp op e1 e2) =
    BinOp op (makeExplicit e1) (makeExplicit e2)
makeExplicit (UniOp op e) =
    UniOp op $ makeExplicit e
makeExplicit other = other
