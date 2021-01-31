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
 -}

module Convert.TypeOf (convert) where

import Data.Tuple (swap)
import qualified Data.Map.Strict as Map

import Convert.ExprUtils (simplify)
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ partScoper
    traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

-- single bit 4-state `logic` type
pattern UnitType :: Type
pattern UnitType = IntegerVector TLogic Unspecified []

-- insert the given declaration into the scope, and convert an TypeOfs within
traverseDeclM :: Decl -> Scoper Type Decl
traverseDeclM decl = do
    item <- traverseModuleItemM (MIPackageItem $ Decl decl)
    let MIPackageItem (Decl decl') = item
    case decl' of
        Variable _ (Implicit sg rs) ident a _ ->
            -- implicit types, which are commonly found in function return
            -- types, are recast as logics to avoid outputting bare ranges
            insertElem ident t' >> return decl'
            where t' = injectRanges (IntegerVector TLogic sg rs) a
        Variable d t ident a e -> do
            let t' = injectRanges t a
            insertElem ident t'
            return $ case t' of
                UnpackedType t'' a' -> Variable d t'' ident a' e
                _ ->                   Variable d t'  ident [] e
        Param _ UnknownType ident String{} ->
            insertElem ident UnknownType >> return decl'
        Param _ UnknownType ident e ->
            typeof e >>= insertElem ident >> return decl'
        Param _ (Implicit sg rs) ident _ ->
            insertElem ident t' >> return decl'
            where t' = IntegerVector TLogic sg rs
        Param _ t ident _ ->
            insertElem ident t >> return decl'
        ParamType{} -> return decl'
        CommentDecl{} -> return decl'

-- convert TypeOf in a ModuleItem
traverseModuleItemM :: ModuleItem -> Scoper Type ModuleItem
traverseModuleItemM (Genvar x) = do
    insertElem x $ IntegerAtom TInteger Unspecified
    return $ Genvar x
traverseModuleItemM item =
    traverseTypesM (traverseNestedTypesM traverseTypeM) item

-- convert TypeOf in a GenItem
traverseGenItemM :: GenItem -> Scoper Type GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

-- convert TypeOf in a Stmt
traverseStmtM :: Stmt -> Scoper Type Stmt
traverseStmtM = traverseStmtExprsM traverseExprM

-- convert TypeOf in a Expr
traverseExprM :: Expr -> Scoper Type Expr
traverseExprM = traverseNestedExprsM $ traverseExprTypesM $
    traverseNestedTypesM traverseTypeM

traverseTypeM :: Type -> Scoper Type Type
traverseTypeM (TypeOf expr) = typeof expr
traverseTypeM other = return other

-- attempts to find the given (potentially hierarchical or generate-scoped)
-- expression in the available scope information
lookupTypeOf :: Expr -> Scoper Type Type
lookupTypeOf expr = do
    details <- lookupElemM expr
    case details of
        Nothing -> return $ TypeOf expr
        Just (_, replacements, typ) ->
            return $ if Map.null replacements
                then typ
                else rewriteType replacements typ
    where
        rewriteType :: Replacements -> Type -> Type
        rewriteType replacements = traverseNestedTypes $ traverseTypeExprs $
            traverseNestedExprs (replace replacements)
        replace :: Replacements -> Expr -> Expr
        replace replacements (Ident x) =
            Map.findWithDefault (Ident x) x replacements
        replace _ other = other

-- determines the type of an expression based on the available scope information
-- according the semantics defined in IEEE 1800-2017, especially Section 11.6
typeof :: Expr -> Scoper Type Type
typeof (Number n) =
    return $ IntegerVector TLogic sg [r]
    where
        r = (RawNum $ size - 1, RawNum 0)
        size = numberBitLength n
        sg = if numberIsSigned n then Signed else Unspecified
typeof (Call (Ident x) args) = typeofCall x args
typeof (orig @ (Bit e _)) = do
    t <- typeof e
    let t' = popRange t
    case t of
        TypeOf{} -> lookupTypeOf orig
        Alias{} -> return $ TypeOf orig
        _ -> return $ typeSignednessOverride t' Unsigned t'
typeof (orig @ (Range e mode r)) = do
    t <- typeof e
    let t' = replaceRange (lo, hi) t
    return $ case t of
        TypeOf{} -> TypeOf orig
        Alias{} -> TypeOf orig
        _ -> typeSignednessOverride t' Unsigned t'
    where
        lo = fst r
        hi = case mode of
            NonIndexed   -> snd r
            IndexedPlus  -> BinOp Sub (uncurry (BinOp Add) r) (RawNum 1)
            IndexedMinus -> BinOp Add (uncurry (BinOp Sub) r) (RawNum 1)
typeof (orig @ (Dot e x)) = do
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
        len = if null str then 1 else 8 * unescapedLength str
typeof other = lookupTypeOf other

-- length of a string literal in characters
unescapedLength :: String -> Integer
unescapedLength [] = 0
unescapedLength ('\\' : _ : rest) = 1 + unescapedLength rest
unescapedLength (_        : rest) = 1 + unescapedLength rest

-- type of a standard (non-member) function call
typeofCall :: String -> Args -> Scoper Type Type
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
        Net           base _ rs -> Net           base sg rs
        Implicit           _ rs -> Implicit           sg rs
        _ -> fallback

-- type of a unary operator expression
typeofUniOp :: UniOp -> Expr -> Scoper Type Type
typeofUniOp UniAdd e = typeof e
typeofUniOp UniSub e = typeof e
typeofUniOp BitNot e = typeof e
typeofUniOp _ _ =
    -- unary reductions and logical negation
    return UnitType

-- type of a binary operator expression (Section 11.6.1)
typeofBinOp :: BinOp -> Expr -> Expr -> Scoper Type Type
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
largerSizeType :: Expr -> Expr -> Scoper Type Type
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
typeSignedness (Net           _ sg _) = signednessFallback Unsigned sg
typeSignedness (Implicit        sg _) = signednessFallback Unsigned sg
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
    where hi = BinOp Sub size (RawNum 1)

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
