{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for the `type` operator
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

pattern UnknownType :: Type
pattern UnknownType = Implicit Unspecified []

traverseDeclM :: Decl -> Scoper Type Decl
traverseDeclM decl = do
    item <- traverseModuleItemM (MIPackageItem $ Decl decl)
    let MIPackageItem (Decl decl') = item
    case decl' of
        Variable Local UnknownType ident [] Nil -> do
            -- functions with no return type implicitly return a single bit
            insertElem ident $ IntegerVector TLogic Unspecified []
            return decl'
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
        Param _ t ident _ ->
            insertElem ident t >> return decl'
        ParamType{} -> return decl'
        CommentDecl{} -> return decl'

traverseModuleItemM :: ModuleItem -> Scoper Type ModuleItem
traverseModuleItemM (Genvar x) = do
    insertElem x $ IntegerAtom TInteger Unspecified
    return $ Genvar x
traverseModuleItemM item =
    traverseTypesM (traverseNestedTypesM traverseTypeM) item

traverseGenItemM :: GenItem -> Scoper Type GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseStmtM :: Stmt -> Scoper Type Stmt
traverseStmtM = traverseStmtExprsM traverseExprM

traverseExprM :: Expr -> Scoper Type Expr
traverseExprM = traverseNestedExprsM $ traverseExprTypesM $
    traverseNestedTypesM traverseTypeM

traverseTypeM :: Type -> Scoper Type Type
traverseTypeM (TypeOf expr) = typeof expr
traverseTypeM other = return other

lookupTypeOf :: Expr -> Scoper Type Type
lookupTypeOf expr = do
    details <- lookupElemM expr
    case details of
        Nothing -> return $ TypeOf expr
        Just (_, replacements, typ) ->
            return $ if Map.null replacements
                then typ
                else rewriteType typ
            where
                rewriteType = traverseNestedTypes $ traverseTypeExprs $
                    traverseNestedExprs replace
                replace :: Expr -> Expr
                replace (Ident x) =
                    Map.findWithDefault (Ident x) x replacements
                replace other = other

typeof :: Expr -> Scoper Type Type
typeof (Number n) =
    return $ IntegerVector TLogic sg [r]
    where
        r = (RawNum $ size - 1, RawNum 0)
        size = numberBitLength n
        sg = if numberIsSigned n then Signed else Unspecified
typeof (Call (Ident "$unsigned") (Args [e] [])) =
    typeof e
typeof (Call (Ident "$signed") (Args [e] [])) =
    typeof e
typeof (Call (Ident "$clog2") (Args [_] [])) =
    return $ IntegerAtom TInteger Unspecified
typeof (Call (Ident x) _) =
    typeof $ Ident x
typeof (orig @ (Bit e _)) = do
    t <- typeof e
    case t of
        TypeOf{} -> lookupTypeOf orig
        Alias{} -> return $ TypeOf orig
        _ -> return $ popRange t
typeof (orig @ (Range e mode r)) = do
    t <- typeof e
    return $ case t of
        TypeOf{} -> TypeOf orig
        Alias{} -> TypeOf orig
        _ -> replaceRange (lo, hi) t
    where
        lo = fst r
        hi = case mode of
            NonIndexed   -> snd r
            IndexedPlus  -> BinOp Sub (uncurry (BinOp Add) r) (RawNum 1)
            IndexedMinus -> BinOp Add (uncurry (BinOp Sub) r) (RawNum 1)
typeof (orig @ (Dot e x)) = do
    t <- typeof e
    case t of
        Struct _ fields [] ->
            return $ fieldsType fields
        Union _ fields [] ->
            return $ fieldsType fields
        _ -> lookupTypeOf orig
    where
        fieldsType :: [Field] -> Type
        fieldsType fields =
            case lookup x $ map swap fields of
                Just typ -> typ
                Nothing -> TypeOf orig
typeof (Cast (Right s) _) = return $ typeOfSize s
typeof (UniOp UniSub  e  ) = typeof e
typeof (UniOp BitNot  e  ) = typeof e
typeof (UniOp LogNot  _  ) = return $ IntegerVector TLogic Unspecified []
typeof (BinOp Pow     e _) = typeof e
typeof (BinOp ShiftL  e _) = typeof e
typeof (BinOp ShiftR  e _) = typeof e
typeof (BinOp ShiftAL e _) = typeof e
typeof (BinOp ShiftAR e _) = typeof e
typeof (BinOp Add     a b) = largerSizeType a b
typeof (BinOp Sub     a b) = largerSizeType a b
typeof (BinOp Mul     a b) = largerSizeType a b
typeof (BinOp Div     a b) = largerSizeType a b
typeof (BinOp Mod     a b) = largerSizeType a b
typeof (BinOp BitAnd  a b) = largerSizeType a b
typeof (BinOp BitXor  a b) = largerSizeType a b
typeof (BinOp BitXnor a b) = largerSizeType a b
typeof (BinOp BitOr   a b) = largerSizeType a b
typeof (Mux   _       a b) = largerSizeType a b
typeof (Concat      exprs) = return $ typeOfSize $ concatSize exprs
typeof (Repeat reps exprs) = return $ typeOfSize size
    where size = BinOp Mul reps (concatSize exprs)
typeof (String str) =
    return $ IntegerVector TBit Unspecified [r]
    where
        r = (RawNum $ len - 1, RawNum 0)
        len = if null str then 1 else 8 * unescapedLength str
        unescapedLength :: String -> Integer
        unescapedLength [] = 0
        unescapedLength ('\\' : _ : rest) = 1 + unescapedLength rest
        unescapedLength (_        : rest) = 1 + unescapedLength rest
typeof other = lookupTypeOf other

-- produces a type large enough to hold either expression
largerSizeType :: Expr -> Expr -> Scoper Type Type
largerSizeType a (Number (Based 1 _ _ _ _)) = typeof a
largerSizeType a b = do
    t <- typeof a
    u <- typeof b
    return $ if t == u
        then t
        else typeOfSize $ largerSizeOf a b

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
typeOfSize :: Expr -> Type
typeOfSize size =
    IntegerVector TLogic sg [(hi, RawNum 0)]
    where
        sg = Unspecified -- suitable for now
        hi = BinOp Sub size (RawNum 1)

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
