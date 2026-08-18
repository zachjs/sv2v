{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `typedef`/`localparam type` and `enum`
 -
 - TODO: Merge and update the old `Enum` and `Typedef` conversion descriptions
 - below.
 -
 - Aliased types can appear in all data declarations, including modules, blocks,
 - and function parameters. They are also found in type cast expressions.
 -
 - This conversion replaces references to enum items with their values. The
 - values are explicitly cast to the enum's base type.
 -
 - SystemVerilog allows for enums to have any number of the items' values
 - specified or unspecified. If the first one is unspecified, it is 0. All other
 - unspecified values take on the value of the previous item, plus 1.
 -
 - It is an error for multiple items of the same enum to take on the same value,
 - whether implicitly or explicitly. We catch try to catch "obvious" instances
 - of conflicts.
 -}

module Convert.TypeName (convert) where

import Control.Monad (when, (>=>))
import Data.List (elemIndices)
import Data.Tuple (swap)

import Control.Monad.Reader
import Convert.TypeOf (injectRanges, popRange, typeSignednessOverride)
import Convert.ExprUtils
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ flip runReader False . evalScoperT . scopeModule scoper
    where scoper = scopeModuleItem
            traverseDecl traverseModuleItem traverseGenItem traverseStmt

type SC = ScoperT IdentKind (Reader Bool)

data IdentKind
    = EnumItem Expr -- enum item value
    | TypeName Type -- resolved typename
    | Pending -- unresolved type parameter
    | NonType String Type -- anything else
    deriving Show

resolveTypeOrExpr :: TypeOrExpr -> SC TypeOrExpr
resolveTypeOrExpr tore
    | Left (TypeOf expr) <- tore = possibleTypeName tore expr
    | Right expr <- tore = possibleTypeName tore expr
    | otherwise = return tore

possibleTypeName :: TypeOrExpr -> Expr -> SC TypeOrExpr
possibleTypeName orig expr
    | Just (x, rs1) <- exprToTypeName [] expr = do
        details <- lookupElemM x
        return $ case details of
            Just (_, _, TypeName typ) ->
                Left $ tf $ rs1 ++ rs2
                where (tf, rs2) = typeRanges typ
            Just (_, _, Pending) ->
                Left $ Alias x rs1
            _ -> orig
    | otherwise = return orig

-- aliases in type-or-expr contexts are parsed as expressions
exprToTypeName :: [Range] -> Expr -> Maybe (Identifier, [Range])
exprToTypeName rs (Ident x) = Just (x, rs)
exprToTypeName rs (Bit expr idx) =
    exprToTypeName (r : rs) expr
    where r = (RawNum 0, BinOp Sub idx (RawNum 1))
exprToTypeName rs (Range expr NonIndexed r) =
    exprToTypeName (r : rs) expr
exprToTypeName _ _ = Nothing

traverseExpr :: Expr -> SC Expr
traverseExpr (Cast v e) = do
    v' <- resolveTypeOrExpr v
    traverseExpr' $ Cast v' e
traverseExpr (DimsFn f v) = do
    v' <- resolveTypeOrExpr v
    traverseExpr' $ DimsFn f v'
traverseExpr (DimFn f v e) = do
    v' <- resolveTypeOrExpr v
    traverseExpr' $ DimFn f v' e
traverseExpr (Pattern items) = do
    names <- mapM resolveTypeOrExpr $ map fst items
    let exprs = map snd items
    traverseExpr' $ Pattern $ zip names exprs
traverseExpr other = traverseExpr' other

traverseExpr' :: Expr -> SC Expr
traverseExpr' expr = local (const False) $ do
    details <- lookupElemM expr
    case details of
        Just (_, _, EnumItem value) -> recurse value
        _ -> do
            (expr', _) <- tryElabEnumFn expr
            recurse $ if expr' == Nil
                then expr
                else expr'
    where
        recurse =
            traverseSinglyNestedExprsM traverseExpr >=>
            traverseExprTypesM traverseType

traverseModuleItem :: ModuleItem -> SC ModuleItem
traverseModuleItem (Genvar x) =
    insertElem x (NonType "genvar" t) >> return (Genvar x)
    where t = IntegerAtom TInteger Unspecified
traverseModuleItem (Instance m params x rs p) = local (const True) $ do
    let mapParam (i, v) = resolveTypeOrExpr v >>= \v' -> return (i, v')
    params' <- mapM mapParam params
    traverseModuleItemM' $ Instance m params' x rs p
traverseModuleItem item = traverseModuleItemM' item

traverseModuleItemM' :: ModuleItem -> SC ModuleItem
traverseModuleItemM' =
    traverseNodesM traverseExpr return traverseType traverseLHSM return
    where traverseLHSM = traverseNestedLHSsM $ traverseLHSExprsM traverseExpr

traverseGenItem :: GenItem -> SC GenItem
traverseGenItem = traverseGenItemExprsM traverseExpr

traverseStmt :: Stmt -> SC Stmt
traverseStmt = traverseStmtExprsM traverseExpr

traverseDecl :: Decl -> SC Decl
traverseDecl decl =
    case decl of
        Variable d t x a e -> do
            t' <- local (const True) $ traverseType t
            let t'' = justReplaceEnums t'
            scopeType (makeVar $ injectRanges t' a) >>= insertElem x . NonType "var"
            return $ Variable d t'' x a e
        Net  d n s t x a e -> do
            t' <- local (const True) $ traverseType t
            let t'' = justReplaceEnums t'
            scopeType (makeVar $ injectRanges t' a) >>= insertElem x . NonType "net"
            return $ Net d n s t'' x a e
        Param  s   t x   e -> do
            t' <- local (const True) $ traverseType t
            t'' <- case t of
                UnknownType -> typeof e
                _ -> return t'
            scopeType (makeVar t'') >>= insertElem x . NonType (show s)
            let t''' = justReplaceEnums t'
            return $ Param s (
                case t''' of
                    UnpackedType u rs1 ->
                        tf $ rs1 ++ rs2
                        where (tf, rs2) = typeRanges u
                    _ -> t'''
                ) x e
        ParamType Localparam x t -> do
            t' <- local (const True) $ traverseType t
            scopeType t' >>= insertElem x . TypeName
            return $ CommentDecl $ "removed localparam type " ++ x
        ParamType Parameter x t -> do
            t' <- traverseType t
            insertElem x Pending
            return $ ParamType Parameter x t'
        CommentDecl{} -> return decl
    >>= traverseDeclNodesM return traverseExpr

makeVar :: Type -> Type
makeVar (Implicit sg rs) = IntegerVector TLogic sg rs
makeVar other = other

-- replace enum types and insert enum items
replaceEnum :: Bool -> Type -> SC Type
-- TODO: Perhaps I don't need to resolve TypeOf in base types here, but I should
-- make sure they get scoped properly!
replaceEnum track (Enum typ@Alias{} items rs) = do
    typ' <- resolveTypeName typ
    replaceEnum track $ Enum typ' items rs
replaceEnum track (Enum (Implicit sg rl) v rs) =
    replaceEnum track $ Enum t' v rs
    where
        -- default to a 32 bit logic
        t' = IntegerVector TLogic sg rl'
        rl' = if null rl
            then [(RawNum 31, RawNum 0)]
            else rl
replaceEnum track orig@(Enum t v rs) = do
    when track $ insertEnumItems t v
    preserveEnums <- lift ask
    return $ if preserveEnums
        then orig
        else tf $ rl ++ rs
    where (tf, rl) = typeRanges t
replaceEnum _ other = return other

insertEnumItems :: Type -> [EnumItem] -> SC ()
insertEnumItems itemType items =
    -- check for obviously duplicate values
    if noDuplicates
        then mapM_ (uncurry insertEnumItem) items'
        else scopedErrorM $ "enum conversion has duplicate vals: "
                ++ show items'
    where
        insertEnumItem :: Identifier -> Expr -> SC ()
        insertEnumItem x = scopeExpr >=> insertElem x . EnumItem
        items' = elabEnumItems itemType items
        vals = map snd items'
        noDuplicates = all (null . tail . flip elemIndices vals) vals

elabEnumItems :: Type -> [EnumItem] -> [EnumItem]
elabEnumItems itemType items =
    zip keys vals'
    where
        vals' = map (Cast $ Left itemType) vals
        (keys, valsRaw) = unzip items
        vals = tail $ scanl step (UniOp UniSub $ RawNum 1) valsRaw
        step :: Expr -> Expr -> Expr
        step expr Nil = simplify $ BinOp Add expr (RawNum 1)
        step _ expr = expr

resolveTypeName :: Type -> SC Type
resolveTypeName (TypeOf expr) = typeof expr
resolveTypeName (Alias st rs1) = do
    details <- lookupElemM st
    case details of
        Just (_, _, TypeName typ) ->
            return $ tf $ rs1 ++ rs2
            where (tf, rs2) = typeRanges typ
        Just (_, _, Pending) ->
            return $ Alias st rs1
        Just (_, _, NonType kind _) ->
            scopedErrorM $ "expected typename, but found " ++ kind
                ++ " identifier " ++ show st
        Just (_, _, EnumItem _) ->
            scopedErrorM $ "expected typename, but found enum item identifier "
                ++ show st
        Nothing ->
            scopedErrorM $ "couldn't resolve typename " ++ show st
resolveTypeName (TypedefRef expr) = do
    details <- lookupElemM expr
    case details of
        Just (_, _, TypeName typ) -> return typ
        Just (_, _, Pending) ->
            error "TypdefRef invariant violated! Please file an issue."
        Just (_, _, NonType kind _) ->
            scopedErrorM $ "expected interface-based typename, but found "
                ++ kind ++ " " ++ show expr
        Just (_, _, EnumItem _) ->
            scopedErrorM $ "expected interface-based typename, but found "
                ++ "enum item " ++ show expr
        -- This can occur when the interface conversion is delayed due to
        -- multi-dimensional instances.
        Nothing -> return $ TypedefRef expr
resolveTypeName other = return other

typeof :: Expr -> SC Type
typeof expr = do
    details <- lookupElemM expr
    case details of
        Just (_, _, NonType _ typ) ->
            resolveTypeName typ
        Just{} -> typeof' expr
        Nothing -> do
            (expr', typ') <- tryElabEnumFn expr
            if expr' == Nil
                then typeof' expr
                else return typ'

typeof' :: Expr -> SC Type
typeof' (Cast (Left typ) _) = resolveTypeName typ
typeof' orig@(Cast (Right expr) _) = do
    details <- lookupElemM expr
    case details of
        Just (_, _, TypeName typ) -> resolveTypeName typ
        _ -> return $ TypeOf orig
typeof' orig@(Dot expr fieldName) = do
    typ <- typeof expr
    case typ of
        Struct _ fields [] -> fieldsType orig typ fields fieldName
        Union  _ fields [] -> fieldsType orig typ fields fieldName
        _ -> return $ TypeOf orig
typeof' orig@(Bit e _) = do
    t <- typeof e
    case t of
        TypeOf{} -> return $ TypeOf orig
        Alias{} -> return $ TypeOf orig
        _ -> do
            t' <- popRange orig t
            return $ typeSignednessOverride t' Unsigned t'
typeof' expr = return $ TypeOf expr

fieldsType :: Expr -> Type -> [Field] -> Identifier -> SC Type
fieldsType expr struct fields fieldName =
    case lookup fieldName $ map swap fields of
        Just typ -> resolveTypeName typ
        Nothing -> scopedErrorM $ "field '" ++ fieldName ++ "' not found in "
                    ++ show struct ++ ", in expression " ++ show expr

tryElabEnumFn :: Expr -> SC (Expr, Type)
tryElabEnumFn (Dot e n) = tryElabEnumFn' e n Nil
tryElabEnumFn (Call (Dot e n) args) =
    case args of
        Args [ ] [        ] -> return Nil
        Args [a] [        ] -> return a
        Args [ ] [("N", a)] -> return a
        _ -> scopedErrorM $ "unexpected arguments " ++ show args ++ " passed "
                ++ "to enum method " ++ show (Dot e n)
    >>= tryElabEnumFn' e n
tryElabEnumFn _ = return (Nil, UnknownType)

tryElabEnumFn' :: Expr -> Identifier -> Expr -> SC (Expr, Type)
tryElabEnumFn' expr name arg = do
    typ <- typeof expr
    case typ of
        Enum itemType items [] -> do
            (expr', returnsEnum) <- elabEnumFn items' expr name arg
            let typ' = if returnsEnum then typ else TypeOf expr'
            return (expr', typ')
            where items' = elabEnumItems itemType items
        _ -> return (Nil, UnknownType)

elabEnumFn :: [EnumItem] -> Expr -> Identifier -> Expr -> SC (Expr, Bool)

-- elaborate simple enum methods that don't take an argument
elabEnumFn items expr name arg
    | name == "first" = simple True  $ snd . head
    | name == "last"  = simple True  $ snd . last
    | name == "num"   = simple False $ RawNum . fromIntegral . length
    | name == "name"  = simple False $ enumItemName expr
    where
        simple returnsEnum operation = if arg == Nil
            then return (operation items, returnsEnum)
            else scopedErrorM $ "unexpected argument " ++ show arg
                    ++ " passed to enum method " ++ show (Dot expr name)

-- elaborate iterator enum methods, which take an optional argument
elabEnumFn items key name offset
    | name == "next" = return $ iterator Add   1
    | name == "prev" = return $ iterator Sub (-1)
    where
        def = Number $ UnbasedUnsized BitX
        keys = map snd items
        len = length keys
        indices = map RawNum [0..fromIntegral len - 1]

        iterator :: BinOp -> Int -> (Expr, Bool)
        iterator op dir = (, True) $ case offset of
            Nil ->
                select def key keys shifted
                where shifted = take len $ drop (len + dir) (cycle keys)
            _ ->
                select def index indices keys
                where index = enumIndex op offset key keys

elabEnumFn _ _ name _ = scopedErrorM $ "unknown enum method " ++ show name

-- lookup `key` in `keys`, selecting the corresponding value, with default `def`
select :: Expr -> Expr -> [Expr] -> [Expr] -> Expr
select def key keys = foldr step def . zip keys
    where
        step :: (Expr, Expr) -> Expr -> Expr
        step = uncurry $ Mux . BinOp Eq key

-- string literal name of the enum item with the given value
enumItemName :: Expr -> [EnumItem] -> Expr
enumItemName key items =
    select (String "") key keys (map String names)
    where (names, keys) = unzip items

-- zero-based index of the enum item with the given value
enumIndex :: BinOp -> Expr -> Expr -> [Expr] -> Expr
enumIndex op offset key keys =
    modulo $
    BinOp Add numItems $
    BinOp op
        (select numItems key keys values)
        (modulo $ Cast (Left argType) offset)
    where
        numItems = RawNum $ fromIntegral $ length keys
        modulo = flip (BinOp Mod) numItems
        values = map RawNum [0..]
        argType = IntegerAtom TInt Unsigned

traverseType :: Type -> SC Type
traverseType typ
    | TypeOf{} <- typ = traverseTypeIgnoreEnumItems typ
    | Alias{} <- typ = traverseTypeIgnoreEnumItems typ
    | TypedefRef{} <- typ = traverseTypeIgnoreEnumItems typ
    | otherwise =
        resolveTypeName typ >>=
        replaceEnum True >>=
        traverseSinglyNestedTypesM traverseType >>=
        traverseTypeExprsM traverseExpr

traverseTypeIgnoreEnumItems :: Type -> SC Type
traverseTypeIgnoreEnumItems =
    resolveTypeName >=>
    replaceEnum False >=>
    traverseSinglyNestedTypesM traverseTypeIgnoreEnumItems >=>
    traverseTypeExprsM traverseExpr

justReplaceEnums :: Type -> Type
justReplaceEnums (Enum t _ rs) =
    tf $ rl ++ rs
    where (tf, rl) = typeRanges t
justReplaceEnums t =
    traverseSinglyNestedTypes justReplaceEnums t
