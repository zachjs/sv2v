{- sv2v
 - Author: Gomez
 -
 - Conversion for enumerated type methods: next, prev, first, last, and num.
 -
 - IEEE 1800-2017 Section 6.19.5. These methods are rewritten to synthesizable
 - expressions using the enum's value list. `name()` is not converted.
 -
 - This pass must run before Convert.Enum strips enum types down to their base
 - integer-vector types (i.e. earlier in the composed main-phase pipeline than
 - Enum, which with `foldr1 (.)` means it appears *after* Enum in the list).
 -
 - `next`/`prev` wrap around the enumeration. An optional unsigned step count is
 - supported when it is a constant expression. Values that are not members of
 - the enumeration map to the first member (matching common synthesizable
 - defaults for 2-state enums).
 -
 - Enum item values are scoped and cached when the enum type is declared so
 - method calls inside nested scopes still compare against the correct constants.
 -
 - typeof/lookupTypeOf/injectRanges are duplicated in trimmed form from
 - Convert.TypeOf because that pass elaborates TypeOf nodes and runs much earlier;
 - here we only need Ident/Dot/Cast resolution while Enum types are still intact.
 -}

module Convert.EnumMethod (convert) where

import Control.Monad (zipWithM_, (>=>) )
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import Convert.ExprUtils (dimensionsSize, simplify)
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

data Stored = StoredType Type | StoredEnumItem Expr

type ST = Scoper Stored

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ partScoper
    traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

traverseDeclM :: Decl -> ST Decl
traverseDeclM decl@Net{} =
    traverseNetAsVarM traverseDeclM decl
traverseDeclM decl = do
    -- record type aliases before rewriting nested exprs so `e_t x; x.next()`
    -- can resolve while the typedef still carries the Enum item list
    case decl of
        ParamType _ x t -> do
            insertElem x (StoredType t)
            cacheEnumItems t
        Variable _ t ident a _ -> do
            insertElem ident (StoredType $ injectRanges t a)
            cacheEnumItems t
        Param _ UnknownType ident e ->
            typeof e >>= insertElem ident . StoredType
        Param _ t ident _ -> insertElem ident (StoredType t)
        _ -> return ()
    traverseDeclNodesM traverseTypeM traverseExprM decl

traverseModuleItemM :: ModuleItem -> ST ModuleItem
traverseModuleItemM =
    traverseNodesM traverseExprM return traverseTypeM traverseLHSM return
    where traverseLHSM = traverseNestedLHSsM $ traverseLHSExprsM traverseExprM

traverseGenItemM :: GenItem -> ST GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseStmtM :: Stmt -> ST Stmt
traverseStmtM = traverseStmtExprsM traverseExprM

traverseTypeM :: Type -> ST Type
traverseTypeM =
    traverseSinglyNestedTypesM traverseTypeM >=>
    traverseTypeExprsM traverseExprM

traverseExprM :: Expr -> ST Expr
traverseExprM (Call (Dot e method) (Args pn kw)) = do
    e' <- traverseExprM e
    pn' <- mapM traverseExprM pn
    kw' <- mapM (\(x, a) -> (x,) <$> traverseExprM a) kw
    let args' = Args pn' kw'
    if isEnumMethod method
        then do
            t <- typeof e' >>= resolveType
            converted <- convertMethodM method e' t args'
            case converted of
                Call{} -> return converted
                _ -> traverseExprM converted
        else return $ Call (Dot e' method) args'
traverseExprM other =
    traverseSinglyNestedExprsM traverseExprM other
        >>= traverseExprTypesM traverseTypeM

injectRanges :: Type -> [Range] -> Type
injectRanges t [] = t
injectRanges t rs = UnpackedType t rs

-- follow typedef aliases to the underlying type
resolveType :: Type -> ST Type
resolveType (Alias x rs) = do
    details <- lookupElemM x
    case details of
        Just (_, replacements, StoredType typ) -> do
            typ' <- resolveType $ replaceInType replacements typ
            let (tf, rs2) = typeRanges typ'
            return $ tf $ rs ++ rs2
        Just (_, _, StoredEnumItem{}) -> return $ Alias x rs
        Nothing -> return $ Alias x rs
resolveType other = return other

lookupTypeOf :: Expr -> ST Type
lookupTypeOf expr = do
    details <- lookupElemM expr
    case details of
        Just (_, replacements, StoredType typ) ->
            resolveType $ replaceInType replacements typ
        _ -> return $ TypeOf expr

typeof :: Expr -> ST Type
typeof (Cast (Left t) _) = resolveType t
typeof (Dot e x) = do
    t <- typeof e >>= resolveType
    case t of
        Struct _ fields [] -> return $ fieldType fields
        Union  _ fields [] -> return $ fieldType fields
        _ -> lookupTypeOf (Dot e x)
    where
        fieldType fields =
            fromMaybe (TypeOf (Dot e x)) $ lookup x $ map swap fields
typeof (Ident x) = lookupTypeOf (Ident x)
typeof other = lookupTypeOf other

cacheEnumItems :: Type -> ST ()
cacheEnumItems t = case peel t of
    Just (itemBase, items) ->
        zipWithM_ (cacheItem itemBase) (map fst items) (rawValues itemBase items)
    _ -> return ()
    where
        cacheItem itemBase name val = do
            scoped <- scopeExpr $ Cast (Left itemBase) val
            insertElem name (StoredEnumItem scoped)

isEnumMethod :: Identifier -> Bool
isEnumMethod method = method `elem`
    ["next", "prev", "first", "last", "num", "name"]

convertMethodM :: Identifier -> Expr -> Type -> Args -> ST Expr
convertMethodM method expr typ args =
    case peel typ of
        Nothing -> case typ of
            Enum Alias{} _ _ -> return $ Call (Dot expr method) args
            -- defer until a later main iteration after ParamType substitutes
            TypeOf{} -> return $ Call (Dot expr method) args
            _ -> return $ Call (Dot expr method) args
        Just (base, items) -> case method of
            "name" -> enumMethodError method expr
            "num" | noArgs args ->
                return $ RawNum $ fromIntegral $ length items
            "first" | noArgs args ->
                itemValues base items >>= \vals ->
                    return $ head vals
            "last" | noArgs args ->
                itemValues base items >>= \vals ->
                    return $ last vals
            "next" ->
                stepMethodM expr base items args 1
            "prev" ->
                stepMethodM expr base items args (-1)
            _ -> enumMethodError method expr

enumMethodError :: Identifier -> Expr -> ST Expr
enumMethodError method expr =
    scopedErrorM $ "cannot convert enum method " ++ method
        ++ " on " ++ show expr

peel :: Type -> Maybe (Type, [EnumItem])
peel (Enum Alias{} _ _) = Nothing
peel (Enum (Implicit sg rl) items rs) =
    peel $ Enum t items rs
    where
        t = IntegerVector TLogic sg rl'
        rl' = if null rl then [(RawNum 31, RawNum 0)] else rl
peel (Enum base items _) = Just (base, items)
peel _ = Nothing

noArgs :: Args -> Bool
noArgs (Args [] []) = True
noArgs _ = False

stepMethodM :: Expr -> Type -> [EnumItem] -> Args -> Integer -> ST Expr
stepMethodM expr base items args direction = do
    case stepCount args of
        Nothing -> enumMethodError "next/prev" expr
        Just steps -> do
            vals <- itemValues base items
            applyStepsM expr base vals (direction * steps)

stepCount :: Args -> Maybe Integer
stepCount (Args [] []) = Just 1
stepCount (Args [n] []) =
    case simplify n of
        Number num -> numberToInteger num
        _ -> Nothing
stepCount _ = Nothing

rawValues :: Type -> [EnumItem] -> [Expr]
rawValues _ items =
    tail $ scanl step (UniOp UniSub $ RawNum 1) $ map snd items
    where
        step prev Nil = simplify $ BinOp Add prev (RawNum 1)
        step _ expr = expr

itemValues :: Type -> [EnumItem] -> ST [Expr]
itemValues base items = mapM lookupCached (map fst items)
    where
        byName = zip (map fst items) (rawValues base items)
        lookupCached name = do
            details <- lookupElemM name
            case details of
                Just (_, _, StoredEnumItem e) -> return e
                _ ->
                    case lookup name byName of
                        Nothing -> enumMethodError "next/prev" (Ident name)
                        Just raw -> scopeExpr $ Cast (Left base) raw

castVal :: Type -> Expr -> Expr
castVal t e = Cast (Left t) e

applyStepsM :: Expr -> Type -> [Expr] -> Integer -> ST Expr
applyStepsM expr base vals steps
    | steps == 0 = return $ head vals
    | otherwise = case (valsFromZero vals, typeBitWidth base) of
        (Just ns, Just width) | fullRange ns width ->
            return $ castVal base $ simplify $ deltaExpr expr steps
        _ -> return $ applyStepsMux expr vals steps
    where
        deltaExpr e n
            | n > 0 = BinOp Add e (RawNum n)
            | otherwise = BinOp Sub e (RawNum (-n))

valsFromZero :: [Expr] -> Maybe [Integer]
valsFromZero = mapM exprToInteger

exprToInteger :: Expr -> Maybe Integer
exprToInteger e = case simplify e of
    Number num -> numberToInteger num
    Cast _ inner -> exprToInteger inner
    _ -> Nothing

typeBitWidth :: Type -> Maybe Integer
typeBitWidth t =
    let (_, rs) = typeRanges t
    in if null rs then Nothing else exprToInteger (dimensionsSize rs)

fullRange :: [Integer] -> Integer -> Bool
fullRange ns width =
    let len = fromIntegral $ length ns
        expected = 2 ^ width
    in len == expected && ns == [0 .. len - 1]

applyStepsMux :: Expr -> [Expr] -> Integer -> Expr
applyStepsMux expr vals steps =
    foldr (\(curr, next) rest -> Mux (BinOp Eq expr curr) next rest)
        (head vals)
        (zip vals rotated)
    where
        len = fromIntegral $ length vals
        n = steps `mod` len
        rotated = drop (fromIntegral n) vals ++ take (fromIntegral n) vals
