{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `struct packed` and `union packed`
 -}

module Convert.Struct (convert) where

import Control.Monad ((>=>), when)
import Data.List (partition)
import Data.Tuple (swap)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type StructInfo = (Type, Map.Map Identifier Range)

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ (Part _ _ Module _ _ _ _)) =
    traverseModuleItems (traverseTypes' ExcludeParamTypes convertType) $
    partScoper traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM
    description
convertDescription other = other

convertStruct :: Type -> Maybe StructInfo
convertStruct (Struct Unpacked fields _) =
    convertStruct' True Unspecified fields
convertStruct (Struct (Packed sg) fields _) =
    convertStruct' True sg fields
convertStruct (Union  (Packed sg) fields _) =
    convertStruct' False sg fields
convertStruct _ = Nothing

convertStruct' :: Bool -> Signing -> [Field] -> Maybe StructInfo
convertStruct' isStruct sg fields =
    if canUnstructure
        then Just (unstructType, unstructFields)
        else Nothing
    where
        zero = Number "0"
        typeRange :: Type -> Range
        typeRange t =
            case ranges of
                [] -> (zero, zero)
                [range] -> range
                _ -> error "Struct.hs invariant failure"
            where ranges = snd $ typeRanges t

        -- extract info about the fields
        fieldTypes = map fst fields
        fieldRanges = map typeRange fieldTypes
        fieldSizes = map rangeSize fieldRanges

        -- layout the fields into the unstructured type; note that `scanr` is
        -- used here because SystemVerilog structs are laid out backwards
        fieldLos =
            if isStruct
                then map simplify $ tail $ scanr (BinOp Add) (Number  "0") fieldSizes
                else map simplify $ repeat (Number "0")
        fieldHis =
            if isStruct
                then map simplify $ init $ scanr (BinOp Add) (Number "-1") fieldSizes
                else map simplify $ map (BinOp Add (Number "-1")) fieldSizes

        -- create the mapping structure for the unstructured fields
        keys = map snd fields
        unstructRanges = zip fieldHis fieldLos
        unstructFields = Map.fromList $ zip keys unstructRanges

        -- create the unstructured type; result type takes on the signing of the
        -- struct itself to preserve behavior of operations on the whole struct
        structSize =
            if isStruct
                then foldl1 (BinOp Add) fieldSizes
                else head fieldSizes
        packedRange = (simplify $ BinOp Sub structSize (Number "1"), zero)
        unstructType = IntegerVector TLogic sg [packedRange]

        -- check if this struct can be packed into an integer vector; we only
        -- pack flat integer vector types; the fields will be flattened and
        -- converted by other phases
        isFlatIntVec :: Type -> Bool
        isFlatIntVec (IntegerVector _ _ rs) = length rs <= 1
        isFlatIntVec _ = False
        canUnstructure = all isFlatIntVec fieldTypes


-- convert a struct type to its unstructured equivalent
convertType :: Type -> Type
convertType t1 =
    case convertStruct t1 of
        Nothing -> t1
        Just (t2, _) -> tf2 (rs1 ++ rs2)
            where (tf2, rs2) = typeRanges t2
    where (_, rs1) = typeRanges t1

-- write down the types of declarations
traverseDeclM :: Decl -> Scoper Type Decl
traverseDeclM decl = do
    decl' <- case decl of
        Variable d t x a e -> do
            let (tf, rs) = typeRanges t
            when (isRangeable t) $
                insertElem x (tf $ a ++ rs)
            let e' = convertExpr t e
            return $ Variable d t x a e'
        Param s t x e -> do
            insertElem x t
            let e' = convertExpr t e
            return $ Param s t x e'
        ParamType{} -> return decl
        CommentDecl{} -> return decl
    traverseDeclExprsM traverseExprM decl'
    where
        isRangeable :: Type -> Bool
        isRangeable IntegerAtom{} = False
        isRangeable NonInteger{}  = False
        isRangeable _ = True

traverseGenItemM :: GenItem -> Scoper Type GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseModuleItemM :: ModuleItem -> Scoper Type ModuleItem
traverseModuleItemM =
    traverseLHSsM  traverseLHSM  >=>
    traverseExprsM traverseExprM >=>
    traverseAsgnsM traverseAsgnM

traverseStmtM :: Stmt -> Scoper Type Stmt
traverseStmtM (Subroutine expr args) = do
    argsMapper <- embedScopes convertCall expr
    let args' = argsMapper args
    let stmt' = Subroutine expr args'
    traverseStmtM' stmt'
traverseStmtM stmt = traverseStmtM' stmt

traverseStmtM' :: Stmt -> Scoper Type Stmt
traverseStmtM' =
    traverseStmtLHSsM  traverseLHSM  >=>
    traverseStmtExprsM traverseExprM >=>
    traverseStmtAsgnsM traverseAsgnM

traverseExprM :: Expr -> Scoper Type Expr
traverseExprM = traverseNestedExprsM $
    embedScopes convertSubExpr >=> return . snd

traverseLHSM :: LHS -> Scoper Type LHS
traverseLHSM = traverseNestedLHSsM $ convertLHS >=> return . snd

-- removes the innermost range from the given type, if possible
dropInnerTypeRange :: Type -> Type
dropInnerTypeRange t =
    case typeRanges t of
        (_, []) -> unknownType
        (tf, rs) -> tf $ tail rs

unknownType :: Type
unknownType = Implicit Unspecified []

traverseAsgnM :: (LHS, Expr) -> Scoper Type (LHS, Expr)
traverseAsgnM (lhs, expr) = do
    -- convert the LHS using the innermost type information
    (typ, lhs') <- convertLHS lhs
    -- convert the RHS using the LHS type information, and then the innermost
    -- type information on the resulting RHS
    (_, expr') <- embedScopes convertSubExpr $ convertExpr typ expr
    return (lhs', expr')

specialTag :: Char
specialTag = ':'
defaultKey :: String
defaultKey = specialTag : "default"

structIsntReady :: Type -> Bool
structIsntReady = (Nothing ==) . convertStruct

-- try expression conversion by looking at the *outermost* type first
convertExpr :: Type -> Expr -> Expr
convertExpr _ Nil = Nil
convertExpr t (Mux c e1 e2) =
    Mux c e1' e2'
    where
        e1' = convertExpr t e1
        e2' = convertExpr t e2
-- TODO: This is really a conversion for using default patterns to
-- populate arrays. Maybe this should be somewhere else?
convertExpr (IntegerVector t sg (r:rs)) (Pattern [(":default", e)]) =
    Repeat (rangeSize r) [e']
    where e' = convertExpr (IntegerVector t sg rs) e
-- TODO: This is a conversion for concat array literals with elements
-- that are unsized numbers. This probably belongs somewhere else.
convertExpr (t @ IntegerVector{}) (Pattern items) =
    if all (null . fst) items
        then convertExpr t $ Concat $ map snd items
        else Pattern items
convertExpr (t @ IntegerVector{}) (Concat exprs) =
    if all isUnsizedNumber exprs
        then Concat exprs'
        else Concat exprs
    where
        caster = Cast (Left $ dropInnerTypeRange t)
        exprs' = map caster exprs
        isUnsizedNumber :: Expr -> Bool
        isUnsizedNumber (Number n) = not $ elem '\'' n
        isUnsizedNumber (UniOp UniSub e) = isUnsizedNumber e
        isUnsizedNumber _ = False
convertExpr (Struct packing fields (_:rs)) (Concat exprs) =
    Concat $ map (convertExpr (Struct packing fields rs)) exprs
convertExpr (Struct packing fields (_:rs)) (Bit e _) =
    convertExpr (Struct packing fields rs) e
convertExpr (Struct packing fields []) (Pattern [("", Repeat (Number nStr) exprs)]) =
    case fmap fromIntegral (readNumber nStr) of
        Just n -> convertExpr (Struct packing fields []) $ Pattern $
                zip (repeat "") (concat $ take n $ repeat exprs)
        Nothing ->
            error $ "unable to handle repeat in pattern: " ++
                (show $ Repeat (Number nStr) exprs)
convertExpr (struct @ (Struct _ fields [])) (Pattern itemsOrig) =
    if extraNames /= Set.empty then
        error $ "pattern " ++ show (Pattern itemsOrig) ++
            " has extra named fields: " ++
            show (Set.toList extraNames) ++ " that are not in " ++ show struct
    else if structIsntReady struct then
        Pattern items
    else
        Concat
            $ map (uncurry $ Cast . Left)
            $ zip (map fst fields) (map snd items)
    where
        fieldNames = map snd fields
        fieldTypeMap = Map.fromList $ map swap fields

        itemsNamed =
            -- patterns either use positions based or name/type/default
            if all ((/= "") . fst) itemsOrig then
                itemsOrig
            -- position-based patterns should cover every field
            else if length itemsOrig /= length fields then
                error $ "struct pattern " ++ show (Pattern itemsOrig) ++
                    " doesn't have the same # of items as " ++ show struct
            -- if the pattern does not use identifiers, use the
            -- identifiers from the struct type definition in order
            else
                zip fieldNames (map snd itemsOrig)
        (specialItems, namedItems) =
            partition ((== specialTag) . head . fst) itemsNamed
        namedItemMap = Map.fromList namedItems
        specialItemMap = Map.fromList specialItems

        extraNames = Set.difference
            (Set.fromList $ map fst namedItems)
            (Map.keysSet fieldTypeMap)

        items = zip fieldNames $ map resolveField fieldNames
        resolveField :: Identifier -> Expr
        resolveField fieldName =
            convertExpr fieldType $
            -- look up by name
            if Map.member fieldName namedItemMap then
                namedItemMap Map.! fieldName
            -- recurse for substructures
            else if isStruct fieldType then
                Pattern specialItems
            -- look up by field type
            else if Map.member fieldTypeName specialItemMap then
                specialItemMap Map.! fieldTypeName
            -- fall back on the default value
            else if Map.member defaultKey specialItemMap then
                specialItemMap Map.! defaultKey
            else
                error $ "couldn't find field " ++ fieldName ++
                    " from struct definition " ++ show struct ++
                    " in struct pattern " ++ show itemsOrig
            where
                fieldType = fieldTypeMap Map.! fieldName
                fieldTypeName =
                    specialTag : (show $ fst $ typeRanges fieldType)
                isStruct :: Type -> Bool
                isStruct (Struct{}) = True
                isStruct _ = False

convertExpr (Struct packing fields (r : rs)) (Pattern items) =
    if all null keys
        then convertExpr (structTf (r : rs)) (Concat vals)
        else Repeat (rangeSize r) [subExpr']
    where
        (keys, vals) = unzip items
        subExpr = Pattern items
        structTf = Struct packing fields
        subExpr' = convertExpr (structTf rs) subExpr
convertExpr (Struct packing fields (r : rs)) subExpr =
    Repeat (rangeSize r) [subExpr']
    where
        structTf = Struct packing fields
        subExpr' = convertExpr (structTf rs) subExpr
convertExpr _ other = other

fallbackType :: Scopes Type -> Expr -> (Type, Expr)
fallbackType scopes e =
    case lookupExpr scopes e of
        Nothing -> (unknownType, e)
        Just (_, _, t) -> (t, e)

-- converting LHSs by looking at the innermost types first
convertLHS :: LHS -> Scoper Type (Type, LHS)
convertLHS l = do
    let e = lhsToExpr l
    (t, e') <- embedScopes convertSubExpr e
    return $ case exprToLHS e' of
        Just l' -> (t, l')
        Nothing -> error $ "struct conversion created non-LHS from "
                    ++ (show e) ++ " to " ++ (show e')

-- try expression conversion by looking at the *innermost* type first
convertSubExpr :: Scopes Type -> Expr -> (Type, Expr)
convertSubExpr scopes (Dot e x) =
    if isntStruct subExprType then
        fallbackType scopes $ Dot e' x
    else if structIsntReady subExprType then
        (fieldType, Dot e' x)
    else
        (fieldType, undotted)
    where
        (subExprType, e') = convertSubExpr scopes e
        (fieldType, bounds, dims) = lookupFieldInfo subExprType x
        base = fst bounds
        len = rangeSize bounds
        undotted = if null dims || rangeSize (head dims) == Number "1"
            then Bit e' (fst bounds)
            else Range e' IndexedMinus (base, len)
convertSubExpr scopes (Range (Dot e x) NonIndexed rOuter) =
    if isntStruct subExprType then
        fallbackType scopes orig'
    else if structIsntReady subExprType then
        (dropInnerTypeRange fieldType, orig')
    else
        (dropInnerTypeRange fieldType, undotted)
    where
        (subExprType, e') = convertSubExpr scopes e
        orig' = Range (Dot e' x) NonIndexed rOuter
        (fieldType, bounds, dims) = lookupFieldInfo subExprType x
        [dim] = dims
        rangeLeft = ( BinOp Sub (fst bounds) $ BinOp Sub (fst dim) (fst rOuter)
                    , BinOp Sub (fst bounds) $ BinOp Sub (fst dim) (snd rOuter) )
        rangeRight =( BinOp Add (snd bounds) $ BinOp Sub (snd dim) (fst rOuter)
                    , BinOp Add (snd bounds) $ BinOp Sub (snd dim) (snd rOuter) )
        undotted = Range e' NonIndexed $
            endianCondRange dim rangeLeft rangeRight
convertSubExpr scopes (Range (Dot e x) mode (baseO, lenO)) =
    if isntStruct subExprType then
        fallbackType scopes orig'
    else if structIsntReady subExprType then
        (dropInnerTypeRange fieldType, orig')
    else
        (dropInnerTypeRange fieldType, undotted)
    where
        (subExprType, e') = convertSubExpr scopes e
        orig' = Range (Dot e' x) mode (baseO, lenO)
        (fieldType, bounds, dims) = lookupFieldInfo subExprType x
        [dim] = dims
        baseLeft  = BinOp Sub (fst bounds) $ BinOp Sub (fst dim) baseO
        baseRight = BinOp Add (snd bounds) $ BinOp Sub (snd dim) baseO
        baseDec = baseLeft
        baseInc = case mode of
            IndexedPlus  -> BinOp Add (BinOp Sub baseRight lenO) one
            IndexedMinus -> BinOp Sub (BinOp Add baseRight lenO) one
            NonIndexed   -> error "invariant violated"
        base = endianCondExpr dim baseDec baseInc
        undotted = Range e' mode (base, lenO)
        one = Number "1"
convertSubExpr scopes (Range e mode r) =
    (dropInnerTypeRange t, Range e' mode r)
    where (t, e') = convertSubExpr scopes e
convertSubExpr scopes (Bit (Dot e x) i) =
    if isntStruct subExprType then
        fallbackType scopes orig'
    else if structIsntReady subExprType then
        (dropInnerTypeRange fieldType, orig')
    else
        (dropInnerTypeRange fieldType, Bit e' i')
    where
        (subExprType, e') = convertSubExpr scopes e
        orig' = Bit (Dot e' x) i
        (fieldType, bounds, dims) = lookupFieldInfo subExprType x
        [dim] = dims
        iLeft  = BinOp Sub (fst bounds) $ BinOp Sub (fst dim) i
        iRight = BinOp Add (snd bounds) $ BinOp Sub (snd dim) i
        i' = endianCondExpr dim iLeft iRight
convertSubExpr scopes (Bit e i) =
    if t == unknownType
        then fallbackType scopes $ Bit e' i
        else (dropInnerTypeRange t, Bit e' i)
    where (t, e') = convertSubExpr scopes e
convertSubExpr scopes (Call e args) =
    (retType, Call e args')
    where
        (retType, _) = fallbackType scopes e
        args' = convertCall scopes e args
convertSubExpr scopes (Cast (Left t) e) =
    (t, Cast (Left t) e')
    where (_, e') = convertSubExpr scopes e
convertSubExpr scopes (Pattern items) =
    if all (== "") $ map fst items'
        then (unknownType, Concat $ map snd items')
        else (unknownType, Pattern items')
    where
        items' = map mapItem items
        mapItem (x, e) = (x, e')
            where (_, e') = convertSubExpr scopes e
convertSubExpr scopes (Mux a b c) =
    (t, Mux a' b' c')
    where
        (_, a') = convertSubExpr scopes a
        (t, b') = convertSubExpr scopes b
        (_, c') = convertSubExpr scopes c
convertSubExpr scopes other =
    fallbackType scopes other

-- get the fields and type function of a struct or union
getFields :: Type -> Maybe [Field]
getFields (Struct _ fields []) = Just fields
getFields (Union  _ fields []) = Just fields
getFields _ = Nothing

isntStruct :: Type -> Bool
isntStruct = (== Nothing) . getFields

-- get the field type, flattended bounds, and original type dimensions
lookupFieldInfo :: Type -> Identifier -> (Type, Range, [Range])
lookupFieldInfo struct fieldName =
    if maybeFieldType == Nothing
        then error $ "field '" ++ fieldName ++ "' not found in: " ++ show struct
        else (fieldType, bounds, dims)
    where
        Just fields = getFields struct
        maybeFieldType = lookup fieldName $ map swap fields
        Just fieldType = maybeFieldType
        dims = snd $ typeRanges fieldType
        Just (_, unstructRanges) = convertStruct struct
        Just bounds = Map.lookup fieldName unstructRanges

-- attempts to convert based on the assignment-like contexts of TF arguments
convertCall :: Scopes Type -> Expr -> Args -> Args
convertCall scopes fn (Args pnArgs kwArgs) =
    case exprToLHS fn of
        Just fnLHS ->
            Args (map snd pnArgs') kwArgs'
            where
                pnArgs' = map (convertArg fnLHS) $ zip idxs pnArgs
                kwArgs' = map (convertArg fnLHS) kwArgs
        _ -> Args pnArgs kwArgs
    where
        idxs = map show ([0..] :: [Int])
        convertArg :: LHS -> (Identifier, Expr) -> (Identifier, Expr)
        convertArg lhs (x, e) =
            (x, e')
            where
                details = lookupLHS scopes $ LHSDot lhs x
                typ = maybe unknownType thd3 details
                thd3 (_, _, c) = c
                (_, e') = convertSubExpr scopes $ convertExpr typ e
