{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `struct packed` and `union packed`
 -}

module Convert.Struct (convert) where

import Control.Monad ((>=>), when)
import Control.Monad.State.Strict (get)
import Data.Either (isLeft)
import Data.List (elemIndex, find, partition, (\\))
import Data.Maybe (fromJust)
import Data.Tuple (swap)

import Convert.ExprUtils
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type StructInfo = (Type, [(Identifier, Range)])

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription description@(Part _ _ Module _ _ _ _) =
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
        zero = RawNum 0
        typeRange :: Type -> Range
        typeRange t =
            if null ranges
                then (zero, zero)
                else let [range] = ranges in range
            where ranges = snd $ typeRanges t

        -- extract info about the fields
        fieldTypes = map fst fields
        fieldRanges = map typeRange fieldTypes
        fieldSizes = map rangeSize fieldRanges

        -- layout the fields into the unstructured type; note that `scanr` is
        -- used here because SystemVerilog structs are laid out backwards
        fieldLos =
            if isStruct
                then map simplify $ tail $ scanr (BinOp Add) (RawNum 0) fieldSizes
                else map simplify $ repeat (RawNum 0)
        fieldHis =
            if isStruct
                then map simplify $ init $ scanr (BinOp Add) minusOne fieldSizes
                else map simplify $ map (BinOp Add minusOne) fieldSizes
        minusOne = UniOp UniSub $ RawNum 1

        -- create the mapping structure for the unstructured fields
        keys = map snd fields
        unstructRanges = zip fieldHis fieldLos
        unstructFields = zip keys unstructRanges

        -- create the unstructured type; result type takes on the signing of the
        -- struct itself to preserve behavior of operations on the whole struct
        structSize =
            if isStruct
                then foldl1 (BinOp Add) fieldSizes
                else head fieldSizes
        packedRange = (simplify $ BinOp Sub structSize (RawNum 1), zero)
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
        Nothing -> traverseSinglyNestedTypes convertType t1
        Just (t2, _) -> tf2 (rs1 ++ rs2)
            where (tf2, rs2) = typeRanges t2
    where (_, rs1) = typeRanges t1

-- write down the types of declarations
traverseDeclM :: Decl -> Scoper Type Decl
traverseDeclM decl@Net{} =
    traverseNetAsVarM traverseDeclM decl
traverseDeclM decl = do
    decl' <- case decl of
        Variable d t x a e -> do
            let (tf, rs) = typeRanges t
            when (isRangeable t) $
                scopeType (tf $ a ++ rs) >>= insertElem x
            scopes <- get
            let e' = convertExpr scopes t e
            let t' = convertType t
            return $ Variable d t' x a e'
        Param s t x e -> do
            scopeType t >>= insertElem x
            scopes <- get
            let e' = convertExpr scopes t e
            let t' = convertType t
            return $ Param s t' x e'
        _ -> return decl
    traverseDeclExprsM traverseExprM decl'
    where
        isRangeable :: Type -> Bool
        isRangeable IntegerAtom{} = False
        isRangeable NonInteger{}  = False
        isRangeable TypeOf{}      = False
        isRangeable TypedefRef{}  = False
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
traverseExprM = embedScopes convertSubExpr >=> return . snd

traverseLHSM :: LHS -> Scoper Type LHS
traverseLHSM = convertLHS >=> return . snd

-- removes the innermost range from the given type, if possible
dropInnerTypeRange :: Type -> Type
dropInnerTypeRange t =
    case typeRanges t of
        (_, []) -> UnknownType
        (tf, rs) -> tf $ tail rs

-- produces the type of the given part select, if possible
replaceInnerTypeRange :: PartSelectMode -> Range -> Type -> Type
replaceInnerTypeRange NonIndexed r t =
    case typeRanges t of
        (_, []) -> UnknownType
        (tf, rs) -> tf $ r : tail rs
replaceInnerTypeRange IndexedPlus r t =
    replaceInnerTypeRange NonIndexed (snd r, RawNum 1) t
replaceInnerTypeRange IndexedMinus r t =
    replaceInnerTypeRange NonIndexed (snd r, RawNum 1) t

traverseAsgnM :: (LHS, Expr) -> Scoper Type (LHS, Expr)
traverseAsgnM (lhs, expr) = do
    -- convert the LHS using the innermost type information
    (typ, lhs') <- convertLHS lhs
    -- convert the RHS using the LHS type information, and then the innermost
    -- type information on the resulting RHS
    scopes <- get
    let (_, expr') =
            convertSubExpr scopes $
            convertExpr scopes typ expr
    return (lhs', expr')

structIsntReady :: Type -> Bool
structIsntReady = (Nothing ==) . convertStruct

-- try expression conversion by looking at the *outermost* type first
convertExpr :: Scopes a -> Type -> Expr -> Expr
convertExpr _ _ Nil = Nil
convertExpr scopes t (Mux c e1 e2) =
    Mux c e1' e2'
    where
        e1' = convertExpr scopes t e1
        e2' = convertExpr scopes t e2

convertExpr scopes struct@(Struct _ fields []) (Pattern itemsOrig) =
    if not (null extraNames) then
        scopedError scopes $ "pattern " ++ show (Pattern itemsOrig) ++
            " has extra named fields " ++ show extraNames ++
            " that are not in " ++ show struct
    else if structIsntReady struct then
        Pattern items
    else
        Concat $ zipWith (Cast . Left) fieldTypes (map snd items)
    where
        (fieldTypes, fieldNames) = unzip fields

        itemsNamed =
            -- patterns either use positions based or name/type/default
            if all ((/= Right Nil) . fst) itemsOrig then
                itemsOrig
            -- position-based patterns should cover every field
            else if length itemsOrig /= length fields then
                scopedError scopes $ "struct pattern " ++
                    show (Pattern itemsOrig) ++
                    " doesn't have the same number of items as " ++ show struct
            -- if the pattern does not use identifiers, use the
            -- identifiers from the struct type definition in order
            else
                zip (map (Right . Ident) fieldNames) (map snd itemsOrig)
        (typedItems, untypedItems) =
            partition (isLeft . fst) $ reverse itemsNamed
        (numberedItems, namedItems) =
            partition (isNumbered . fst) untypedItems

        isNumbered :: TypeOrExpr -> Bool
        isNumbered (Right (Number n)) =
            if maybeIndex == Nothing
                then scopedError scopes msgNonInteger
                else 0 <= index && index < length fieldNames
                        || scopedError scopes msgOutOfBounds
            where
                maybeIndex = fmap fromIntegral $ numberToInteger n
                Just index = maybeIndex
                msgNonInteger = "pattern index " ++ show (Number n)
                    ++ " is not an integer"
                msgOutOfBounds = "pattern index " ++ show index
                    ++ " is out of bounds for " ++ show struct
        isNumbered _ = False

        extraNames = map (getName . right . fst) namedItems \\ fieldNames
        right = \(Right x) -> x
        getName :: Expr -> Identifier
        getName (Ident x) = x
        getName e = scopedError scopes $ "invalid pattern key " ++ show e
                        ++ " is not a type, field name, or index"

        items = zip
            (map (Right . Ident) fieldNames)
            (map resolveField fieldNames)
        resolveField :: Identifier -> Expr
        resolveField fieldName =
            convertExpr scopes fieldType $
            -- look up by name
            if valueByName /= Nothing then
                fromJust valueByName
            -- recurse for substructures
            else if isStruct fieldType then
                Pattern typedItems
            -- look up by field type
            else if valueByType /= Nothing then
                fromJust valueByType
            -- fall back on the default value
            else if valueDefault /= Nothing then
                fromJust valueDefault
            else if valueByIndex /= Nothing then
                fromJust valueByIndex
            else
                scopedError scopes $ "couldn't find field '" ++ fieldName ++
                    "' from struct definition " ++ show struct ++
                    " in struct pattern " ++ show (Pattern itemsOrig)
            where
                valueByName = lookup (Right $ Ident fieldName) namedItems
                valueByType = lookup (Left fieldType) typedItems
                valueDefault = lookup (Left UnknownType) typedItems
                valueByIndex = fmap snd $ find (indexCheck . fst) numberedItems

                fieldType = fst $ fields !! fieldIndex
                Just fieldIndex = elemIndex fieldName fieldNames

                isStruct :: Type -> Bool
                isStruct Struct{} = True
                isStruct _ = False

                indexCheck :: TypeOrExpr -> Bool
                indexCheck item =
                    fromIntegral value == fieldIndex
                    where
                        Just value = numberToInteger n
                        Right (Number n) = item

convertExpr scopes _ (Cast (Left t) expr) =
    Cast (Left t') $ convertExpr scopes t expr
    where t' = convertType t

convertExpr scopes (Implicit _ []) expr =
    traverseSinglyNestedExprs (convertExpr scopes UnknownType) expr
convertExpr scopes (Implicit sg rs) expr =
    convertExpr scopes (IntegerVector TBit sg rs) expr

-- TODO: This is a conversion for concat array literals with elements
-- that are unsized numbers. This probably belongs somewhere else.
convertExpr scopes t@IntegerVector{} (Concat exprs) =
    if all isUnsizedNumber exprs
        then Concat $ map (Cast $ Left t') exprs
        else Concat $ map (convertExpr scopes t') exprs
    where
        t' = dropInnerTypeRange t
        isUnsizedNumber :: Expr -> Bool
        isUnsizedNumber (Number n) = not $ numberIsSized n
        isUnsizedNumber (UniOp _ e) = isUnsizedNumber e
        isUnsizedNumber (BinOp _ e1 e2) =
            isUnsizedNumber e1 || isUnsizedNumber e2
        isUnsizedNumber _ = False

-- TODO: This is really a conversion for using default patterns to
-- populate arrays. Maybe this should be somewhere else?
convertExpr scopes t orig@(Pattern [(Left UnknownType, expr)]) =
    if null rs
        then orig
        else Repeat count [expr']
    where
        count = rangeSize $ head rs
        expr' = Cast (Left t') $ convertExpr scopes t' expr
        (_, rs) = typeRanges t
        t' = dropInnerTypeRange t

-- pattern syntax used for simple array literals
convertExpr scopes t (Pattern items) =
    if all (== Right Nil) names
        then convertExpr scopes t $ Concat exprs'
        else Pattern items
    where
        (names, exprs) = unzip items
        t' = dropInnerTypeRange t
        exprs' = map (convertExpr scopes t') exprs

-- propagate types through concatenation expressions
convertExpr scopes t (Concat exprs) =
    Concat exprs'
    where
        t' = dropInnerTypeRange t
        exprs' = map (convertExpr scopes t') exprs

convertExpr scopes _ expr =
    traverseSinglyNestedExprs (convertExpr scopes UnknownType) expr

fallbackType :: Scopes Type -> Expr -> (Type, Expr)
fallbackType scopes e =
    (t, e)
    where
        t = case lookupElem scopes e of
                Nothing -> UnknownType
                Just (_, _, typ) -> typ

pattern MakeSigned :: Expr -> Expr
pattern MakeSigned e = Call (Ident "$signed") (Args [e] [])

-- converting LHSs by looking at the innermost types first
convertLHS :: LHS -> Scoper Type (Type, LHS)
convertLHS l = do
    let e = lhsToExpr l
    (t, e') <- embedScopes convertSubExpr e
    -- per IEEE 1800-2017 sections 10.7 and 11.8, the signedness of the LHS does
    -- not affect the evaluation and sign-extension of the RHS
    let Just l' = exprToLHS $ case e' of
                        MakeSigned e'' -> e''
                        _ -> e'
    return (t, l')

-- try expression conversion by looking at the *innermost* type first
convertSubExpr :: Scopes Type -> Expr -> (Type, Expr)
convertSubExpr scopes (Dot e x) =
    if isntStruct subExprType then
        fallbackType scopes $ Dot e' x
    else if structIsntReady subExprType then
        (fieldType, Dot e' x)
    else
        (fieldType, undottedWithSign)
    where
        (subExprType, e') = convertSubExpr scopes e
        (fieldType, bounds, dims) = lookupFieldInfo scopes subExprType e' x
        base = fst bounds
        len = rangeSize bounds
        undotted = if null dims || rangeSize (head dims) == RawNum 1
            then Bit e' (fst bounds)
            else Range e' IndexedMinus (base, len)
        -- retain signedness of fields which would otherwise be lost via the
        -- resulting bit or range selection
        IntegerVector _ fieldSg _ = fieldType
        undottedWithSign =
            if fieldSg == Signed
                then MakeSigned undotted
                else undotted

convertSubExpr scopes (Range (Dot e x) NonIndexed rOuter) =
    if isntStruct subExprType then
        (UnknownType, orig')
    else if structIsntReady subExprType then
        (replaceInnerTypeRange NonIndexed rOuter' fieldType, orig')
    else if null dims then
        scopedError scopes $ "illegal access to range "
            ++ show (Range Nil NonIndexed rOuter) ++ " of " ++ show (Dot e x)
            ++ ", which has type " ++ show fieldType
    else
        (replaceInnerTypeRange NonIndexed rOuter' fieldType, undotted)
    where
        (roLeft, roRight) = rOuter
        (subExprType, e') = convertSubExpr scopes e
        (_, roLeft') = convertSubExpr scopes roLeft
        (_, roRight') = convertSubExpr scopes roRight
        rOuter' = (roLeft', roRight')
        orig' = Range (Dot e' x) NonIndexed rOuter'
        (fieldType, bounds, dims) = lookupFieldInfo scopes subExprType e' x
        [dim] = dims
        rangeLeft = ( BinOp Sub (fst bounds) $ BinOp Sub (fst dim) roLeft'
                    , BinOp Sub (fst bounds) $ BinOp Sub (fst dim) roRight' )
        rangeRight =( BinOp Add (snd bounds) $ BinOp Sub (snd dim) roLeft'
                    , BinOp Add (snd bounds) $ BinOp Sub (snd dim) roRight' )
        undotted = Range e' NonIndexed $
            endianCondRange dim rangeLeft rangeRight
convertSubExpr scopes (Range (Dot e x) mode (baseO, lenO)) =
    if isntStruct subExprType then
        (UnknownType, orig')
    else if structIsntReady subExprType then
        (replaceInnerTypeRange mode (baseO', lenO') fieldType, orig')
    else if null dims then
        scopedError scopes $ "illegal access to range "
            ++ show (Range Nil mode (baseO, lenO)) ++ " of " ++ show (Dot e x)
            ++ ", which has type " ++ show fieldType
    else
        (replaceInnerTypeRange mode (baseO', lenO') fieldType, undotted)
    where
        (subExprType, e') = convertSubExpr scopes e
        (_, baseO') = convertSubExpr scopes baseO
        (_, lenO') = convertSubExpr scopes lenO
        orig' = Range (Dot e' x) mode (baseO', lenO')
        (fieldType, bounds, dims) = lookupFieldInfo scopes subExprType e' x
        [dim] = dims
        baseLeft  = BinOp Sub (fst bounds) $ BinOp Sub (fst dim) baseO'
        baseRight = BinOp Add (snd bounds) $ BinOp Sub (snd dim) baseO'
        baseDec = baseLeft
        baseInc = if mode == IndexedPlus
            then BinOp Add (BinOp Sub baseRight lenO') one
            else BinOp Sub (BinOp Add baseRight lenO') one
        base = endianCondExpr dim baseDec baseInc
        undotted = Range e' mode (base, lenO')
        one = RawNum 1
convertSubExpr scopes (Range e mode (left, right)) =
    (replaceInnerTypeRange mode r' t, Range e' mode r')
    where
        (t, e') = convertSubExpr scopes e
        (_, left') = convertSubExpr scopes left
        (_, right') = convertSubExpr scopes right
        r' = (left', right')
convertSubExpr scopes (Bit (Dot e x) i) =
    if isntStruct subExprType then
        (dropInnerTypeRange backupType, orig')
    else if structIsntReady subExprType then
        (dropInnerTypeRange fieldType, orig')
    else if null dims then
        scopedError scopes $ "illegal access to bit " ++ show i ++ " of "
            ++ show (Dot e x) ++ ", which has type " ++ show fieldType
    else
        (dropInnerTypeRange fieldType, Bit e' iFlat)
    where
        (subExprType, e') = convertSubExpr scopes e
        (_, i') = convertSubExpr scopes i
        (backupType, _) = fallbackType scopes $ Dot e' x
        orig' = Bit (Dot e' x) i'
        (fieldType, bounds, dims) = lookupFieldInfo scopes subExprType e' x
        [dim] = dims
        left  = BinOp Sub (fst bounds) $ BinOp Sub (fst dim) i'
        right = BinOp Add (snd bounds) $ BinOp Sub (snd dim) i'
        iFlat = endianCondExpr dim left right
convertSubExpr scopes (Bit e i) =
    if t == UnknownType
        then (UnknownType, Bit e' i')
        else (dropInnerTypeRange t, Bit e' i')
    where
        (t, e') = convertSubExpr scopes e
        (_, i') = convertSubExpr scopes i
convertSubExpr scopes (Call e args) =
    (retType, Call e args')
    where
        (retType, _) = fallbackType scopes e
        args' = convertCall scopes e args
convertSubExpr scopes (Cast (Left t) e) =
    (t, Cast (Left t) e')
    where (_, e') = convertSubExpr scopes e
convertSubExpr scopes (Pattern items) =
    if all (== Right Nil) $ map fst items'
        then (UnknownType, Concat $ map snd items')
        else (UnknownType, Pattern items')
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
convertSubExpr scopes (Ident x) =
    fallbackType scopes (Ident x)
convertSubExpr scopes e =
    (UnknownType, ) $
        traverseExprTypes typeMapper $
        traverseSinglyNestedExprs exprMapper e
    where
        exprMapper = snd . convertSubExpr scopes
        typeMapper = convertType .
            traverseNestedTypes (traverseTypeExprs exprMapper)

-- get the fields and type function of a struct or union
getFields :: Type -> Maybe [Field]
getFields (Struct _ fields []) = Just fields
getFields (Union  _ fields []) = Just fields
getFields _ = Nothing

isntStruct :: Type -> Bool
isntStruct = (== Nothing) . getFields

-- get the field type, flattened bounds, and original type dimensions
lookupFieldInfo :: Scopes Type -> Type -> Expr -> Identifier
    -> (Type, Range, [Range])
lookupFieldInfo scopes struct base fieldName =
    if maybeFieldType == Nothing
        then scopedError scopes $ "field '" ++ fieldName ++ "' not found in "
                ++ show struct ++ ", in expression "
                ++ show (Dot base fieldName)
        else (fieldType, bounds, dims)
    where
        Just fields = getFields struct
        maybeFieldType = lookup fieldName $ map swap fields
        Just fieldType = maybeFieldType
        dims = snd $ typeRanges fieldType
        Just (_, unstructRanges) = convertStruct struct
        Just bounds = lookup fieldName unstructRanges

-- attempts to convert based on the assignment-like contexts of TF arguments
convertCall :: Scopes Type -> Expr -> Args -> Args
convertCall scopes fn (Args pnArgs kwArgs) =
    Args (map snd pnArgs') kwArgs'
    where
        Just fnLHS = exprToLHS fn
        pnArgs' = map (convertArg fnLHS) $ zip idxs pnArgs
        kwArgs' = map (convertArg fnLHS) kwArgs
        idxs = map show ([0..] :: [Int])
        convertArg :: LHS -> (Identifier, Expr) -> (Identifier, Expr)
        convertArg lhs (x, e) =
            (x, e')
            where
                details = lookupElem scopes $ LHSDot lhs x
                typ = maybe UnknownType thd3 details
                thd3 (_, _, c) = c
                (_, e') = convertSubExpr scopes $ convertExpr scopes typ e
