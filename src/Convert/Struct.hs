{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `struct packed` and `union packed`
 -}

module Convert.Struct (convert) where

import Control.Monad.State
import Control.Monad.Writer
import Data.List (partition)
import Data.Tuple (swap)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type TypeFunc = [Range] -> Type
type StructInfo = (Type, Map.Map Identifier (Range, Expr))
type Structs = Map.Map TypeFunc StructInfo
type Types = Map.Map Identifier Type
type Idents = Set.Set Identifier

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ Part{}) =
    traverseModuleItems (traverseTypes' ExcludeParamTypes $ convertType structs) $
    Part attrs extern kw lifetime name ports (items ++ funcs)
    where
        description' @ (Part attrs extern kw lifetime name ports items) =
            scopedConversion (traverseDeclM structs) traverseModuleItemM
                traverseStmtM tfArgTypes description
        -- collect information about this description
        structs = execWriter $ collectModuleItemsM
            (collectTypesM collectStructM) description
        tfArgTypes = execWriter $ collectModuleItemsM collectTFArgsM description
        -- determine which of the packer functions we actually need
        calledFuncs = execWriter $ collectModuleItemsM
            (collectExprsM $ collectNestedExprsM collectCallsM) description'
        packerFuncs = Set.map packerFnName $ Map.keysSet structs
        calledPackedFuncs = Set.intersection calledFuncs packerFuncs
        funcs = map packerFn $ filter isNeeded $ Map.keys structs
        isNeeded tf = Set.member (packerFnName tf) calledPackedFuncs
        -- helpers for the scoped traversal
        traverseModuleItemM :: ModuleItem -> State Types ModuleItem
        traverseModuleItemM item =
            traverseLHSsM  traverseLHSM  item >>=
            traverseExprsM traverseExprM      >>=
            traverseAsgnsM traverseAsgnM
        traverseStmtM :: Stmt -> State Types Stmt
        traverseStmtM (Subroutine expr args) = do
            stateTypes <- get
            let stmt' = Subroutine expr $ convertCall
                            structs stateTypes expr args
            traverseStmtLHSsM  traverseLHSM  stmt' >>=
                traverseStmtExprsM traverseExprM   >>=
                traverseStmtAsgnsM traverseAsgnM
        traverseStmtM stmt =
            traverseStmtLHSsM  traverseLHSM  stmt >>=
            traverseStmtExprsM traverseExprM      >>=
            traverseStmtAsgnsM traverseAsgnM
        traverseExprM =
            traverseNestedExprsM $ stately converter
            where
                converter :: Types -> Expr -> Expr
                converter types expr =
                    snd $ convertAsgn structs types (LHSIdent "", expr)
        traverseLHSM =
            traverseNestedLHSsM $ stately converter
            where
                converter :: Types -> LHS -> LHS
                converter types lhs =
                    fst $ convertAsgn structs types (lhs, Ident "")
        traverseAsgnM = stately $ convertAsgn structs
convertDescription other = other

-- write down unstructured versions of packed struct types
collectStructM :: Type -> Writer Structs ()
collectStructM (Struct Unpacked fields _) =
    collectStructM' (Struct Unpacked) True Unspecified fields
collectStructM (Struct (Packed sg) fields _) =
    collectStructM' (Struct $ Packed sg) True sg fields
collectStructM (Union  (Packed sg) fields _) =
    collectStructM' (Union $ Packed sg) False sg fields
collectStructM _ = return ()

collectStructM'
    :: ([Field] -> [Range] -> Type)
    -> Bool -> Signing -> [Field] -> Writer Structs ()
collectStructM' constructor isStruct sg fields = do
    if canUnstructure
        then tell $ Map.singleton
            (constructor fields)
            (unstructType, unstructFields)
        else return ()
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
        unstructOffsets = map simplify $ map snd fieldRanges
        unstructRanges = zip fieldHis fieldLos
        keys = map snd fields
        vals = zip unstructRanges unstructOffsets
        unstructFields = Map.fromList $ zip keys vals

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
convertType :: Structs -> Type -> Type
convertType structs t1 =
    case Map.lookup tf1 structs of
        Nothing -> t1
        Just (t2, _) -> tf2 (rs1 ++ rs2)
            where (tf2, rs2) = typeRanges t2
    where (tf1, rs1) = typeRanges t1

-- writes down the names of called functions
collectCallsM :: Expr -> Writer Idents ()
collectCallsM (Call (Ident f) _) = tell $ Set.singleton f
collectCallsM _ = return ()

collectTFArgsM :: ModuleItem -> Writer Types ()
collectTFArgsM (MIPackageItem item) = do
    _ <- case item of
        Function _ t f decls _ -> do
            tell $ Map.singleton f t
            mapM (collect f) (zip [0..] decls)
        Task     _   f decls _ ->
            mapM (collect f) (zip [0..] decls)
        _ -> return []
    return ()
    where
        collect :: Identifier -> (Int, Decl) -> Writer Types ()
        collect f (idx, (Variable _ t x _ _)) = do
            tell $ Map.singleton (f ++ ":" ++ show idx) t
            tell $ Map.singleton (f ++ ":" ++ x) t
        collect _ _ = return ()
collectTFArgsM _ = return ()

-- write down the types of declarations
traverseDeclM :: Structs -> Decl -> State Types Decl
traverseDeclM structs origDecl = do
    case origDecl of
        Variable d t x a me -> do
            let (tf, rs) = typeRanges t
            if isRangeable t
                then modify $ Map.insert x (tf $ a ++ rs)
                else return ()
            case me of
                Nothing -> return origDecl
                Just e -> do
                    e' <- convertDeclExpr x e
                    return $ Variable d t x a (Just e')
        Param s t x e -> do
            modify $ Map.insert x t
            e' <- convertDeclExpr x e
            return $ Param s t x e'
        ParamType{} -> return origDecl
        CommentDecl{} -> return origDecl
    where
        convertDeclExpr :: Identifier -> Expr -> State Types Expr
        convertDeclExpr x e = do
            types <- get
            let (LHSIdent _, e') = convertAsgn structs types (LHSIdent x, e)
            return e'
        isRangeable :: Type -> Bool
        isRangeable (IntegerAtom _ _) = False
        isRangeable (NonInteger  _  ) = False
        isRangeable _ = True

-- produces a function which packs the components of a struct literal
packerFn :: TypeFunc -> ModuleItem
packerFn structTf =
    MIPackageItem $
    Function Automatic (structTf []) fnName decls [retStmt]
    where
        Struct _ fields [] = structTf []
        toInput (t, x) = Variable Input t x [] Nothing
        decls = map toInput fields
        retStmt = Return $ Concat $ map (Ident . snd) fields
        fnName = packerFnName structTf

-- returns a "unique" name for the packer for a given struct type
packerFnName :: TypeFunc -> Identifier
packerFnName structTf =
    "sv2v_struct_" ++ shortHash structTf

-- removes the innermost range from the given type, if possible
dropInnerTypeRange :: Type -> Type
dropInnerTypeRange t =
    case typeRanges t of
        (_, []) -> Implicit Unspecified []
        (tf, rs) -> tf $ tail rs

-- This is where the magic happens. This is responsible for converting struct
-- accesses, assignments, and literals, given appropriate information about the
-- structs and the current declaration context. The general strategy involves
-- looking at the innermost type of a node to convert outer uses of fields, and
-- then using the outermost type to figure out the corresponding struct
-- definition for struct literals that are encountered.
convertAsgn :: Structs -> Types -> (LHS, Expr) -> (LHS, Expr)
convertAsgn structs types (lhs, expr) =
    (lhs', expr')
    where
        (typ, lhs') = convertLHS lhs
        expr' = snd $ convertSubExpr $ convertExpr typ expr

        -- converting LHSs by looking at the innermost types first
        convertLHS :: LHS -> (Type, LHS)
        convertLHS l =
            (t, l')
            where
                e = lhsToExpr l
                (t, e') = convertSubExpr e
                Just l' = exprToLHS e'

        specialTag = ':'
        defaultKey = specialTag : "default"

        -- try expression conversion by looking at the *outermost* type first
        convertExpr :: Type -> Expr -> Expr
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
        convertExpr (t @ IntegerVector{}) (Concat exprs) =
            if all isUnsizedNumber exprs
                then Concat exprs'
                else Concat exprs
            where
                size = DimsFn FnBits (Left $ dropInnerTypeRange t)
                caster = Cast (Right size)
                exprs' = map caster exprs
                isUnsizedNumber :: Expr -> Bool
                isUnsizedNumber (Number n) = not $ elem '\'' n
                isUnsizedNumber _ = False
        convertExpr (Struct packing fields (_:rs)) (Concat exprs) =
            Concat $ map (convertExpr (Struct packing fields rs)) exprs
        convertExpr (Struct packing fields (_:rs)) (Bit e _) =
            convertExpr (Struct packing fields rs) e
        convertExpr (Struct packing fields []) (Pattern [("", Repeat (Number nStr) exprs)]) =
            case readNumber nStr of
                Just n -> convertExpr (Struct packing fields []) $ Pattern $
                        zip (repeat "") (concat $ take n $ repeat exprs)
                Nothing ->
                    error $ "unable to handle repeat in pattern: " ++
                        (show $ Repeat (Number nStr) exprs)
        convertExpr (Struct packing fields []) (Pattern itemsOrig) =
            if extraNames /= Set.empty then
                error $ "pattern " ++ show (Pattern itemsOrig) ++
                    " has extra named fields: " ++
                    show (Set.toList extraNames) ++ " that are not in " ++
                    show structTf
            else if Map.member structTf structs then
                Call
                    (Ident $ packerFnName structTf)
                    (Args (map (Just . snd) items) [])
            else
                Pattern items
            where
                structTf = Struct packing fields
                fieldNames = map snd fields
                fieldTypeMap = Map.fromList $ map swap fields

                itemsNamed =
                    -- patterns either use positions based or name/type/default
                    if all ((/= "") . fst) itemsOrig then
                        itemsOrig
                    -- position-based patterns should cover every field
                    else if length itemsOrig /= length fields then
                        error $ "struct pattern " ++ show (Pattern itemsOrig) ++
                            " doesn't have the same # of items as " ++
                            show structTf
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
                            " from struct definition " ++ show structTf ++
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

        -- try expression conversion by looking at the *innermost* type first
        convertSubExpr :: Expr -> (Type, Expr)
        convertSubExpr (Ident x) =
            case Map.lookup x types of
                Nothing -> (Implicit Unspecified [], Ident x)
                Just t -> (t, Ident x)
        convertSubExpr (Dot e x) =
            case subExprType of
                Struct p fields [] -> undot (Struct p fields) fields
                Union  p fields [] -> undot (Union  p fields) fields
                _ -> (Implicit Unspecified [], Dot e' x)
            where
                (subExprType, e') = convertSubExpr e
                undot structTf fields =
                    if Map.notMember structTf structs
                        then (fieldType, Dot e' x)
                        else (fieldType, Range  e' NonIndexed r)
                    where
                        fieldType = lookupFieldType fields x
                        r = lookupUnstructRange structTf x
        convertSubExpr (Range eOuter NonIndexed (rOuter @ (hiO, loO))) =
            -- VCS doesn't allow ranges to be cascaded, so we need to combine
            -- nested Ranges into a single range. My understanding of the
            -- semantics are that a range returns a new, zero-indexed sub-range.
            case eOuter' of
                Range eInner NonIndexed (_, loI) ->
                    (t', Range eInner NonIndexed (simplify hi, simplify lo))
                    where
                        lo = BinOp Add loI loO
                        hi = BinOp Add loI hiO
                Range eInner IndexedPlus (baseI, _) ->
                    (t', Range eInner IndexedPlus (simplify base, simplify len))
                    where
                        base = BinOp Add baseI loO
                        len = rangeSize rOuter
                _ -> (t', Range eOuter' NonIndexed rOuter)
            where
                (t, eOuter') = convertSubExpr eOuter
                t' = dropInnerTypeRange t
        convertSubExpr (Range eOuter IndexedPlus (rOuter @ (baseO, lenO))) =
            case eOuter' of
                Range eInner NonIndexed (hiI, loI) ->
                    (t', Range eInner IndexedPlus (simplify base, simplify len))
                    where
                        base = BinOp Add baseO $
                            endianCondExpr (hiI, loI) loI hiI
                        len = lenO
                _ -> (t', Range eOuter' IndexedPlus rOuter)
            where
                (t, eOuter') = convertSubExpr eOuter
                t' = dropInnerTypeRange t
        convertSubExpr (Range e m r) =
            (t', Range e' m r)
            where
                (t, e') = convertSubExpr e
                t' = dropInnerTypeRange t
        convertSubExpr (Concat exprs) =
            (Implicit Unspecified [], Concat $ map (snd . convertSubExpr) exprs)
        convertSubExpr (Stream o e exprs) =
            (Implicit Unspecified [], Stream o e' exprs')
            where
                e' = (snd . convertSubExpr) e
                exprs' = map (snd . convertSubExpr) exprs
        convertSubExpr (BinOp op e1 e2) =
            (Implicit Unspecified [], BinOp op e1' e2')
            where
                (_, e1') = convertSubExpr e1
                (_, e2') = convertSubExpr e2
        convertSubExpr (Bit e i) =
            case e' of
                Range eInner NonIndexed (_, loI) ->
                    (t', Bit eInner (simplify $ BinOp Add loI i'))
                Range eInner IndexedPlus (baseI, _) ->
                    (t', Bit eInner (simplify $ BinOp Add baseI i'))
                _ -> (t', Bit e' i')
            where
                (t, e') = convertSubExpr e
                t' = dropInnerTypeRange t
                (_, i') = convertSubExpr i
        convertSubExpr (Call e args) =
            (retType, Call e $ convertCall structs types e' args)
            where
                (_, e') = convertSubExpr e
                retType = case e' of
                    Ident f -> case Map.lookup f types of
                        Nothing -> Implicit Unspecified []
                        Just t -> t
                    _ -> Implicit Unspecified []
        convertSubExpr (String s) = (Implicit Unspecified [], String s)
        convertSubExpr (Number n) = (Implicit Unspecified [], Number n)
        convertSubExpr (Time   n) = (Implicit Unspecified [], Time   n)
        convertSubExpr (PSIdent x y) = (Implicit Unspecified [], PSIdent x y)
        convertSubExpr (Repeat e es) =
            (Implicit Unspecified [], Repeat e' es')
            where
                (_, e') = convertSubExpr e
                es' = map (snd . convertSubExpr) es
        convertSubExpr (UniOp op e) =
            (Implicit Unspecified [], UniOp op e')
            where (_, e') = convertSubExpr e
        convertSubExpr (Mux a b c) =
            (t, Mux a' b' c')
            where
                (_, a') = convertSubExpr a
                (t, b') = convertSubExpr b
                (_, c') = convertSubExpr c
        convertSubExpr (Cast (Left t) sub) =
            (t, Cast (Left t) (snd $ convertSubExpr sub))
        convertSubExpr (Cast (Right e) sub) =
            (Implicit Unspecified [], Cast (Right e) (snd $ convertSubExpr sub))
        convertSubExpr (DimsFn f tore) =
            (Implicit Unspecified [], DimsFn f tore')
            where tore' = convertTypeOrExpr tore
        convertSubExpr (DimFn f tore e) =
            (Implicit Unspecified [], DimFn f tore' e')
            where
                tore' = convertTypeOrExpr tore
                e' = snd $ convertSubExpr e
        convertSubExpr (Pattern items) =
            if all (== "") $ map fst items'
                then (Implicit Unspecified [], Concat $ map snd items')
                else (Implicit Unspecified [], Pattern items')
            where
                items' = map mapItem items
                mapItem (mx, e) = (mx, snd $ convertSubExpr e)
        convertSubExpr (Inside e l) =
            (t, Inside e' l')
            where
                t = IntegerVector TLogic Unspecified []
                (_, e') = convertSubExpr e
                l' = map mapItem l
                mapItem :: ExprOrRange -> ExprOrRange
                mapItem (Left a) = Left $ snd $ convertSubExpr a
                mapItem (Right (a, b)) = Right (a', b')
                    where
                        (_, a') = convertSubExpr a
                        (_, b') = convertSubExpr b
        convertSubExpr (MinTypMax a b c) =
            (t, MinTypMax a' b' c')
            where
                (_, a') = convertSubExpr a
                (t, b') = convertSubExpr b
                (_, c') = convertSubExpr c
        convertSubExpr Nil = (Implicit Unspecified [], Nil)

        convertTypeOrExpr :: TypeOrExpr -> TypeOrExpr
        convertTypeOrExpr (Left t) = Left t
        convertTypeOrExpr (Right e) = Right $ snd $ convertSubExpr e

        -- lookup the range of a field in its unstructured type
        lookupUnstructRange :: TypeFunc -> Identifier -> Range
        lookupUnstructRange structTf fieldName =
            case Map.lookup fieldName fieldRangeMap of
                Nothing -> error $ "field '" ++ fieldName ++
                    "' not found in struct: " ++ show structTf
                Just r -> r
            where fieldRangeMap = Map.map fst $ snd $ structs Map.! structTf

        -- lookup the type of a field in the given field list
        lookupFieldType :: [(Type, Identifier)] -> Identifier -> Type
        lookupFieldType fields fieldName = fieldMap Map.! fieldName
            where fieldMap = Map.fromList $ map swap fields

-- attempts to convert based on the assignment-like contexts of TF arguments
convertCall :: Structs -> Types -> Expr -> Args -> Args
convertCall structs types fn (Args pnArgs kwArgs) =
    case fn of
        Ident _ -> args
        _ -> Args pnArgs kwArgs
    where
        Ident f = fn
        idxs = map show ([0..] :: [Int])
        args = Args
            (map snd $ map convertArg $ zip idxs pnArgs)
            (map convertArg kwArgs)
        convertArg :: (Identifier, Maybe Expr) -> (Identifier, Maybe Expr)
        convertArg (x, Nothing) = (x, Nothing)
        convertArg (x, Just e ) = (x, Just e')
            where
                (_, e') = convertAsgn structs types
                    (LHSIdent $ f ++ ":" ++ x, e)

