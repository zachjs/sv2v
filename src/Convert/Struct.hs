{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `packed struct`
 -}

module Convert.Struct (convert) where

import Data.Maybe (isJust)
import Data.List (sortOn)
import Data.Tuple (swap)
import Control.Monad.Writer
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type TypeFunc = [Range] -> Type
type StructInfo = (Type, Map.Map Identifier (Range, Expr))
type Structs = Map.Map TypeFunc StructInfo
type Types = Map.Map Identifier Type

convert :: AST -> AST
convert = traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription description =
    traverseModuleItems (traverseExprs $ traverseNestedExprs $ convertOnlyExpr structs types) $
    traverseModuleItems (traverseTypes $ convertType structs) $
    traverseModuleItems (traverseAsgns $ convertAsgn structs types) $
    description
    where
        structs = execWriter $ collectModuleItemsM
            (collectTypesM collectType) description
        typesA = execWriter $ collectModuleItemsM
            (collectDeclsM collectDecl) description
        typesB = execWriter $ collectModuleItemsM
            collectFunction description
        types = Map.union typesA typesB


-- write down unstructured versions of a packed struct type
collectType :: Type -> Writer Structs ()
collectType (Struct (Packed sg) fields _) = do
    -- TODO: How should we combine the structs Signing with that of the types it
    -- contains?
    if canUnstructure
        then tell $ Map.singleton
            (Struct (Packed sg) fields)
            (unstructType, unstructFields)
        else return ()
    where
        zero = Number "0"
        typeRange :: Type -> Range
        typeRange t =
            if null ranges then (zero, zero) else head ranges
            where ranges = snd $ typeRanges t

        -- extract info about the fields
        fieldTypes = map fst fields
        fieldRanges = map typeRange fieldTypes
        fieldSizes = map rangeSize fieldRanges

        -- layout the fields into the unstructured type; note that `scanr` is
        -- used here because SystemVerilog structs are laid out backwards
        fieldLos = map simplify $ tail $ scanr (BinOp Add) (Number  "0") fieldSizes
        fieldHis = map simplify $ init $ scanr (BinOp Add) (Number "-1") fieldSizes

        -- create the mapping structure for the unstructured fields
        unstructOffsets = map simplify $ map snd fieldRanges
        unstructRanges = zip fieldHis fieldLos
        keys = map snd fields
        vals = zip unstructRanges unstructOffsets
        unstructFields = Map.fromList $ zip keys vals

        -- create the unstructured type
        tf = fst $ typeRanges $ head fieldTypes
        structSize = foldl1 (BinOp Add) fieldSizes
        packedRange = (simplify $ BinOp Sub structSize (Number "1"), zero)
        unstructType = tf [packedRange]

        -- TODO: For now, we only convert packed structs which contain fields
        -- with all the same base type. We might be able to get away with
        -- converting everything to a Logic type. This should work in cases of
        -- mixed `wire`/`logic` or `reg`/`logic`.
        fieldClasses = map (show . fst . typeRanges) fieldTypes
        isComplex :: Type -> Bool
        isComplex (Struct _ _ _ ) = True
        isComplex (Enum _ _ _ ) = True
        isComplex (Alias _ _) = True
        isComplex _ = False
        canUnstructure =
            all (head fieldClasses ==) fieldClasses &&
            not (any isComplex fieldTypes)

collectType _ = return ()


-- convert a struct type to its unstructured equivalent
convertType :: Structs -> Type -> Type
convertType structs t1 =
    case Map.lookup tf1 structs of
        Nothing -> t1
        Just (t2, _) -> tf2 (rs2 ++ rs1)
            where (tf2, rs2) = typeRanges t2
    where (tf1, rs1) = typeRanges t1


-- write down the type a declarations
collectDecl :: Decl -> Writer Types ()
collectDecl (Variable _ (Implicit _ []) _ _ _) = return ()
collectDecl (Variable _ t x a _) =
    -- We add the unpacked dimensions to the type so that our type traversal can
    -- correctly match-off the dimensions whenever we see a `Bit` or `Range`
    -- expression.
    tell $ Map.singleton x (tf $ rs ++ a)
    where (tf, rs) = typeRanges t
collectDecl (Parameter  t x _) = tell $ Map.singleton x t
collectDecl (Localparam t x _) = tell $ Map.singleton x t

-- write down the return type of a function
collectFunction :: ModuleItem -> Writer Types ()
collectFunction (MIPackageItem (Function _ t f _ _)) = tell $ Map.singleton f t
collectFunction _ = return ()

convertOnlyExpr :: Structs -> Types -> Expr -> Expr
convertOnlyExpr structs types expr =
    snd $ convertAsgn structs types (LHSIdent "", expr)

convertAsgn :: Structs -> Types -> (LHS, Expr) -> (LHS, Expr)
convertAsgn structs types (lhs, expr) =
    (lhs', expr')
    where
        (typ, lhs') = convertLHS lhs
        expr' = snd $ convertSubExpr $ convertExpr typ expr

        -- converting LHSs by looking at the innermost types first
        convertLHS :: LHS -> (Type, LHS)
        convertLHS (LHSIdent  x) =
            case Map.lookup x types of
                Nothing -> (Implicit Unspecified [], LHSIdent x)
                Just t -> (t, LHSIdent x)
        convertLHS (LHSBit l e) =
            if null rs
                then (Implicit Unspecified [], LHSBit l' e)
                else (tf $ tail rs, LHSBit l' e)
            where
                (t, l') = convertLHS l
                (tf, rs) = typeRanges t
        convertLHS (LHSRange l r ) =
            if null rs
                then (Implicit Unspecified [], LHSRange l' r)
                else (tf rs', LHSRange l' r)
            where
                (t, l') = convertLHS l
                (tf, rs) = typeRanges t
                rs' = r : tail rs
        convertLHS (LHSDot    l x ) =
            case t of
                InterfaceT _ _ _ -> (Implicit Unspecified [], LHSDot l' x)
                Struct _ _ _ -> case Map.lookup structTf structs of
                    Nothing -> (fieldType, LHSDot l' x)
                    Just (structT, m) -> (tf [tr], LHSRange l' r)
                        where
                            (tf, _) = typeRanges structT
                            (r @ (hi, lo), base) = m Map.! x
                            hi' = BinOp Add base $ BinOp Sub hi lo
                            lo' = base
                            tr = (simplify hi', simplify lo')
                Implicit sg _ -> (Implicit sg [], LHSDot l' x)
                _ -> error $ "convertLHS encountered dot for bad type: " ++ show (t, l, x)
            where
                (t, l') = convertLHS l
                Struct p fields [] = t
                structTf = Struct p fields
                fieldType = lookupFieldType fields x
        convertLHS (LHSConcat lhss) =
            (Implicit Unspecified [], LHSConcat $ map (snd . convertLHS) lhss)

        -- try expression conversion by looking at the *outermost* type first
        convertExpr :: Type -> Expr -> Expr
        -- TODO: This is really a conversion for using default patterns to
        -- populate arrays. Maybe this should be somewhere else?
        convertExpr (IntegerVector t sg (r:rs)) (Pattern [(Just "default", e)]) =
            Repeat (rangeSize r) [e']
            where e' = convertExpr (IntegerVector t sg rs) e
        convertExpr (Struct (Packed sg) fields (_:rs)) (Bit e _) =
            convertExpr (Struct (Packed sg) fields rs) e
        convertExpr (Struct (Packed _) fields _) (Pattern [(Just "default", e)]) =
            Concat $ take (length fields) (repeat e)
        convertExpr (Struct (Packed sg) fields []) (Pattern items) =
            if Map.notMember structTf structs
                then Pattern items''
                else Concat exprs
            where
                subMap = \(Just ident, subExpr) ->
                    (Just ident, convertExpr (lookupFieldType fields ident) subExpr)
                structTf = Struct (Packed sg) fields
                items' =
                    -- if the pattern does not use identifiers, use the
                    -- identifiers from the struct type definition in order
                    if not (all (isJust . fst) items)
                        then zip (map (Just. snd) fields) (map snd items)
                        else items
                items'' = map subMap items'
                fieldRange = \(Just x, _) -> lookupUnstructRange structTf x
                exprs = map snd $ reverse $ sortOn fieldRange items''
        convertExpr _ other = other

        -- try expression conversion by looking at the *innermost* type first
        convertSubExpr :: Expr -> (Type, Expr)
        convertSubExpr (Ident x) =
            case Map.lookup x types of
                Nothing -> (Implicit Unspecified [], Ident x)
                Just t -> (t, Ident x)
        convertSubExpr (Dot e x) =
            case subExprType of
                Struct _ _ _ ->
                    if Map.notMember structTf structs
                        then (fieldType, Dot e' x)
                        else (fieldType, Range  e' r)
                _ -> (Implicit Unspecified [], Dot e' x)
            where
                (subExprType, e') = convertSubExpr e
                Struct p fields [] = subExprType
                structTf = Struct p fields
                fieldType = lookupFieldType fields x
                r = lookupUnstructRange structTf x
        convertSubExpr (Range eOuter (rOuter @ (hiO, loO))) =
            -- VCS doesn't allow ranges to be cascaded, so we need to combine
            -- nested Ranges into a single range. My understanding of the
            -- semantics are that a range return a new, zero-indexed sub-range.
            case eOuter' of
                Range eInner (hiI, loI) ->
                    (t, Range eInner (simplify hi, simplify lo))
                    where
                        hi = BinOp Add (BinOp Sub hiI loI) hiO
                        lo = BinOp Add loI loO
                _ -> (t, Range eOuter' rOuter)
            where (t, eOuter') = convertSubExpr eOuter
        convertSubExpr (Concat exprs) =
            (Implicit Unspecified [], Concat $ map (snd . convertSubExpr) exprs)
        convertSubExpr (BinOp op e1 e2) =
            (Implicit Unspecified [], BinOp op e1' e2')
            where
                (_, e1') = convertSubExpr e1
                (_, e2') = convertSubExpr e2
        convertSubExpr (Bit e i) =
            (t', Bit e' i')
            where
                (t, e') = convertSubExpr e
                t' = case typeRanges t of
                    (_, []) -> Implicit Unspecified []
                    (tf, rs) -> tf $ tail rs
                (_, i') = convertSubExpr i
        -- TODO: There are other expression cases that we probably need to
        -- recurse into. That said, it's not clear to me how much we really
        -- expect to see things like concatenated packed structs, for example.
        convertSubExpr other = (Implicit Unspecified [], other)

        -- lookup the range of a field in its unstructured type
        lookupUnstructRange :: TypeFunc -> Identifier -> Range
        lookupUnstructRange structTf fieldName =
            fieldRangeMap Map.! fieldName
            where fieldRangeMap = Map.map fst $ snd $ structs Map.! structTf

        -- lookup the type of a field in the given field list
        lookupFieldType :: [(Type, Identifier)] -> Identifier -> Type
        lookupFieldType fields fieldName = fieldMap Map.! fieldName
            where fieldMap = Map.fromList $ map swap fields
