{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for the `type` operator
 -
 - TODO: This conversion only supports the most basic expressions so far. We can
 - add support for range and bit accesses, struct fields, and perhaps even
 - arithmetic operations. Bits and pieces of similar logic exist in other
 - conversion.
 -}

module Convert.TypeOf (convert) where

import Control.Monad.State
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Int (Int32)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Info = Map.Map Identifier Type

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ Part{}) =
    scopedConversion traverseDeclM traverseModuleItemM traverseStmtM
        initialState description
    where
        Part _ _ _ _ _ _ items = description
        initialState = Map.fromList $ mapMaybe returnType items
        returnType :: ModuleItem -> Maybe (Identifier, Type)
        returnType (MIPackageItem (Function _ t f _ _)) =
            if t == Implicit Unspecified []
                -- functions with no return type implicitly return a single bit
                then Just (f, IntegerVector TLogic Unspecified [])
                else Just (f, t)
        returnType _ = Nothing
convertDescription other = other

traverseDeclM :: Decl -> State Info Decl
traverseDeclM decl = do
    item <- traverseModuleItemM (MIPackageItem $ Decl decl)
    let MIPackageItem (Decl decl') = item
    case decl' of
        Variable d t ident a me -> do
            let t' = injectRanges t a
            modify $ Map.insert ident t'
            return $ case t' of
                UnpackedType t'' a' -> Variable d t'' ident a' me
                _ ->                   Variable d t'  ident [] me
        Param    _ t ident   _ -> do
            let t' = if t == Implicit Unspecified []
                        then IntegerAtom TInteger Unspecified
                        else t
            modify $ Map.insert ident t'
            return decl'
        ParamType{} -> return decl'
        CommentDecl{} -> return decl'

traverseModuleItemM :: ModuleItem -> State Info ModuleItem
traverseModuleItemM item = traverseTypesM traverseTypeM item

traverseStmtM :: Stmt -> State Info Stmt
traverseStmtM =
    traverseStmtExprsM $ traverseNestedExprsM $ traverseExprTypesM traverseTypeM

traverseTypeM :: Type -> State Info Type
traverseTypeM (TypeOf expr) = typeof expr
traverseTypeM other = return other

typeof :: Expr -> State Info Type
typeof (Number n) =
    return $ IntegerVector TLogic sg [r]
    where
        (size, sg) = parseNumber n
        r = (Number $ show (size - 1), Number "0")
typeof (orig @ (Ident x)) = do
    res <- gets $ Map.lookup x
    return $ fromMaybe (TypeOf orig) res
typeof (orig @ (Call (Ident x) _)) = do
    res <- gets $ Map.lookup x
    return $ fromMaybe (TypeOf orig) res
typeof (orig @ (Bit e _)) = do
    t <- typeof e
    return $ case t of
        TypeOf _ -> TypeOf orig
        _ -> popRange t
typeof (orig @ (Range e mode r)) = do
    t <- typeof e
    return $ case t of
        TypeOf _ -> TypeOf orig
        _ -> replaceRange (lo, hi) t
    where
        lo = fst r
        hi = case mode of
            NonIndexed   -> snd r
            IndexedPlus  -> BinOp Sub (uncurry (BinOp Add) r) (Number "1")
            IndexedMinus -> BinOp Add (uncurry (BinOp Sub) r) (Number "1")
typeof (orig @ (Cast (Right (Ident x)) _)) = do
    typeMap <- get
    if Map.member x typeMap
        then return $ typeOfSize (Ident x)
        else return $ TypeOf orig
typeof (Cast (Right s) _) = return $ typeOfSize s
typeof (UniOp BitNot  e  ) = typeof e
typeof (BinOp Pow     e _) = typeof e
typeof (BinOp ShiftL  e _) = typeof e
typeof (BinOp ShiftR  e _) = typeof e
typeof (BinOp ShiftAL e _) = typeof e
typeof (BinOp ShiftAR e _) = typeof e
typeof (BinOp Add     a b) = return $ largerSizeType a b
typeof (BinOp Sub     a b) = return $ largerSizeType a b
typeof (BinOp Mul     a b) = return $ largerSizeType a b
typeof (BinOp Div     a b) = return $ largerSizeType a b
typeof (BinOp Mod     a b) = return $ largerSizeType a b
typeof (BinOp BitAnd  a b) = return $ largerSizeType a b
typeof (BinOp BitXor  a b) = return $ largerSizeType a b
typeof (BinOp BitXnor a b) = return $ largerSizeType a b
typeof (BinOp BitOr   a b) = return $ largerSizeType a b
typeof (Mux   _       a b) = return $ largerSizeType a b
typeof (Concat      exprs) = return $ typeOfSize $ concatSize exprs
typeof (Repeat reps exprs) = return $ typeOfSize size
    where size = BinOp Mul reps (concatSize exprs)
typeof other = return $ TypeOf other

-- determines the size and sign of a number literal
parseNumber :: String -> (Int32, Signing)
parseNumber s =
    case elemIndex '\'' s of
        Nothing  -> (32, Signed)
        Just 0   -> parseNumber $ '3' : '2' : s
        Just idx -> (size, signing)
            where
                Just size = readNumber $ take idx s
                signing = case drop (idx + 1) s of
                    's' : _ -> Signed
                    _       -> Unsigned

-- produces a type large enough to hold either expression
largerSizeType :: Expr -> Expr -> Type
largerSizeType a b =
    typeOfSize larger
    where
        sizeof = DimsFn FnBits . Right
        cond = BinOp Ge (sizeof a) (sizeof b)
        larger = Mux cond (sizeof a) (sizeof b)

-- returns the total size of concatenated list of expressions
concatSize :: [Expr] -> Expr
concatSize exprs =
    foldl (BinOp Add) (Number "0") $
    map sizeof exprs
    where
        sizeof = DimsFn FnBits . Right

-- produces a generic type of the given size
typeOfSize :: Expr -> Type
typeOfSize size =
    IntegerVector TLogic sg [(hi, Number "0")]
    where
        sg = Unspecified -- suitable for now
        hi = BinOp Sub size (Number "1")

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
