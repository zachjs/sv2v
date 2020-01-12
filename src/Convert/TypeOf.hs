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
import Data.Maybe (fromMaybe, mapMaybe)
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
            modify $ Map.insert ident t
            return decl'
        ParamType    _     _ _ -> return decl'

traverseModuleItemM :: ModuleItem -> State Info ModuleItem
traverseModuleItemM item = traverseTypesM traverseTypeM item

traverseStmtM :: Stmt -> State Info Stmt
traverseStmtM =
    traverseStmtExprsM $ traverseNestedExprsM $ traverseExprTypesM traverseTypeM

traverseTypeM :: Type -> State Info Type
traverseTypeM (TypeOf expr) = typeof expr
traverseTypeM other = return other

typeof :: Expr -> State Info Type
typeof (orig @ (Ident x)) = do
    res <- gets $ Map.lookup x
    return $ fromMaybe (TypeOf orig) res
typeof (orig @ (Call (Ident x) _)) = do
    res <- gets $ Map.lookup x
    return $ fromMaybe (TypeOf orig) res
typeof (orig @ (Bit (Ident x) _)) = do
    res <- gets $ Map.lookup x
    return $ maybe (TypeOf orig) popRange res
typeof other = return $ TypeOf other

-- combines a type with unpacked ranges
injectRanges :: Type -> [Range] -> Type
injectRanges t [] = t
injectRanges (UnpackedType t rs) unpacked = UnpackedType t $ unpacked ++ rs
injectRanges t unpacked = UnpackedType t unpacked

-- removes the outermost range of the given type
popRange :: Type -> Type
popRange t =
    tf $ tail rs
    where (tf, rs) = typeRanges t
