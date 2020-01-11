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
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Info = Map.Map Identifier (Type, [Range])

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ Part{}) =
    scopedConversion traverseDeclM traverseModuleItemM traverseStmtM
        initialState description
    where
        Part _ _ _ _ _ _ items = description
        initialState = Map.fromList $ mapMaybe returnType items
        returnType :: ModuleItem -> Maybe (Identifier, (Type, [Range]))
        returnType (MIPackageItem (Function _ t f _ _)) =
            Just (f, (t', []))
            where t' = if t == Implicit Unspecified []
                    then IntegerVector TLogic Unspecified []
                    else t
        returnType _ = Nothing
convertDescription other = other

traverseDeclM :: Decl -> State Info Decl
traverseDeclM decl = do
    case decl of
        Variable _ t ident a _ -> modify $ Map.insert ident (t, a)
        Param    _ t ident   _ -> modify $ Map.insert ident (t, [])
        ParamType    _     _ _ -> return ()
    item <- traverseModuleItemM (MIPackageItem $ Decl decl)
    let MIPackageItem (Decl decl') = item
    return decl'

traverseModuleItemM :: ModuleItem -> State Info ModuleItem
traverseModuleItemM item = traverseTypesM traverseTypeM item

traverseStmtM :: Stmt -> State Info Stmt
traverseStmtM stmt = do
    let item = Initial stmt
    item' <- traverseModuleItemM item
    let Initial stmt' = item'
    return stmt'

traverseTypeM :: Type -> State Info Type
traverseTypeM (TypeOf expr) = typeof expr
traverseTypeM other = return other

typeof :: Expr -> State Info Type
typeof (orig @ (Ident x)) = do
    res <- gets $ Map.lookup x
    return $ maybe (TypeOf orig) injectRanges res
typeof (orig @ (Call (Ident x) _)) = do
    res <- gets $ Map.lookup x
    return $ maybe (TypeOf orig) injectRanges res
typeof other = return $ TypeOf other

-- combines a type with unpacked ranges
injectRanges :: (Type, [Range]) -> Type
injectRanges (t, unpacked) =
    tf $ packed ++ unpacked
    where (tf, packed) = typeRanges t
