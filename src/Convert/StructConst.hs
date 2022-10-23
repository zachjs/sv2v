{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - High-level elaboration for struct constant accesses
 -
 - This greatly simplifies designs with long sequences of struct parameters
 - which extend and reference one another, as seen in BlackParrot.
 -}

module Convert.StructConst (convert) where

import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type StructType = [Field]
type StructValue = [(TypeOrExpr, Expr)]

type Const = (StructType, StructValue)
type Consts = Map.Map Identifier Const
type Types = Map.Map Identifier StructType

type SC = State (Types, Consts)

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription =
    flip evalState mempty .
    traverseModuleItemsM (traverseDeclsM elaborateDecl)

insertType :: Identifier -> [Field] -> SC ()
insertType ident typ = do
    (types, consts) <- get
    let types' = Map.insert ident typ types
    put (types', consts)

insertConst :: Identifier -> Const -> SC ()
insertConst ident cnst = do
    (types, consts) <- get
    let consts' = Map.insert ident cnst consts
    put (types, consts')

lookupType :: Type -> SC [Field]
lookupType (Alias ident []) = do
    maybeFields <- gets $ Map.lookup ident . fst
    return $ fromMaybe [] maybeFields
lookupType (Struct (Packed Unspecified) fields []) =
    return fields
lookupType _ = return []

lookupConst :: Identifier -> SC (Maybe Const)
lookupConst param = gets $ Map.lookup param . snd

elaborateDecl :: Decl -> SC Decl
-- track struct type parameters
elaborateDecl decl@(ParamType Localparam x t)
    | Struct (Packed Unspecified) fields [] <- t =
        insertType x fields >> return decl
-- track and resolve struct constants
elaborateDecl (Param Localparam t x e) = do
    e' <- elaborateExpr e
    fields <- lookupType t
    when (not $ null fields) $ do
        maybeValues <- extractStructValue e'
        case maybeValues of
            Just values -> insertConst x (fields, values)
            Nothing -> return ()
    return $ Param Localparam t x e'
elaborateDecl decl = return decl

-- extract the pattern items, including for simple aliases
extractStructValue :: Expr -> SC (Maybe StructValue)
extractStructValue (Pattern values) = return $ Just values
extractStructValue (Ident param) = fmap (fmap snd) $ lookupConst param
extractStructValue _ = return Nothing

-- elaborate constant field accesses
elaborateExpr :: Expr -> SC Expr
elaborateExpr expr@(Dot (Ident param) field) =
    fmap (fromMaybe expr . join . fmap (resolveParam field)) (lookupConst param)
elaborateExpr expr =
    traverseSinglyNestedExprsM elaborateExpr expr

-- lookup value in struct constant
resolveParam :: Identifier -> Const -> Maybe Expr
resolveParam field (fields, values) = do
    fieldType <- lookup field (map swap fields)
    value <- mplus
        (lookup (Right $ Ident field) values)
        (lookup (Left UnknownType) values)
    Just $ Cast (Left fieldType) value
