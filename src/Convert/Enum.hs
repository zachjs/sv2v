{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `enum`
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

module Convert.Enum (convert) where

import Control.Monad (zipWithM_, (>=>))
import Data.List (elemIndices)

import Convert.ExprUtils
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type SC = Scoper Expr

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ partScoper
    traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

traverseDeclM :: Decl -> SC Decl
traverseDeclM decl = do
    case decl of
        Variable _ _ x _ _ -> insertElem x Nil
        Net  _ _ _ _ x _ _ -> insertElem x Nil
        Param    _ _ x   _ -> insertElem x Nil
        ParamType  _ x   _ -> insertElem x Nil
        CommentDecl{} -> return ()
    traverseDeclTypesM traverseTypeM decl >>=
        traverseDeclExprsM traverseExprM

traverseModuleItemM :: ModuleItem -> SC ModuleItem
traverseModuleItemM (Genvar x) =
    insertElem x Nil >> return (Genvar x)
traverseModuleItemM item =
    traverseNodesM traverseExprM return traverseTypeM traverseLHSM return item
    where traverseLHSM = traverseLHSExprsM traverseExprM

traverseGenItemM :: GenItem -> SC GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseStmtM :: Stmt -> SC Stmt
traverseStmtM = traverseStmtExprsM traverseExprM

traverseTypeM :: Type -> SC Type
traverseTypeM =
    traverseSinglyNestedTypesM traverseTypeM >=>
    traverseTypeExprsM traverseExprM >=>
    replaceEnum

traverseExprM :: Expr -> SC Expr
traverseExprM (Ident x) = do
    details <- lookupElemM x
    return $ case details of
        Just (_, _, Nil) -> Ident x
        Just (_, _, e) -> e
        Nothing -> Ident x
traverseExprM expr =
    traverseSinglyNestedExprsM traverseExprM expr
        >>= traverseExprTypesM traverseTypeM

-- replace enum types and insert enum items
replaceEnum :: Type -> SC Type
replaceEnum t@(Enum Alias{} v _) = -- not ready
    mapM_ (flip insertElem Nil . fst) v >> return t
replaceEnum (Enum (Implicit sg rl) v rs) =
    replaceEnum $ Enum t' v rs
    where
        -- default to a 32 bit logic
        t' = IntegerVector TLogic sg rl'
        rl' = if null rl
            then [(RawNum 31, RawNum 0)]
            else rl
replaceEnum (Enum t v rs) =
    insertEnumItems t v >> return (tf $ rl ++ rs)
    where (tf, rl) = typeRanges t
replaceEnum other = return other

insertEnumItems :: Type -> [(Identifier, Expr)] -> SC ()
insertEnumItems itemType items =
    -- check for obviously duplicate values
    if noDuplicates
        then zipWithM_ insertEnumItem keys vals
        else scopedErrorM $ "enum conversion has duplicate vals: "
                ++ show (zip keys vals)
    where
        insertEnumItem :: Identifier -> Expr -> SC ()
        insertEnumItem x = scopeExpr . Cast (Left itemType) >=> insertElem x
        (keys, valsRaw) = unzip items
        vals = tail $ scanl step (UniOp UniSub $ RawNum 1) valsRaw
        noDuplicates = all (null . tail . flip elemIndices vals) vals
        step :: Expr -> Expr -> Expr
        step expr Nil = simplify $ BinOp Add expr (RawNum 1)
        step _ expr = expr
