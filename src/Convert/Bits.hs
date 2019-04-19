{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Elaboration of `$bits`, where possible
 -}

module Convert.Bits (convert) where

import Control.Monad.State
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Info = Map.Map Identifier (Type, [Range])

convert :: AST -> AST
convert = traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription =
    scopedConversion traverseDeclM traverseModuleItemM traverseStmtM Map.empty

-- collects and converts multi-dimensional packed-array declarations
traverseDeclM :: Decl -> State Info Decl
traverseDeclM (origDecl @ (Variable _ t ident a _)) = do
    modify $ Map.insert ident (t, a)
    return origDecl
traverseDeclM other = return other

traverseModuleItemM :: ModuleItem -> State Info ModuleItem
traverseModuleItemM item = traverseExprsM traverseExprM item

traverseStmtM :: Stmt -> State Info Stmt
traverseStmtM stmt = traverseStmtExprsM traverseExprM stmt

traverseExprM :: Expr -> State Info Expr
traverseExprM = traverseNestedExprsM $ stately convertExpr

convertExpr :: Info -> Expr -> Expr
convertExpr _ (Bits (Left t)) =
    case t of
        IntegerVector _ _ rs -> dimensionsSize rs
        Implicit        _ rs -> dimensionsSize rs
        Net             _ rs -> dimensionsSize rs
        _ -> Bits $ Left t
convertExpr info (Bits (Right e)) =
    case e of
        Ident x ->
            case Map.lookup x info of
                Nothing -> Bits $ Right e
                Just (t, rs) -> simplify $ BinOp Mul
                        (dimensionsSize rs)
                        (convertExpr info $ Bits $ Left t)
        Concat exprs ->
            foldl (BinOp Add) (Number "0") $
            map (convertExpr info) $
            map (Bits . Right) $
            exprs
        Range expr mode range ->
            simplify $ BinOp Mul size
                (convertExpr info $ Bits $ Right $ Bit expr (Number "0"))
            where
                size = case mode of
                    NonIndexed   -> rangeSize range
                    IndexedPlus  -> snd range
                    IndexedMinus -> snd range
        Bit (Ident x) idx ->
            case Map.lookup x info of
                Nothing -> Bits $ Right $ Bit (Ident x) idx
                Just (t, rs) ->
                    convertExpr info $ Bits $ Left t'
                    where t' = popRange t rs
        _ -> Bits $ Right e
convertExpr _ other = other

popRange :: Type -> [Range] -> Type
popRange t rs =
    tf $ tail rsCombined
    where
        (tf, trs) = typeRanges t
        rsCombined = rs ++ trs
