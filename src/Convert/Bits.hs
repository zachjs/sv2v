{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Elaboration of `$bits` expressions.
 -
 - Some tools support $bits in Verilog, but it is not part of the specification,
 - so we have to convert it ourselves.
 -
 - `$bits(t)`, where `t` is a type, is trivially elaborated to the product of
 - the sizes of its dimensions once `t` is resolved to a primitive base type.
 -
 - `$bits(e)`, where `e` is an expression, requires a scoped traversal to
 - determine the underlying type of expression. The conversion recursively
 - breaks the expression into its subtypes, finding their sizes instead.
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

traverseDeclM :: Decl -> State Info Decl
traverseDeclM decl = do
    case decl of
        Variable _ t ident a _ -> modify $ Map.insert ident (t, a)
        Parameter  t ident   _ -> modify $ Map.insert ident (t, [])
        Localparam t ident   _ -> modify $ Map.insert ident (t, [])
    return decl

traverseModuleItemM :: ModuleItem -> State Info ModuleItem
traverseModuleItemM item = traverseExprsM traverseExprM item

traverseStmtM :: Stmt -> State Info Stmt
traverseStmtM stmt = traverseStmtExprsM traverseExprM stmt

traverseExprM :: Expr -> State Info Expr
traverseExprM = traverseNestedExprsM $ stately convertExpr

-- simplify a bits expression given scoped type information
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

-- combines the given type and dimensions and returns a new type with the
-- innermost range removed
popRange :: Type -> [Range] -> Type
popRange t rs =
    tf $ tail rsCombined
    where
        (tf, trs) = typeRanges t
        rsCombined = rs ++ trs
