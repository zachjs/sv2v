{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Elaboration of size casts, dimension query system functions, and ternary
 - expressions where the condition references a localparam.
 -
 - Our conversions generate a lot of ternary expressions. This conversion
 - attempts to make the code output a bit cleaner. Note that we can only do this
 - simplification on localparams because parameters can be overridden at
 - instantiation.
 -
 - This conversion applies the heuristic that it will only make substitutions
 - into a ternary condition if making substitutions immediately enables the
 - expression to be simplified further.
 -}

module Convert.Simplify (convert) where

import Control.Monad.State
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Info = Map.Map Identifier Expr

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription =
    scopedConversion traverseDeclM traverseModuleItemM traverseStmtM Map.empty

traverseDeclM :: Decl -> State Info Decl
traverseDeclM decl = do
    case decl of
        Param Localparam _ x e -> modify $ Map.insert x e
        _ -> return ()
    return decl

traverseModuleItemM :: ModuleItem -> State Info ModuleItem
traverseModuleItemM item = traverseExprsM traverseExprM item

traverseStmtM :: Stmt -> State Info Stmt
traverseStmtM stmt = traverseStmtExprsM traverseExprM stmt

traverseExprM :: Expr -> State Info Expr
traverseExprM = traverseNestedExprsM $ stately convertExpr

convertExpr :: Info -> Expr -> Expr
convertExpr info (Cast (Right c) e) =
    case c' of
        Number _ ->
            if sized == e
                then Cast (Right c') e
                else sized
        _ -> Cast (Right c') e
    where
        c' = simplify $ traverseNestedExprs (substitute info) (simplify c)
        sized = sizedExpr "" c' e
convertExpr info (DimFn f v e) =
    DimFn f v e'
    where
        e' = simplify $ traverseNestedExprs (substitute info) e
convertExpr info (Mux cc aa bb) =
    if before == after
        then Mux cc aa bb
        else simplify $ Mux after aa bb
    where
        before = traverseNestedExprs (substitute info) (simplify cc)
        after = simplify before
convertExpr _ other = other

substitute :: Info -> Expr -> Expr
substitute info (Ident x) =
    case Map.lookup x info of
        Nothing -> Ident x
        Just e -> e
substitute _ other = other
