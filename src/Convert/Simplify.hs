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
    let mi = MIPackageItem $ Decl decl
    mi' <- traverseModuleItemM mi
    let MIPackageItem (Decl decl') = mi'
    return decl'

traverseModuleItemM :: ModuleItem -> State Info ModuleItem
traverseModuleItemM (Instance m p x r l) = do
    p' <- mapM paramBindingMapper p
    traverseExprsM traverseExprM $ Instance m p' x r l
    where
        paramBindingMapper (param, Left t) = do
            t' <- traverseTypeExprsM substituteExprM t
            return (param, Left t')
        paramBindingMapper (param, Right e) = return (param, Right e)
traverseModuleItemM item = traverseExprsM traverseExprM item

traverseStmtM :: Stmt -> State Info Stmt
traverseStmtM stmt = traverseStmtExprsM traverseExprM stmt

traverseExprM :: Expr -> State Info Expr
traverseExprM = traverseNestedExprsM $ stately convertExpr

substituteExprM :: Expr -> State Info Expr
substituteExprM = traverseNestedExprsM $ stately substitute

convertExpr :: Info -> Expr -> Expr
convertExpr info (Cast (Right c) e) =
    Cast (Right c') e
    where
        c' = simplify $ substitute info c
convertExpr info (DimFn f v e) =
    DimFn f v e'
    where
        e' = simplify $ substitute info e
convertExpr info (Call (Ident "$clog2") (Args [e] [])) =
    if clog2' == clog2
        then clog2
        else clog2'
    where
        e' = simplify $ substitute info e
        clog2 = Call (Ident "$clog2") (Args [e'] [])
        clog2' = simplify clog2
convertExpr info (Mux cc aa bb) =
    if before == after
        then simplify $ Mux cc aa bb
        else simplify $ Mux after aa bb
    where
        before = substitute info cc
        after = simplify before
convertExpr _ (other @ Repeat{}) = traverseNestedExprs simplify other
convertExpr _ (other @ Concat{}) = simplify other
convertExpr _ (other @ BinOp{}) = simplify other
convertExpr _ (other @ UniOp{}) = simplify other
convertExpr _ other = other

substitute :: Info -> Expr -> Expr
substitute info expr =
    traverseNestedExprs substitute' $ simplify expr
    where
        substitute' :: Expr -> Expr
        substitute' (Ident x) =
            case Map.lookup x info of
                Nothing -> Ident x
                Just e -> e
        substitute' other = other
