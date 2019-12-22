{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `inside` expressions and cases
 -
 - The expressions are compared to each candidate using the wildcard comparison
 - operator. Note that if expression has any Xs or Zs that are not wildcarded in
 - the candidate, the results is `1'bx`. As required by the specification, the
 - result of each comparison is combined using an OR reduction.
 -
 - `case ... inside` statements are converted to an equivalent if-else cascade.
 -
 - TODO: Add support for array value ranges.
 - TODO: This conversion may cause an expression with side effects to be
 - evaluated more than once.
 -}

module Convert.Inside (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

import Data.Maybe (fromMaybe)

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems convertModuleItem

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem item =
    traverseExprs (traverseNestedExprs convertExpr) $
    traverseStmts convertStmt $
    item

convertExpr :: Expr -> Expr
convertExpr (Inside Nil valueRanges) =
    Inside Nil valueRanges
convertExpr (Inside expr valueRanges) =
    if length checks == 1
        then head checks
        else UniOp RedOr $ Concat checks
    where
        checks = map toCheck valueRanges
        toCheck :: ExprOrRange -> Expr
        toCheck (Left e) =
            Mux
            (BinOp TNe rxr lxlxrxr)
            (Number "1'bx")
            (BinOp WEq expr e)
            where
                lxl = BinOp BitXor expr expr
                rxr = BinOp BitXor e e
                lxlxrxr = BinOp BitXor lxl rxr
        toCheck (Right (lo, hi)) =
            BinOp LogAnd
                (BinOp Le lo expr)
                (BinOp Ge hi expr)
convertExpr other = other

convertStmt :: Stmt -> Stmt
convertStmt (Case u kw expr items) =
    if not $ any isSpecialInside exprs then
        Case u kw expr items
    else if kw /= CaseN then
        error $ "cannot use inside with " ++ show kw
    else
        foldr ($) defaultStmt $
        map (uncurry $ If NoCheck) $
        zip comps stmts
    where
        exprs = map fst items
        itemsNonDefault = filter (not . null . fst) items
        isSpecialInside :: [Expr] -> Bool
        isSpecialInside [Inside Nil _] = True
        isSpecialInside _ = False
        makeComp :: [Expr] -> Expr
        makeComp [Inside Nil ovr] = Inside expr ovr
        makeComp _ = error "internal invariant violated"
        comps = map (makeComp . fst) itemsNonDefault
        stmts = map snd itemsNonDefault
        defaultStmt = fromMaybe Null (lookup [] items)
convertStmt other = other
