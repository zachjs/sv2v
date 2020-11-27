{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `inside` expressions and cases
 -
 - The expressions are compared to each candidate using `==?`, the wildcard
 - comparison. As required by the specification, the result of each comparison
 - is combined using an OR reduction.
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

import Control.Monad.Writer
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
        toCheck (Left pattern) =
            BinOp WEq expr pattern
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
    else if hasSideEffects expr then
        Block Seq "" [decl] [stmt]
    else
        foldr ($) defaultStmt $
        map (uncurry $ If NoCheck) $
        zip comps stmts
    where
        exprs = map fst items
        -- evaluate expressions with side effects once
        tmp = "sv2v_temp_" ++ shortHash expr
        decl = Variable Local (TypeOf expr) tmp [] expr
        stmt = convertStmt (Case u kw (Ident tmp) items)
        -- underlying inside case elaboration
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

hasSideEffects :: Expr -> Bool
hasSideEffects expr =
    getAny $ execWriter $ collectNestedExprsM write expr
    where
        write :: Expr -> Writer Any ()
        write Call{} = tell $ Any True
        write _ = return ()
