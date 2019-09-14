{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion of streaming concatenations.
 -}

module Convert.Stream (convert) where

import Control.Monad.Writer
import Data.List.Unique (complex)

import Convert.Traverse
import Language.SystemVerilog.AST

type Funcs = [ModuleItem]

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ (Part _ _ _ _ _ _)) =
    Part extern kw lifetime name ports (items ++ funcs)
    where
        (description', funcSet) =
            runWriter $ traverseModuleItemsM (traverseStmtsM traverseStmtM) description
        Part extern kw lifetime name ports items = description'
        (funcs, _, _) = complex funcSet
convertDescription other = other

streamerBlock :: Expr -> Expr -> (LHS -> Expr -> Stmt) -> LHS -> Expr -> Stmt
streamerBlock chunk size asgn output input =
    Block Nothing
    [ Variable Local t inp [] $ Just input
    , Variable Local t out [] Nothing
    , Variable Local (IntegerAtom TInteger Unspecified) idx [] Nothing
    , Variable Local (IntegerAtom TInteger Unspecified) bas [] Nothing
    ]
    [ For inits cmp incr stmt
    , AsgnBlk AsgnOpEq (LHSIdent bas) (Ident idx)
    , For inits cmp2 incr2 stmt2
    , asgn output (Ident out)
    ]
    where
        lo = Number "0"
        hi = BinOp Sub size (Number "1")
        t = IntegerVector TLogic Unspecified [(hi, lo)]
        name = streamerBlockName chunk size
        inp = name ++ "_inp"
        out = name ++ "_out"
        idx = name ++ "_idx"
        bas = name ++ "_bas"
        -- main chunk loop
        inits = [Right (LHSIdent idx, lo)]
        cmp = Just $ BinOp Le (Ident idx) (BinOp Sub hi chunk)
        incr = [(LHSIdent idx, AsgnOp Add, chunk)]
        lhs = LHSRange (LHSIdent out) IndexedMinus (BinOp Sub hi (Ident idx), chunk)
        expr = Range (Ident inp) IndexedPlus (Ident idx, chunk)
        stmt = AsgnBlk AsgnOpEq lhs expr
        -- final chunk loop
        cmp2 = Just $ BinOp Lt (Ident idx) (BinOp Sub size (Ident bas))
        incr2 = [(LHSIdent idx, AsgnOp Add, Number "1")]
        lhs2 = LHSBit (LHSIdent out) (Ident idx)
        expr2 = Bit (Ident inp) (BinOp Add (Ident idx) (Ident bas))
        stmt2 = AsgnBlk AsgnOpEq lhs2 expr2

streamerBlockName :: Expr -> Expr -> Identifier
streamerBlockName chunk size =
    "_sv2v_strm_" ++ shortHash (chunk, size)

traverseStmtM :: Stmt -> Writer Funcs Stmt
traverseStmtM (AsgnBlk op lhs expr) =
    traverseAsgnM (lhs, expr) (AsgnBlk op)
traverseStmtM (Asgn mt lhs expr) =
    traverseAsgnM (lhs, expr) (Asgn mt)
traverseStmtM other = return other

traverseAsgnM :: (LHS, Expr) -> (LHS -> Expr -> Stmt) -> Writer Funcs Stmt
traverseAsgnM (lhs, Stream StreamR _ exprs) constructor =
    return $ constructor lhs expr
    where
        expr = Concat $ exprs ++ [Repeat delta [Number "1'b0"]]
        size = DimsFn FnBits $ Right $ lhsToExpr lhs
        exprSize = DimsFn FnBits $ Right (Concat exprs)
        delta = BinOp Sub size exprSize
traverseAsgnM (LHSStream StreamR _ lhss, expr) constructor =
    return $ constructor (LHSConcat lhss) expr
traverseAsgnM (lhs, Stream StreamL chunk exprs) constructor = do
    return $ streamerBlock chunk size constructor lhs expr
    where
        expr = Concat $ Repeat delta [Number "1'b0"] : exprs
        size = DimsFn FnBits $ Right $ lhsToExpr lhs
        exprSize = DimsFn FnBits $ Right (Concat exprs)
        delta = BinOp Sub size exprSize
traverseAsgnM (LHSStream StreamL chunk lhss, expr) constructor = do
    return $ streamerBlock chunk size constructor lhs expr
    where
        lhs = LHSConcat lhss
        size = DimsFn FnBits $ Right expr
traverseAsgnM (lhs, expr) constructor =
    return $ constructor lhs expr
