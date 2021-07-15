{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `foreach` loops.
 -
 - We simply convert these loops to a series of loops, with the bounds and
 - direction provided by the array dimension query system functions. Omitted
 - indices are skipped.
 -}

module Convert.Foreach (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert =
    map $ traverseDescriptions $ traverseModuleItems $
    traverseStmts $ traverseNestedStmts convertStmt

convertStmt :: Stmt -> Stmt
convertStmt (Foreach x idxs stmt) =
    (foldl (.) id $ map toLoop $ zip [1..] idxs) stmt
    where
        toLoop :: (Integer, Identifier) -> (Stmt -> Stmt)
        toLoop (_, "") = id
        toLoop (d, i) =
            Block Seq "" [idxDecl] . pure .
                For [(LHSIdent i, queryFn FnLeft)] cmp [incr]
            where
                queryFn f = DimFn f (Right $ Ident x) (RawNum d)
                idxType = IntegerAtom TInteger Unspecified
                idxDecl = Variable Local idxType i [] Nil
                cmp =
                    Mux (BinOp Eq (queryFn FnIncrement) (RawNum 1))
                        (BinOp Ge (Ident i) (queryFn FnRight))
                        (BinOp Le (Ident i) (queryFn FnRight))
                incr = (LHSIdent i, AsgnOp Sub, queryFn FnIncrement)
convertStmt other = other
