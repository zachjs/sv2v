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
    traverseStmts convertStmt

convertStmt :: Stmt -> Stmt
convertStmt (Foreach x idxs stmt) =
    (foldl (.) id $ map toLoop $ zip [1..] idxs) stmt
    where
        toLoop :: (Int, Maybe Identifier) -> (Stmt -> Stmt)
        toLoop (_, Nothing) = id
        toLoop (d, Just i) =
            For [Left idxDecl] (Just cmp) [incr]
            where
                queryFn f = DimFn f (Right $ Ident x) (Number $ show d)
                idxDecl = Variable Local (IntegerAtom TInteger Unspecified) i []
                    $ Just $ queryFn FnLeft
                cmp =
                    Mux (BinOp Eq (queryFn FnIncrement) (Number "1"))
                        (BinOp Ge (Ident i) (queryFn FnRight))
                        (BinOp Le (Ident i) (queryFn FnRight))
                incr = (LHSIdent i, AsgnOp Sub, queryFn FnIncrement)
convertStmt other = other
