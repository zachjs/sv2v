{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `casez` and `casex`
 -
 - Note that this conversion does not completely replicate the behavior of
 - `casex` and `casez` in cases where that case expression itself (rather than
 - just the case item patterns) contains wildcard values. This is apparently
 - rarely ever intentionally done.
 -}

module Convert.CaseKW (convert) where

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: AST -> AST
convert = traverseDescriptions (traverseModuleItems (traverseStmts convertStmt))

-- Conversions:
-- `casez` -> `case` with wildcards (?, z) expanded
-- `casex` -> `case` with wildcards (?, z, x) expanded
-- to be either 0 or 1

wildcards :: CaseKW -> [Char]
wildcards CaseN = [] -- CaseN == `case`
wildcards CaseZ = ['?', 'z', 'Z']
wildcards CaseX = ['?', 'z', 'Z', 'x', 'X']

possibilities :: [Char]
possibilities = ['0', '1']

explodeBy :: [Char] -> String -> [String]
explodeBy _ "" = [""]
explodeBy wilds (x : xs) =
    (map (:) chars) <*> (explodeBy wilds xs)
    where chars = if elem x wilds then possibilities else [x]

expandExpr :: [Char] -> Expr -> [Expr]
expandExpr wilds (Number s) = map Number $ explodeBy wilds s
expandExpr [] other = [other]
-- TODO: Hopefully they only give us constant expressions...
-- TODO: We could be given a constant identifier...
expandExpr _ other = error $ "CaseKW conversion encountered case that was not a number, which is dubious..." ++ (show other)

-- Note that we don't have to convert the statements within the cases, as the
-- conversion template takes care of that for us.
convertStmt :: Stmt -> Stmt
convertStmt (Case kw expr cases def) =
    Case CaseN expr cases' def
    where
        wilds = wildcards kw
        cases' = map convertCase cases
        convertCase :: Case -> Case
        convertCase (exprs, stmt) = (exprs', stmt)
            where exprs' = concat $ map (expandExpr wilds) exprs
convertStmt other = other
