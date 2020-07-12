{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `==?` and `!=?`
 -
 - `a ==? b` is defined as the bitwise comparison of `a` and `b`, where X and Z
 - values in `b` (but not those in `a`) are used as wildcards. This conversion
 - relies on the fact that works because any value xor'ed with X or Z becomes X.
 -
 - Procedure for `A ==? B`:
 - 1. If there is any bit in A that doesn't match a non-wildcarded bit in B,
 -    then the result is always `1'b0`.
 - 2. If there is any X or Z in A that is not wildcarded in B, then the result
 -    is `1'bx`.
 - 3. Otherwise, the result is `1'b1`.
 -
 - `!=?` is simply converted as the logical negation of `==?`, which is
 -
 - TODO: For code using actual wildcard patterns, this conversion produces code
 - which is not synthesizable.
 -
 - The conversion for `inside` produces wildcard equality comparisons as per the
 - SystemVerilog specification. However, many usages of `inside` don't depend on
 - the wildcard behavior. To avoid generating needlessly complex output, this
 - conversion use the standard equality operator if the pattern obviously
 - contains no wildcard bits.
 -}

module Convert.Wildcard (convert) where

import Control.Monad.State
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type Patterns = Map.Map Identifier Number

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription =
    scopedConversion traverseDeclM traverseModuleItemM traverseStmtM Map.empty

traverseDeclM :: Decl -> State Patterns Decl
traverseDeclM decl = do
    case decl of
        Param Localparam _ x (Number n) -> modify $ Map.insert x n
        _ -> return ()
    let mi = MIPackageItem $ Decl decl
    mi' <- traverseModuleItemM mi
    let MIPackageItem (Decl decl') = mi'
    return decl'

traverseModuleItemM :: ModuleItem -> State Patterns ModuleItem
traverseModuleItemM = traverseExprsM traverseExprM

traverseStmtM :: Stmt -> State Patterns Stmt
traverseStmtM = traverseStmtExprsM traverseExprM

traverseExprM :: Expr -> State Patterns Expr
traverseExprM = traverseNestedExprsM $ stately convertExpr

isPlainPattern :: Patterns -> Expr -> Bool
isPlainPattern _ (Number n) =
    numberToInteger n /= Nothing
isPlainPattern patterns (Ident x) =
    case Map.lookup x patterns of
        Nothing -> False
        Just n -> isPlainPattern patterns (Number n)
isPlainPattern _ _ = False

convertExpr :: Patterns -> Expr -> Expr
convertExpr patterns (BinOp WEq l r) =
    if isPlainPattern patterns r
        then BinOp Eq l r
        else
            BinOp BitAnd couldMatch $
            BinOp BitOr  noExtraXZs $
            Number (Based 1 False Binary 0 1)
    where
        lxl = BinOp BitXor l l
        rxr = BinOp BitXor r r
        -- Step #1: definitive mismatch
        couldMatch = BinOp TEq rxlxl lxrxr
        rxlxl = BinOp BitXor r lxl
        lxrxr = BinOp BitXor l rxr
        -- Step #2: extra X or Z
        noExtraXZs = BinOp TEq lxlxrxr rxr
        lxlxrxr = BinOp BitXor lxl rxr
convertExpr patterns (BinOp WNe l r) =
    UniOp LogNot $
    convertExpr patterns $
    BinOp WEq l r
convertExpr _ other = other
