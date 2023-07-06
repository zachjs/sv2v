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
 - The conversion for `inside` produces wildcard equality comparisons as per the
 - SystemVerilog specification. However, many usages of `inside` don't depend on
 - the wildcard behavior. To avoid generating needlessly complex output, this
 - conversion use the standard equality operator if the pattern obviously
 - contains no wildcard bits.
 -}

module Convert.Wildcard (convert) where

import Control.Monad (when)
import Data.Bits ((.|.))

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription =
    partScoper traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

traverseDeclM :: Decl -> Scoper Number Decl
traverseDeclM decl = do
    case decl of
        Param Localparam _ x (Number n) -> insertElem x n
        Param Parameter  _ x (Number n) ->
            when (numberToInteger n /= Nothing) $ insertElem x n
        _ -> return ()
    let mi = MIPackageItem $ Decl decl
    mi' <- traverseModuleItemM mi
    let MIPackageItem (Decl decl') = mi'
    return decl'

traverseModuleItemM :: ModuleItem -> Scoper Number ModuleItem
traverseModuleItemM = traverseExprsM traverseExprM

traverseGenItemM :: GenItem -> Scoper Number GenItem
traverseGenItemM = traverseGenItemExprsM traverseExprM

traverseStmtM :: Stmt -> Scoper Number Stmt
traverseStmtM = traverseStmtExprsM traverseExprM

traverseExprM :: Expr -> Scoper Number Expr
traverseExprM = traverseNestedExprsM $ embedScopes convertExpr

lookupPattern :: Scopes Number -> Expr -> Maybe Number
lookupPattern _ (Number n) = Just n
lookupPattern scopes e =
    case lookupElem scopes e of
        Nothing -> Nothing
        Just (_, _, n) -> Just n

convertExpr :: Scopes Number -> Expr -> Expr
convertExpr scopes (BinOp WEq l r) =
    if maybePattern == Nothing then
        BinOp BitAnd couldMatch $
        BinOp BitOr  noExtraXZs $
        Number (Based 1 False Binary 0 1)
    else if numberToInteger pattern /= Nothing then
        BinOp Eq l r
    else
        BinOp Eq (BinOp BitOr l mask) pattern'
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
        -- For wildcard patterns we can find, use masking
        maybePattern = lookupPattern scopes r
        Just pattern = maybePattern
        Based size signed base vals knds = pattern
        mask = Number $ Based size signed base knds 0
        pattern' = Number $ Based size signed base (vals .|. knds) 0
convertExpr scopes (BinOp WNe l r) =
    UniOp LogNot $
    convertExpr scopes $
    BinOp WEq l r
convertExpr _ other = other
