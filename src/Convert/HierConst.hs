{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Elaborate hierarchical references to constants
 -
 - [System]Verilog does not allow hierarchical identifiers as constant
 - primaries. However, the resolution of type information across scopes can
 - create such hierarchical references. This conversion performs substitution
 - for any hierarchical references to parameters or localparams, regardless of
 - whether or not they occur within what should be a constant expression.
 -
 - If an identifier refers to a parameter which has been shadowed locally, the
 - conversion creates a localparam alias of the parameter at the top level scope
 - and refers to the parameter using that alias instead.
 -
 - TODO: Support resolution of hierarchical references to constant functions
 - TODO: Some other conversions still blindly substitute type information
 -}

module Convert.HierConst (convert) where

import Control.Monad (when)
import qualified Data.Map.Strict as Map

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (Part attrs extern kw lifetime name ports items) =
    Part attrs extern kw lifetime name ports $
        if null shadowedParams
            then items'
            else map expand items'
    where
        (items', mapping) = runScoper $ scopeModuleItems scoper name items
        scoper = scopeModuleItem
            traverseDeclM
            (traverseExprsM traverseExprM)
            (traverseGenItemExprsM traverseExprM)
            (traverseStmtExprsM traverseExprM)
        shadowedParams = Map.keys $ Map.filter (== HierParam True) $
            extractMapping mapping
        expand = traverseNestedModuleItems $ expandParam shadowedParams
convertDescription description = description

expandParam :: [Identifier] -> ModuleItem -> ModuleItem
expandParam shadowed (MIPackageItem (Decl param@(Param Parameter _ x _))) =
    if elem x shadowed
        then Generate $ map (GenModuleItem . wrap) [param, extra]
        else wrap param
    where
        wrap = MIPackageItem . Decl
        extra = Param Localparam UnknownType (prefix x) (Ident x)
expandParam _ item = item

prefix :: Identifier -> Identifier
prefix = (++) "_sv2v_disambiguate_"

data Hier
    = HierParam Bool
    | HierLocalparam Expr
    | HierVar
    deriving Eq

type ST = Scoper Hier

traverseDeclM :: Decl -> ST Decl
traverseDeclM decl = do
    case decl of
        Param Parameter _ x _ ->
            insertElem x (HierParam False)
        Param Localparam UnknownType x e ->
            scopeExpr e >>= insertElem x . HierLocalparam
        Param Localparam (Implicit sg rs) x e ->
            scopeExpr (Cast (Left t) e) >>= insertElem x . HierLocalparam
            where t = IntegerVector TBit sg rs
        Param Localparam (IntegerVector _ sg rs) x e ->
            scopeExpr (Cast (Left t) e) >>= insertElem x . HierLocalparam
            where t = IntegerVector TBit sg rs
        Param Localparam t x e ->
            scopeExpr (Cast (Left t) e) >>= insertElem x . HierLocalparam
        Variable _ _ x [] _ -> insertElem x HierVar
        Net  _ _ _ _ x [] _ -> insertElem x HierVar
        _ -> return ()
    traverseDeclExprsM traverseExprM decl

-- substitute hierarchical references to constants
traverseExprM :: Expr -> ST Expr
traverseExprM expr@(Dot _ x) = do
    expr' <- traverseSinglyNestedExprsM traverseExprM expr
    detailsE <- lookupElemM expr'
    detailsX <- lookupElemM x
    case (detailsE, detailsX) of
        (Just ([_, _], _, HierParam{}), Just ([_, _], _, HierParam{})) ->
            return $ Ident x
        (Just ([_, _], _, HierVar), Just ([_, _], _, HierVar)) ->
            return $ Ident x
        (Just (accesses@[Access _ Nil, _], _, HierParam False), _) -> do
            details <- lookupElemM $ prefix x
            when (details == Nothing) $
                insertElem accesses (HierParam True)
            return $ Ident $ prefix x
        (Just ([Access _ Nil, _], _, HierParam True), _) ->
            return $ Ident $ prefix x
        (Just (aE, replacements, HierLocalparam value), Just (aX, _, _)) ->
            if aE == aX && Map.null replacements
                then return $ Ident x
                else traverseSinglyNestedExprsM traverseExprM $
                        replaceInExpr replacements value
        (Just (_, replacements, HierLocalparam value), Nothing) ->
            traverseSinglyNestedExprsM traverseExprM $
                replaceInExpr replacements value
        _ -> traverseSinglyNestedExprsM traverseExprM expr
traverseExprM expr = traverseSinglyNestedExprsM traverseExprM expr
