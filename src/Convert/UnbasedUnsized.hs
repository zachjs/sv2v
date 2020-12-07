{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for unbased, unsized literals ('0, '1, 'z, 'x)
 -
 - The literals are given a binary base, a size of 1, and are made signed to
 - allow sign extension. For context-determined expressions, the converted
 - literals are repeated to match the context-determined size.
 -
 - As a special case, unbased, unsized literals which take on the size of a
 - module's port are replaced as above, but with the size of the port being
 - determined based on the parameter bindings of the instance and the definition
 - of the instantiated module.
 -}

module Convert.UnbasedUnsized (convert) where

import Control.Monad.Writer.Strict
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map

import Convert.ExprUtils
import Convert.Traverse
import Language.SystemVerilog.AST

type Part = ([Identifier], [ModuleItem])
type Parts = Map.Map Identifier Part

data ExprContext
    = SelfDetermined
    | ContextDetermined Expr
    deriving (Eq, Show)

convert :: [AST] -> [AST]
convert files =
    map (traverseDescriptions convertDescription) files
    where
        parts = execWriter $ mapM (collectDescriptionsM collectPartsM) files
        convertDescription = traverseModuleItems $ convertModuleItem parts

collectPartsM :: Description -> Writer Parts ()
collectPartsM (Part _ _ _ _ name ports items) =
    tell $ Map.singleton name (ports, items)
collectPartsM _ = return ()

convertModuleItem :: Parts -> ModuleItem -> ModuleItem
convertModuleItem parts (Instance moduleName params instanceName [] bindings) =
    if Map.member moduleName parts && not (any isTypeParam moduleItems)
        then convertModuleItem' $
                Instance moduleName params instanceName [] bindings'
        else Instance moduleName params instanceName [] bindings
    where
        bindings' = zipWith convertBinding bindings [0..]
        (portNames, moduleItems) =
            case Map.lookup moduleName parts of
                Nothing -> error $ "could not find module: " ++ moduleName
                Just partInfo -> partInfo
        isTypeParam :: ModuleItem -> Bool
        isTypeParam (MIPackageItem (Decl ParamType{})) = True
        isTypeParam _ = False
        tag = Ident "~~uub~~"
        convertBinding :: PortBinding -> Int -> PortBinding
        convertBinding (portName, expr) idx =
            (portName, ) $
            traverseNestedExprs (replaceBindingExpr portName idx) $
            convertExpr (ContextDetermined tag) expr
        replaceBindingExpr :: Identifier -> Int -> Expr -> Expr
        replaceBindingExpr portName idx (orig @ (Repeat _ [ConvertedUU a b])) =
            if orig == sizedLiteralFor tag ch
                then Repeat portSize [ConvertedUU a b]
                else orig
            where
                ch = charForBit a b
                portName' =
                    if null portName
                        then lookupBindingName portNames idx
                        else portName
                portSize = determinePortSize portName' params moduleItems
        replaceBindingExpr _ _ other = other
convertModuleItem _ other = convertModuleItem' other

determinePortSize :: Identifier -> [ParamBinding] -> [ModuleItem] -> Expr
determinePortSize portName instanceParams moduleItems =
    step (reverse initialMapping) moduleItems
    where
        moduleParams = parameterNames moduleItems
        initialMapping = catMaybes $
            zipWith createParamReplacement instanceParams [0..]
        createParamReplacement
            :: ParamBinding -> Int -> Maybe (Identifier, Expr)
        createParamReplacement ("", b) idx =
            createParamReplacement (paramName, b) idx
            where paramName = lookupBindingName moduleParams idx
        createParamReplacement (_, Left _) _ = Nothing
        createParamReplacement (paramName, Right expr) _ =
            Just (paramName, tagExpr expr)

        step :: [(Identifier, Expr)] -> [ModuleItem] -> Expr
        step mapping (MIPackageItem (Decl (Param _ _ x e)) : rest) =
            step ((x, e) : mapping) rest
        step mapping (MIPackageItem (Decl (Variable _ t x a _)) : rest) =
            if x == portName
                then substituteExpr (reverse mapping) size
                else step mapping rest
            where size = BinOp Mul (dimensionsSize a) (DimsFn FnBits $ Left t)
        step mapping (_ : rest) = step mapping rest
        step _ [] = error $ "could not find size of port " ++ portName

substituteExpr :: [(Identifier, Expr)] -> Expr -> Expr
substituteExpr _ (Ident (':' : x)) =
    Ident x
substituteExpr mapping (Dot (Ident x) y) =
    case lookup x mapping of
        Nothing -> Dot (Ident x) y
        Just (Pattern items) ->
            case lookup y items of
                Just item -> substituteExpr mapping item
                Nothing -> Dot (substituteExpr mapping (Pattern items)) y
        Just expr -> Dot (substituteExpr mapping expr) y
substituteExpr mapping (Ident x) =
    case lookup x mapping of
        Nothing -> Ident x
        Just expr -> substituteExpr mapping expr
substituteExpr mapping expr =
    traverseSinglyNestedExprs (substituteExpr mapping) expr

tagExpr :: Expr -> Expr
tagExpr (Ident x) = Ident (':' : x)
tagExpr expr = traverseSinglyNestedExprs tagExpr expr

-- given a list of module items, produces the parameter names in order
parameterNames :: [ModuleItem] -> [Identifier]
parameterNames =
    execWriter . mapM (collectNestedModuleItemsM $ collectDeclsM collectDeclM)
    where
        collectDeclM :: Decl -> Writer [Identifier] ()
        collectDeclM (Param Parameter   _ x _) = tell [x]
        collectDeclM (ParamType Parameter x _) = tell [x]
        collectDeclM _ = return ()

lookupBindingName :: [Identifier] -> Int -> Identifier
lookupBindingName names idx =
    if idx < length names
        then names !! idx
        else error $ "out of bounds binding " ++ show (names, idx)

convertModuleItem' :: ModuleItem -> ModuleItem
convertModuleItem' =
    traverseExprs (convertExpr SelfDetermined) .
    traverseTypes (traverseNestedTypes convertType) .
    traverseAsgns convertAsgn

literalFor :: Char -> Expr
literalFor 'Z' = literalFor 'z'
literalFor 'X' = literalFor 'x'
literalFor '0' = Number $ Based 1 True Binary 0 0
literalFor '1' = Number $ Based 1 True Binary 1 0
literalFor 'x' = Number $ Based 1 True Binary 0 1
literalFor 'z' = Number $ Based 1 True Binary 1 1
literalFor ch = error $ "unexpected unbased-unsized digit: " ++ [ch]

pattern ConvertedUU :: Integer -> Integer -> Expr
pattern ConvertedUU a b = Number (Based 1 True Binary a b)

charForBit :: Integer -> Integer -> Char
charForBit 0 0 = '0'
charForBit 1 0 = '1'
charForBit 0 1 = 'x'
charForBit 1 1 = 'z'
charForBit _ _ = error "charForBit invariant violated"

sizedLiteralFor :: Expr -> Char -> Expr
sizedLiteralFor expr ch =
    Repeat size [literalFor ch]
    where size = DimsFn FnBits $ Right expr

convertAsgn :: (LHS, Expr) -> (LHS, Expr)
convertAsgn (lhs, expr) =
    (lhs, convertExpr context expr)
    where context = ContextDetermined $ lhsToExpr lhs

convertExpr :: ExprContext -> Expr -> Expr
convertExpr _ (DimsFn fn (Right e)) =
    DimsFn fn $ Right $ convertExpr SelfDetermined e
convertExpr _ (Cast te e) =
    Cast te $ convertExpr SelfDetermined e
convertExpr _ (Concat exprs) =
    Concat $ map (convertExpr SelfDetermined) exprs
convertExpr context (Pattern [(":default", e @ UU{})]) =
    convertExpr context e
convertExpr _ (Pattern items) =
    Pattern $ zip
    (map fst items)
    (map (convertExpr SelfDetermined . snd) items)
convertExpr _ (Call expr (Args pnArgs kwArgs)) =
    Call expr $ Args pnArgs' kwArgs'
    where
        pnArgs' = map (convertExpr SelfDetermined) pnArgs
        Pattern kwArgs' = convertExpr SelfDetermined $ Pattern kwArgs
convertExpr _ (Repeat count exprs) =
    Repeat count $ map (convertExpr SelfDetermined) exprs
convertExpr SelfDetermined (Mux cond (e1 @ UU{}) (e2 @ UU{})) =
    Mux
    (convertExpr SelfDetermined cond)
    (convertExpr SelfDetermined e1)
    (convertExpr SelfDetermined e2)
convertExpr SelfDetermined (Mux cond e1 e2) =
    Mux
    (convertExpr SelfDetermined cond)
    (convertExpr (ContextDetermined e2) e1)
    (convertExpr (ContextDetermined e1) e2)
convertExpr (ContextDetermined expr) (Mux cond e1 e2) =
    Mux
    (convertExpr SelfDetermined cond)
    (convertExpr context e1)
    (convertExpr context e2)
    where context = ContextDetermined expr
convertExpr SelfDetermined (BinOp op e1 e2) =
    if isPeerSizedBinOp op || isParentSizedBinOp op
        then BinOp op
            (convertExpr (ContextDetermined e2) e1)
            (convertExpr (ContextDetermined e1) e2)
        else BinOp op
            (convertExpr SelfDetermined e1)
            (convertExpr SelfDetermined e2)
convertExpr (ContextDetermined expr) (BinOp op e1 e2) =
    if isPeerSizedBinOp op then
        BinOp op
            (convertExpr (ContextDetermined e2) e1)
            (convertExpr (ContextDetermined e1) e2)
    else if isParentSizedBinOp op then
        BinOp op
            (convertExpr context e1)
            (convertExpr context e2)
    else
        BinOp op
            (convertExpr SelfDetermined e1)
            (convertExpr SelfDetermined e2)
    where context = ContextDetermined expr
convertExpr context (UniOp op expr) =
    if isSizedUniOp op
        then UniOp op (convertExpr context expr)
        else UniOp op (convertExpr SelfDetermined expr)
convertExpr SelfDetermined (UU ch) =
    literalFor ch
convertExpr (ContextDetermined expr) (UU ch) =
    sizedLiteralFor expr ch
convertExpr _ other = other

pattern UU :: Char -> Expr
pattern UU ch = Number (UnbasedUnsized ch)

convertType :: Type -> Type
convertType (TypeOf e) = TypeOf $ convertExpr SelfDetermined e
convertType other = other

isParentSizedBinOp :: BinOp -> Bool
isParentSizedBinOp BitAnd  = True
isParentSizedBinOp BitXor  = True
isParentSizedBinOp BitXnor = True
isParentSizedBinOp BitOr   = True
isParentSizedBinOp Mul     = True
isParentSizedBinOp Div     = True
isParentSizedBinOp Mod     = True
isParentSizedBinOp Add     = True
isParentSizedBinOp Sub     = True
isParentSizedBinOp _       = False

isPeerSizedBinOp :: BinOp -> Bool
isPeerSizedBinOp Eq      = True
isPeerSizedBinOp Ne      = True
isPeerSizedBinOp TEq     = True
isPeerSizedBinOp TNe     = True
isPeerSizedBinOp WEq     = True
isPeerSizedBinOp WNe     = True
isPeerSizedBinOp Lt      = True
isPeerSizedBinOp Le      = True
isPeerSizedBinOp Gt      = True
isPeerSizedBinOp Ge      = True
isPeerSizedBinOp _       = False

isSizedUniOp :: UniOp -> Bool
isSizedUniOp = (/= LogNot)
