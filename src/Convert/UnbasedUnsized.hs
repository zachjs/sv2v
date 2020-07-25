{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for unbased, unsized literals ('0, '1, 'z, 'x)
 -
 - The literals are given a binary base, a size of 1, and are made signed to
 - allow sign extension. For context-determined expressions, the converted
 - literals are explicitly cast to the appropriate context-determined size.
 -
 - As a special case, unbased, unsized literals which take on the size of a
 - module port binding are replaced with a hierarchical reference to an
 - appropriately sized constant which is injected into the instantiated module's
 - definition. This allows these literals to be used for parameterized ports
 - without further complicating other conversions, as hierarchical references
 - are not allowed within constant expressions.
 -}

module Convert.UnbasedUnsized (convert) where

import Control.Monad.Writer

import Convert.Traverse
import Language.SystemVerilog.AST

data ExprContext
    = SelfDetermined
    | ContextDetermined Expr
    deriving (Eq, Show)

type Port = Either Identifier Int

data Bind = Bind
    { bModule :: Identifier
    , bBit :: Char
    , bPort :: Port
    } deriving (Eq, Show)

type Binds = [Bind]

convert :: [AST] -> [AST]
convert files =
    map (traverseDescriptions $ convertDescription binds) files'
    where
        (files', binds) = runWriter $
            mapM (mapM $ traverseModuleItemsM convertModuleItemM) files

convertDescription :: Binds -> Description -> Description
convertDescription [] other = other
convertDescription binds (Part attrs extern kw lifetime name ports items) =
    Part attrs extern kw lifetime name ports items'
    where
        binds' = filter ((== name) . bModule) binds
        items' = removeDupes [] $ items ++ map (bindItem ports) binds'
        removeDupes :: [Identifier] -> [ModuleItem] -> [ModuleItem]
        removeDupes _ [] = []
        removeDupes existing (item @ (MIPackageItem (Decl decl)) : is) =
            case decl of
                Param Localparam _ x _ ->
                    if elem x existing
                        then removeDupes existing is
                        else item : removeDupes (x : existing) is
                _ -> item : removeDupes existing is
        removeDupes existing (item : is) =
            item : removeDupes existing is
convertDescription _ other = other

bindName :: Bind -> Identifier
bindName (Bind _ ch (Left x)) = "sv2v_uub_" ++ ch : '_' : x
bindName (Bind m ch (Right i)) =
    bindName $ Bind m ch (Left $ show i)

bindItem :: [Identifier] -> Bind -> ModuleItem
bindItem ports bind =
    MIPackageItem $ Decl $ Param Localparam typ name expr
    where
        portName = lookupPort ports (bPort bind)
        size = DimsFn FnBits $ Right $ Ident portName
        rng = (BinOp Sub size (RawNum 1), RawNum 0)
        typ = Implicit Unspecified [rng]
        name = bindName bind
        expr = literalFor $ bBit bind

lookupPort :: [Identifier] -> Port -> Identifier
lookupPort _ (Left x) = x
lookupPort ports (Right i) =
    if i < length ports
        then ports !! i
        else error $ "out of bounds bort binding " ++ show (ports, i)

convertModuleItemM :: ModuleItem -> Writer Binds ModuleItem
convertModuleItemM (Instance moduleName params instanceName [] bindings) = do
    bindings' <- mapM (uncurry convertBinding) $ zip bindings [0..]
    let item = Instance moduleName params instanceName [] bindings'
    return $ convertModuleItem item
    where
        tag = Ident ":uub:"
        convertBinding :: PortBinding -> Int -> Writer Binds PortBinding
        convertBinding (portName, expr) idx = do
            let port = if null portName then Right idx else Left portName
            let expr' = convertExpr (ContextDetermined tag) expr
            expr'' <- traverseNestedExprsM (replaceBindingExpr port) expr'
            return (portName, expr'')
        replaceBindingExpr :: Port -> Expr -> Writer Binds Expr
        replaceBindingExpr port (orig @ (Repeat _ [ConvertedUU a b])) = do
            let ch = charForBit a b
            if orig == sizedLiteralFor tag ch
                then do
                    let bind = Bind moduleName ch port
                    tell [bind]
                    let expr = Dot (Ident instanceName) (bindName bind)
                    return expr
                else return orig
        replaceBindingExpr _ other = return other
convertModuleItemM other = return $ convertModuleItem other

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem =
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
