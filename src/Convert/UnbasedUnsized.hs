{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for unbased, unsized literals ('0, '1, 'z, 'x)
 -
 - The literals are given a binary base, a size of 1, and are made signed to
 - allow sign extension. For context-determined expressions, the converted
 - literals are repeated to match the context-determined size.
 -
 - When an unbased, unsized literal depends on the width a module port, the
 - constant portions of the instantiated module are inlined alongside synthetic
 - declarations matching the size of the port and filled with the desired bit.
 - This allows port widths to depend on functions or parameters while avoiding
 - creating hierarchical or generate-scoped references.
 -}

module Convert.UnbasedUnsized (convert) where

import Control.Monad.Writer.Strict
import Data.Either (isLeft)
import Data.Maybe (isNothing, mapMaybe)
import qualified Data.Map.Strict as Map

import Convert.Package (inject, prefixItems)
import Convert.Traverse
import Language.SystemVerilog.AST

type Part = [ModuleItem]
type Parts = Map.Map Identifier Part
type PortBit = (Identifier, Bit)

data ExprContext
    = SelfDetermined
    | ContextDetermined Expr

convert :: [AST] -> [AST]
convert files =
    map (traverseDescriptions convertDescription) files
    where
        parts = execWriter $ mapM (collectDescriptionsM collectPartsM) files
        convertDescription = traverseModuleItems $ convertModuleItem parts

collectPartsM :: Description -> Writer Parts ()
collectPartsM (Part _ _ _ _ name _ items) =
    tell $ Map.singleton name items
collectPartsM _ = return ()

convertModuleItem :: Parts -> ModuleItem -> ModuleItem
convertModuleItem parts (Instance moduleName params instanceName ds bindings) =
    if null extensionDecls || isNothing maybeModuleItems then
        convertModuleItem' $ instanceBase bindings
    else if hasTypeParams || not moduleIsResolved then
        instanceBase bindings
    else
        Generate $ map GenModuleItem $
            stubItems ++ [instanceBase bindings']
    where
        instanceBase = Instance moduleName params instanceName ds
        maybeModuleItems = Map.lookup moduleName parts
        Just moduleItems = maybeModuleItems

        -- checking whether we're ready to inline
        hasTypeParams = any (isLeft . snd) params
        moduleIsResolved = isEntirelyResolved selectedStubItems

        -- transform the existing bindings to reference extension declarations
        (bindings', extensionDeclLists) = unzip $
            map (convertBinding blockName) bindings
        extensionDecls = map (MIPackageItem . Decl) $ concat extensionDeclLists

        -- inline the necessary portions of the module alongside the selected
        -- extension declarations
        stubItems =
            map (traverseDecls overrideParam) $
            prefixItems blockName selectedStubItems
        selectedStubItems = inject rawStubItems extensionDecls
        rawStubItems = createModuleStub moduleItems
        blockName = "sv2v_uu_" ++ instanceName

        -- override a parameter value in the stub
        overrideParam :: Decl -> Decl
        overrideParam (Param Parameter t x e) =
            Param Localparam t x $
            case lookup xOrig params of
                Just val -> e'
                    where Right e' = val
                Nothing -> e
            where xOrig = drop (length blockName + 1) x
        overrideParam decl = decl

convertModuleItem _ other = convertModuleItem' other

-- convert a port binding and produce a list of needed extension decls
convertBinding :: Identifier -> PortBinding -> (PortBinding, [Decl])
convertBinding blockName (portName, expr) =
    ((portName, exprPatched), portBits)
    where
        exprRaw = convertExpr (ContextDetermined PortTag) expr
        (exprPatched, portBits) = runWriter $ traverseNestedExprsM
            (replaceBindingExpr blockName portName) exprRaw

-- identify and rewrite references to the width of the current port
replaceBindingExpr :: Identifier -> Identifier -> Expr -> Writer [Decl] Expr
replaceBindingExpr blockName portName (PortTaggedUU v k) = do
    tell [extensionDecl portBit]
    return $ Ident $ blockName ++ "_" ++ extensionDeclName portBit
    where portBit = (portName, bitForBased v k)
replaceBindingExpr _ _ other = return other

-- standardized name format for the synthetic declarations below
extensionDeclName :: PortBit -> Identifier
extensionDeclName (portName, bit) = "ext_" ++ portName ++ "_" ++ show bit

-- synthetic declaration with the type of the port filled with the given bit
extensionDecl :: PortBit -> Decl
extensionDecl portBit@(portName, bit) =
    Param Localparam t x e
    where
        t = Alias portName []
        x = extensionDeclName portBit
        e = literalFor bit

-- create an all-constant stub for an instantiated module
createModuleStub :: [ModuleItem] -> [PackageItem]
createModuleStub =
    mapMaybe stub
    where
        stub :: ModuleItem -> Maybe PackageItem
        stub (MIPackageItem (Decl decl)) = fmap Decl $ stubDecl decl
        stub (MIPackageItem item) = Just item
        stub _ = Nothing
        -- transform declarations into appropriate constants and type params
        stubDecl :: Decl -> Maybe Decl
        stubDecl (Variable d t x a _) = makePortType d t x a
        stubDecl (Net  d _ _ t x a _) = makePortType d t x a
        stubDecl decl = Just decl
        -- make a type parameter for each port declaration
        makePortType :: Direction -> Type -> Identifier -> [Range] -> Maybe Decl
        makePortType Input UnknownType x [] = Just $ ParamType Localparam x t
            where t = IntegerVector TLogic Unspecified []
        makePortType Input t x [] = Just $ ParamType Localparam x t
        makePortType _ _ _ _ = Nothing

-- ensure inlining the constants doesn't produce generate-scoped exprs or
-- expression type references
isEntirelyResolved :: [ModuleItem] -> Bool
isEntirelyResolved =
    not . getAny . execWriter .
    mapM (collectNestedModuleItemsM collectModuleItem)
    where
        collectModuleItem :: ModuleItem -> Writer Any ()
        collectModuleItem item =
            collectExprsM collectExpr item >>
            collectTypesM collectType item
        collectExpr :: Expr -> Writer Any ()
        collectExpr Dot{} = tell $ Any True
        collectExpr expr =
            collectExprTypesM collectType expr >>
            collectSinglyNestedExprsM collectExpr expr
        collectType :: Type -> Writer Any ()
        collectType TypeOf{} = tell $ Any True
        collectType typ =
            collectTypeExprsM collectExpr typ >>
            collectSinglyNestedTypesM collectType typ

convertModuleItem' :: ModuleItem -> ModuleItem
convertModuleItem' =
    traverseExprs (convertExpr SelfDetermined) .
    traverseTypes (traverseNestedTypes convertType) .
    traverseAsgns convertAsgn

literalFor :: Bit -> Expr
literalFor = Number . (uncurry $ Based 1 True Binary) . bitToVK

pattern PortTag :: Expr
pattern PortTag = Ident "~~uub~~"

-- a converted literal which depends on the current port's width
pattern PortTaggedUU :: Integer -> Integer -> Expr
pattern PortTaggedUU v k <- Repeat
    (DimsFn FnBits (Right PortTag))
    [Number (Based 1 True Binary v k)]

bitForBased :: Integer -> Integer -> Bit
bitForBased 0 0 = Bit0
bitForBased 1 0 = Bit1
bitForBased 0 1 = BitX
bitForBased _ _ = BitZ

sizedLiteralFor :: Expr -> Bit -> Expr
sizedLiteralFor expr bit =
    Repeat size [literalFor bit]
    where size = DimsFn FnBits $ Right expr

convertAsgn :: (LHS, Expr) -> (LHS, Expr)
convertAsgn (lhs, UU bit) =
    (lhs, literalFor bit)
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
convertExpr context (Pattern [(Left UnknownType, e@UU{})]) =
    convertExpr context e
convertExpr _ (Pattern items) =
    Pattern $ zip
    (map fst items)
    (map (convertExpr SelfDetermined . snd) items)
convertExpr _ (Call expr (Args pnArgs [])) =
    Call expr $ Args pnArgs' []
    where pnArgs' = map (convertExpr SelfDetermined) pnArgs
convertExpr _ (Repeat count exprs) =
    Repeat count $ map (convertExpr SelfDetermined) exprs
convertExpr SelfDetermined (Mux cond e1@UU{} e2@UU{}) =
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
convertExpr SelfDetermined (UU bit) =
    literalFor bit
convertExpr (ContextDetermined expr) (UU bit) =
    sizedLiteralFor expr bit
convertExpr _ other = other

pattern UU :: Bit -> Expr
pattern UU bit <- Number (UnbasedUnsized bit)

convertType :: Type -> Type
convertType (TypeOf e) = TypeOf $ convertExpr SelfDetermined e
convertType other = traverseTypeExprs (convertExpr SelfDetermined) other

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
