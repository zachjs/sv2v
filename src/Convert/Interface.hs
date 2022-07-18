{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for interfaces
 -}

module Convert.Interface (convert) where

import Data.List (intercalate, (\\))
import Data.Maybe (isJust, isNothing, mapMaybe)
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map

import Convert.ExprUtils (endianCondExpr)
import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

data PartInfo = PartInfo
    { pKind :: PartKW
    , pPorts :: [Identifier]
    , pItems :: [ModuleItem]
    }
type PartInfos = Map.Map Identifier PartInfo

type ModportInstances = [(Identifier, (Identifier, Identifier))]
type ModportBinding = (Identifier, (Substitutions, Expr))
type Substitutions = [(Expr, Expr)]

convert :: [AST] -> [AST]
convert files =
    if needsFlattening
        then files
        else traverseFiles
            (collectDescriptionsM collectPart)
            (map . convertDescription)
            files
    where
        -- we can only collect/map non-extern interfaces and modules
        collectPart :: Description -> Writer PartInfos ()
        collectPart (Part _ False kw _ name ports items) =
            tell $ Map.singleton name $ PartInfo kw ports items
        collectPart _ = return ()
        -- multidimensional instances need to be flattened before this
        -- conversion can proceed
        needsFlattening =
            getAny $ execWriter $ mapM (collectDescriptionsM checkPart) files
        checkPart :: Description -> Writer Any ()
        checkPart (Part _ _ _ _ _ _ items) =
            mapM (collectNestedModuleItemsM checkItem) items >> return ()
        checkPart _ = return ()
        checkItem :: ModuleItem -> Writer Any ()
        checkItem (Instance _ _ _ rs _) = when (length rs > 1) $ tell $ Any True
        checkItem _ = return ()

convertDescription :: PartInfos -> Description -> Description
convertDescription _ (Part _ _ Interface _ name _ _) =
    PackageItem $ Decl $ CommentDecl $ "removed interface: " ++ name
convertDescription parts (Part attrs extern Module lifetime name ports items) =
    if null $ extractModportInstances name $ PartInfo Module ports items then
        Part attrs extern Module lifetime name ports items'
    else
        PackageItem $ Decl $ CommentDecl $
            "removed module with interface ports: " ++ name
    where
        items' = evalScoper $ scopeModuleItems scoper name items
        scoper = scopeModuleItem traverseDeclM traverseModuleItemM return return

        traverseDeclM :: Decl -> Scoper [ModportDecl] Decl
        traverseDeclM decl = do
            case decl of
                Variable  _ _ x _ _ -> insertElem x DeclVal
                Net   _ _ _ _ x _ _ -> insertElem x DeclVal
                Param     _ _ x _   -> insertElem x DeclVal
                ParamType _   x _   -> insertElem x DeclVal
                CommentDecl{} -> return ()
            return decl

        lookupIntfElem :: Scopes [ModportDecl] -> Expr -> LookupResult [ModportDecl]
        lookupIntfElem modports expr =
            case lookupElem modports expr of
                Just (_, _, DeclVal) -> Nothing
                other -> other

        traverseModuleItemM :: ModuleItem -> Scoper [ModportDecl] ModuleItem
        traverseModuleItemM (Modport modportName modportDecls) =
            insertElem modportName modportDecls >> return (Generate [])
        traverseModuleItemM instanceItem@Instance{} = do
            modports <- embedScopes (\l () -> l) ()
            if isNothing maybePartInfo then
                return instanceItem
            else if partKind == Interface then
                -- inline instantiation of an interface
                scoper $ Generate $ map GenModuleItem $
                    inlineInstance modports rs []
                    partItems part instanceName paramBindings portBindings
            else if null modportInstances then
                return instanceItem
            else do
                -- inline instantiation of a module
                let modportBindings = getModportBindings modports
                let unconnected = map fst modportInstances \\
                                    map fst modportBindings
                if not (null unconnected)
                    then scopedErrorM $ "instance " ++ instanceName ++ " of "
                            ++ part ++ " has unconnected interface ports: "
                            ++ intercalate ", " unconnected
                    else scoper $ Generate $ map GenModuleItem $
                            inlineInstance modports rs modportBindings partItems
                            part instanceName paramBindings portBindings
            where
                Instance part paramBindings instanceName rs portBindings =
                    instanceItem
                maybePartInfo = Map.lookup part parts
                Just partInfo = maybePartInfo
                PartInfo partKind _ partItems = partInfo

                modportInstances = extractModportInstances part partInfo
                getModportBindings modports = mapMaybe
                    (inferModportBinding modports modportInstances) $
                    map (second $ addImpliedSlice modports) portBindings
                second f = \(a, b) -> (a, f b)

        traverseModuleItemM other = return other

        -- add explicit slices for bindings of entire modport instance arrays
        addImpliedSlice :: Scopes [ModportDecl] -> Expr -> Expr
        addImpliedSlice modports orig@(Dot expr modportName) =
            case lookupIntfElem modports (InstArrKey expr) of
                Just (_, _, InstArrVal l r) ->
                    Dot (Range expr NonIndexed (l, r)) modportName
                _ -> orig
        addImpliedSlice modports expr =
            case lookupIntfElem modports (InstArrKey expr) of
                Just (_, _, InstArrVal l r) ->
                    Range expr NonIndexed (l, r)
                _ -> expr

        -- elaborates and resolves provided modport bindings
        inferModportBinding :: Scopes [ModportDecl] -> ModportInstances ->
            PortBinding -> Maybe ModportBinding
        inferModportBinding modports modportInstances (portName, expr) =
            if maybeInfo == Nothing
                then Nothing
                else Just (portName, modportBinding)
            where
                modportBinding = (substitutions, replaceBit modportE)
                substitutions =
                    genSubstitutions modports base instanceE modportE
                maybeInfo =
                    lookupModportBinding modports modportInstances portName bitd
                Just (instanceE, modportE) = maybeInfo

                (exprUndot, bitd) = case expr of
                    Dot subExpr x -> (subExpr, Dot bitdUndot x)
                    _ -> (expr, bitdUndot)
                bitdUndot = case exprUndot of
                    Range subExpr _ _ -> Bit subExpr taggedOffset
                    Bit subExpr _ -> Bit subExpr untaggedOffset
                    _ -> exprUndot
                bitReplacement = case exprUndot of
                    Range _ mode range -> \e -> Range e mode range
                    Bit _ idx -> flip Bit idx
                    _ -> id
                base = case exprUndot of
                    Range{} -> Bit (Ident portName) Tag
                    _ -> Ident portName

                untaggedOffset = Ident $ modportBaseName portName
                taggedOffset = BinOp Add Tag untaggedOffset

                replaceBit :: Expr -> Expr
                replaceBit (Bit subExpr idx) =
                    if idx == untaggedOffset || idx == taggedOffset
                        then bitReplacement subExpr
                        else Bit subExpr idx
                replaceBit (Dot subExpr x) =
                    Dot (replaceBit subExpr) x
                replaceBit (Ident x) = Ident x
                replaceBit _ = error "replaceBit invariant violated"

        -- determines the underlying modport and interface instances associated
        -- with the given port binding, if it is a modport binding
        lookupModportBinding :: Scopes [ModportDecl] -> ModportInstances
            -> Identifier -> Expr -> Maybe (Expr, Expr)
        lookupModportBinding modports modportInstances portName expr =
            if bindingIsModport then
                -- provided specific instance modport
                foundModport expr
            else if bindingIsBundle && portIsBundle then
                -- bundle bound to a generic bundle
                foundModport expr
            else if bindingIsBundle && not portIsBundle then
                -- given entire interface, but just bound to a modport
                foundModport $ Dot expr modportName
            else if modportInstance /= Nothing then
                scopedError modports $ "could not resolve modport binding "
                    ++ show expr ++ " for port " ++ portName ++ " of type "
                    ++ showModportType interfaceName modportName
            else
                Nothing
            where
                bindingIsModport = lookupIntfElem modports expr /= Nothing
                bindingIsBundle = lookupIntfElem modports (Dot expr "") /= Nothing
                portIsBundle = null modportName
                modportInstance = lookup portName modportInstances
                (interfaceName, modportName) =
                    case modportInstance of
                        Just x -> x
                        Nothing -> scopedError modports $
                            "can't deduce modport for interface " ++ show expr
                            ++ " bound to port " ++ portName

                foundModport modportE =
                    if (null interfaceName || bInterfaceName == interfaceName)
                        && (null modportName || bModportName == modportName)
                        then Just (instanceE, qualifyModport modportE)
                        else scopedError modports msg
                    where
                        bModportName =
                            case modportE of
                                Dot _ x -> x
                                _ -> ""
                        instanceE = findInstance modportE
                        Just (_, _, InterfaceTypeVal bInterfaceName) =
                            lookupIntfElem modports $ InterfaceTypeKey
                                (findInstance modportE)
                        msg = "port " ++ portName ++ " has type "
                            ++ showModportType interfaceName modportName
                            ++ ", but the binding " ++ show expr ++ " has type "
                            ++ showModportType bInterfaceName bModportName

                findInstance :: Expr -> Expr
                findInstance e =
                    case lookupIntfElem modports (Dot e "") of
                        Nothing -> case e of
                            Bit e' _ -> findInstance e'
                            Dot e' _ -> findInstance e'
                            _ -> error "internal invariant violated"
                        Just (accesses, _, _) -> accessesToExpr $ init accesses
                qualifyModport :: Expr -> Expr
                qualifyModport e =
                    accessesToExpr $
                    case lookupIntfElem modports e of
                        Just (accesses, _, _) -> accesses
                        Nothing ->
                            case lookupIntfElem modports (Dot e "") of
                                Just (accesses, _, _) -> init accesses
                                Nothing -> scopedError modports $
                                    "could not find modport " ++ show e

        showModportType :: Identifier -> Identifier -> String
        showModportType "" "" = "generic interface"
        showModportType intf "" = intf
        showModportType intf modp = intf ++ '.' : modp

        -- expand a modport binding into a series of expression substitutions
        genSubstitutions :: Scopes [ModportDecl] -> Expr -> Expr -> Expr
            -> [(Expr, Expr)]
        genSubstitutions modports baseE instanceE modportE =
            (baseE, instanceE) :
            map toPortBinding modportDecls
            where
                a = lookupIntfElem modports modportE
                b = lookupIntfElem modports (Dot modportE "")
                Just (_, replacements, modportDecls) =
                    if a == Nothing then b else a
                toPortBinding (_, x, e) = (x', e')
                    where
                        x' = Dot baseE x
                        e' = replaceInExpr replacements e

        -- association list of modport instances in the given module body
        extractModportInstances :: Identifier -> PartInfo -> ModportInstances
        extractModportInstances part partInfo =
            execWriter $ runScoperT $ scopeModuleItems collector part decls
            where
                collector = scopeModuleItem checkDecl return return return
                decls = filter isDecl $ pItems partInfo
                checkDecl :: Decl -> ScoperT () (Writer ModportInstances) Decl
                checkDecl decl@(Variable _ t x _ _) =
                    if maybeInfo == Nothing then
                        return decl
                    else if elem x (pPorts partInfo) then
                        tell [(x, info)] >> return decl
                    else
                        scopedErrorM $
                            "Modport not in port list: " ++ show t ++ " " ++ x
                            ++ ". Is this an interface missing a port list?"
                    where
                        maybeInfo = extractModportInfo t
                        Just info = maybeInfo
                checkDecl decl = return decl

        extractModportInfo :: Type -> Maybe (Identifier, Identifier)
        extractModportInfo (InterfaceT "" "" _) = Just ("", "")
        extractModportInfo (InterfaceT interfaceName modportName _) =
            if isInterface interfaceName
                then Just (interfaceName, modportName)
                else Nothing
        extractModportInfo (Alias interfaceName _) =
            if isInterface interfaceName
                then Just (interfaceName, "")
                else Nothing
        extractModportInfo _ = Nothing

        isInterface :: Identifier -> Bool
        isInterface partName =
            case Map.lookup partName parts of
                Nothing -> False
                Just info -> pKind info == Interface

convertDescription _ other = other

isDecl :: ModuleItem -> Bool
isDecl (MIPackageItem Decl{}) = True
isDecl _ = False

-- produce the implicit modport decls for an interface bundle
impliedModport :: [ModuleItem] -> [ModportDecl]
impliedModport =
    execWriter . mapM
        (collectNestedModuleItemsM $ collectDeclsM collectModportDecls)
    where
        collectModportDecls :: Decl -> Writer [ModportDecl] ()
        collectModportDecls (Variable _ _ x _ _) =
            tell [(Inout, x, Ident x)]
        collectModportDecls (Net  _ _ _ _ x _ _) =
            tell [(Inout, x, Ident x)]
        collectModportDecls _ = return ()

-- convert an interface-bound module instantiation or an interface instantiation
-- into a series of equivalent inlined module items
inlineInstance :: Scopes [ModportDecl] -> [Range] -> [ModportBinding]
    -> [ModuleItem] -> Identifier -> Identifier -> [ParamBinding]
    -> [PortBinding] -> [ModuleItem]
inlineInstance global ranges modportBindings items partName
    instanceName instanceParams instancePorts =
    comment :
    map (MIPackageItem . Decl) bindingBaseParams ++
    map (MIPackageItem . Decl) parameterBinds ++
    wrapInstance instanceName items'
    : portBindings
    where
        items' = evalScoper $ scopeModuleItems scoper partName $
            map (traverseNestedModuleItems rewriteItem) $
            if null modportBindings
                then itemsChecked ++ infoModports
                else itemsChecked
        itemsChecked = checkBeforeInline global partName items checkErrMsg
        infoModports = [typeModport, dimensionModport, bundleModport]
        scoper = scopeModuleItem
            traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM

        key = shortHash (partName, instanceName)

        -- synthetic modports to be collected and removed after inlining
        bundleModport = Modport "" (impliedModport items)
        dimensionModport = if not isArray
            then Generate []
            else InstArrEncoded arrayLeft arrayRight
        typeModport = InterfaceTypeEncoded partName

        inlineKind =
            if null modportBindings
                then "interface"
                else "module"

        comment = MIPackageItem $ Decl $ CommentDecl $
            "expanded " ++ inlineKind ++ " instance: " ++ instanceName
        portBindings =
            wrapPortBindings $
            map portBindingItem $
            filter ((/= Nil) . snd) $
            filter notSubstituted instancePorts
        notSubstituted :: PortBinding -> Bool
        notSubstituted (portName, _) =
            lookup portName modportBindings == Nothing
        wrapPortBindings :: [ModuleItem] -> [ModuleItem]
        wrapPortBindings =
            if isArray
                then (\x -> [x]) . wrapInstance blockName
                else id
            where blockName = instanceName ++ "_port_bindings"

        rewriteItem :: ModuleItem -> ModuleItem
        rewriteItem =
            traverseDecls $
            removeModportInstance .
            removeDeclDir .
            overrideParam

        traverseDeclM :: Decl -> Scoper () Decl
        traverseDeclM decl = do
            case decl of
                Variable  _ _ x _ _ -> insertElem x ()
                Net   _ _ _ _ x _ _ -> insertElem x ()
                Param     _ _ x _   -> insertElem x ()
                ParamType _   x _   -> insertElem x ()
                CommentDecl{} -> return ()
            traverseDeclExprsM traverseExprM decl

        traverseModuleItemM :: ModuleItem -> Scoper () ModuleItem
        traverseModuleItemM item@Modport{} =
            traverseExprsM (scopeExpr >=> traverseExprM) item
        traverseModuleItemM item@(Instance _ _ x _ _) =
            insertElem x () >> traverseExprsM traverseExprM item
        traverseModuleItemM item =
            traverseExprsM traverseExprM item >>=
            traverseLHSsM  traverseLHSM

        traverseGenItemM :: GenItem -> Scoper () GenItem
        traverseGenItemM item@(GenFor (x, _) _ _ _) = do
            -- don't want to be scoped in modports
            insertElem x ()
            item' <- traverseGenItemExprsM traverseExprM item
            removeElem x
            return item'
        traverseGenItemM item =
            traverseGenItemExprsM traverseExprM item

        traverseStmtM :: Stmt -> Scoper () Stmt
        traverseStmtM =
            traverseStmtExprsM traverseExprM >=>
            traverseStmtLHSsM  traverseLHSM

        -- used for replacing usages of modports in the module being inlined
        modportSubstitutions = concatMap (fst . snd) modportBindings
        lhsReplacements = map (\(x, y) -> (toLHS x, toLHS y)) exprReplacements
        exprReplacements = filter ((/= Nil) . snd) modportSubstitutions
        -- LHSs are replaced using simple substitutions
        traverseLHSM :: LHS -> Scoper () LHS
        traverseLHSM =
            fmap replaceLHS .
            embedScopes tagLHS
        tagLHS :: Scopes () -> LHS -> LHS
        tagLHS scopes lhs
            | lookupElem scopes lhs /= Nothing =
                LHSDot (renamePartLHS lhs) "@"
            | Just portName <- partScopedModportRef $ lhsToExpr lhs =
                LHSIdent portName
            | otherwise =
                traverseSinglyNestedLHSs (tagLHS scopes) lhs
        renamePartLHS :: LHS -> LHS
        renamePartLHS (LHSDot (LHSIdent x) y) =
            if x == partName
                then LHSDot scopedInstanceLHS y
                else LHSDot (LHSIdent x) y
        renamePartLHS lhs = traverseSinglyNestedLHSs renamePartLHS lhs
        replaceLHS :: LHS -> LHS
        replaceLHS (LHSDot lhs "@") = lhs
        replaceLHS (LHSDot (LHSBit lhs elt) field) =
            case lookup (LHSDot (LHSBit lhs Tag) field) lhsReplacements of
                Just resolved -> replaceLHSArrTag elt resolved
                Nothing -> LHSDot (replaceLHS $ LHSBit lhs elt) field
        replaceLHS lhs =
            case lookup lhs lhsReplacements of
                Just lhs' -> lhs'
                Nothing -> traverseSinglyNestedLHSs replaceLHS lhs
        replaceLHSArrTag :: Expr -> LHS -> LHS
        replaceLHSArrTag =
            traverseNestedLHSs . (traverseLHSExprs . replaceArrTag)
        -- top-level expressions may be modports bound to other modports
        traverseExprM :: Expr -> Scoper () Expr
        traverseExprM =
            fmap replaceExpr .
            embedScopes tagExpr
        tagExpr :: Scopes () -> Expr -> Expr
        tagExpr scopes expr
            | lookupElem scopes expr /= Nothing =
                Dot (renamePartExpr expr) "@"
            | Just portName <- partScopedModportRef expr =
                Ident portName
            | otherwise =
                visitExprsStep (tagExpr scopes) expr
        renamePartExpr :: Expr -> Expr
        renamePartExpr (Dot (Ident x) y) =
            if x == partName
                then Dot scopedInstanceExpr y
                else Dot (Ident x) y
        renamePartExpr expr = visitExprsStep renamePartExpr expr
        replaceExpr :: Expr -> Expr
        replaceExpr (Dot expr "@") = expr
        replaceExpr (Ident x) =
            case lookup x modportBindings of
                Just (_, m) -> m
                Nothing -> Ident x
        replaceExpr expr =
            replaceExpr' expr
        replaceExpr' :: Expr -> Expr
        replaceExpr' (Dot expr "@") = expr
        replaceExpr' (Dot (Bit expr elt) field) =
            case lookup (Dot (Bit expr Tag) field) exprReplacements of
                Just resolved -> replaceArrTag (replaceExpr' elt) resolved
                Nothing -> Dot (replaceExpr' $ Bit expr elt) field
        replaceExpr' (Bit expr elt) =
            case lookup (Bit expr Tag) exprReplacements of
                Just resolved -> replaceArrTag (replaceExpr' elt) resolved
                Nothing -> Bit (replaceExpr' expr) (replaceExpr' elt)
        replaceExpr' expr@(Dot Ident{} _) =
            case lookup expr exprReplacements of
                Just expr' -> expr'
                Nothing -> visitExprsStep replaceExprAny expr
        replaceExpr' (Ident x) = Ident x
        replaceExpr' expr = replaceExprAny expr
        replaceExprAny :: Expr -> Expr
        replaceExprAny expr =
            case lookup expr exprReplacements of
                Just expr' -> expr'
                Nothing -> visitExprsStep replaceExpr' expr
        replaceArrTag :: Expr -> Expr -> Expr
        replaceArrTag replacement Tag = replacement
        replaceArrTag replacement expr =
            visitExprsStep (replaceArrTag replacement) expr

        partScopedModportRef :: Expr -> Maybe Identifier
        partScopedModportRef (Dot (Ident x) y) =
            if x == partName && lookup y modportBindings /= Nothing
                then Just y
                else Nothing
        partScopedModportRef _ = Nothing

        visitExprsStep :: (Expr -> Expr) -> Expr -> Expr
        visitExprsStep exprMapper =
            traverseSinglyNestedExprs exprMapper
            . traverseExprTypes (traverseNestedTypes typeMapper)
            where typeMapper = traverseTypeExprs exprMapper

        checkErrMsg :: String -> String
        checkErrMsg exprStr = "inlining instance \"" ++ instanceName
            ++ "\" of " ++ inlineKind ++ " \"" ++ partName
            ++ "\" would make expression \"" ++ exprStr ++ "\" used in \""
            ++ instanceName ++ "\" resolvable when it wasn't previously"

        -- unambiguous reference to the current instance
        scopedInstanceRaw = accessesToExpr $ localAccesses global instanceName
        scopedInstanceExpr =
            if isArray
                then Bit scopedInstanceRaw (Ident loopVar)
                else scopedInstanceRaw
        Just scopedInstanceLHS = exprToLHS scopedInstanceExpr

        removeModportInstance :: Decl -> Decl
        removeModportInstance (Variable d t x a e) =
            if maybeModportBinding == Nothing then
                Variable d t x a e
            else if makeBindingBaseExpr modportE == Nothing then
                CommentDecl $ "removed modport instance " ++ x
            else if null modportDims then
                localparam (modportBaseName x) bindingBaseExpr
            else
                localparam (modportBaseName x) $
                    BinOp Sub bindingBaseExpr (sliceLo NonIndexed modportDim)
            where
                maybeModportBinding = lookup x modportBindings
                Just (_, modportE) = maybeModportBinding
                bindingBaseExpr = Ident $ bindingBaseName ++ x
                modportDims = a ++ snd (typeRanges t)
                [modportDim] = modportDims
        removeModportInstance other = other

        removeDeclDir :: Decl -> Decl
        removeDeclDir (Variable _ t x a e) =
            Variable Local t' x a e
            where t' = case t of
                    Implicit Unspecified rs ->
                        IntegerVector TLogic Unspecified rs
                    _ -> t
        removeDeclDir decl@Net{} =
            traverseNetAsVar removeDeclDir decl
        removeDeclDir other = other

        -- capture the lower bound for each modport array binding
        bindingBaseParams = mapMaybe makeBindingBaseParam modportBindings
        makeBindingBaseParam :: ModportBinding -> Maybe Decl
        makeBindingBaseParam (portName, (_, modportE)) =
            fmap (localparam $ bindingBaseName ++ portName) $
                makeBindingBaseExpr modportE
        bindingBaseName = "_bbase_" ++ key ++ "_"
        makeBindingBaseExpr :: Expr -> Maybe Expr
        makeBindingBaseExpr modportE =
            case modportE of
                Dot (Range _ mode range) _ -> Just $ sliceLo mode range
                Range      _ mode range    -> Just $ sliceLo mode range
                Dot (Bit _ idx) _ -> Just idx
                Bit      _ idx    -> Just idx
                _ -> Nothing

        localparam :: Identifier -> Expr -> Decl
        localparam = Param Localparam (Implicit Unspecified [])

        paramTmp = "_param_" ++ key ++ "_"

        parameterBinds = map makeParameterBind instanceParams
        makeParameterBind :: ParamBinding -> Decl
        makeParameterBind (x, Left t) =
            ParamType Localparam (paramTmp ++ x) t
        makeParameterBind (x, Right e) =
            Param Localparam UnknownType (paramTmp ++ x) e

        overrideParam :: Decl -> Decl
        overrideParam (Param Parameter t x e) =
            Param Localparam t x $
            case lookup x instanceParams of
                Nothing -> e
                Just _  -> Ident $ paramTmp ++ x
        overrideParam (ParamType Parameter x t) =
            ParamType Localparam x $
            case lookup x instanceParams of
                Nothing -> t
                Just _  -> Alias (paramTmp ++ x) []
        overrideParam other = other

        portBindingItem :: PortBinding -> ModuleItem
        portBindingItem (ident, expr) =
            if findDeclDir ident == Input
                then bind (LHSDot (inj LHSBit LHSIdent) ident) expr
                else bind (toLHS expr) (Dot (inj Bit Ident) ident)
            where
                bind = Assign AssignOptionNone
                inj bit idn = if null ranges
                    then idn instanceName
                    else bit (idn instanceName) (Ident loopVar)

        declDirs = execWriter $
            mapM (collectDeclsM collectDeclDir) items
        collectDeclDir :: Decl -> Writer (Map.Map Identifier Direction) ()
        collectDeclDir (Variable dir _ ident _ _) =
            when (dir /= Local) $
                tell $ Map.singleton ident dir
        collectDeclDir net@Net{} =
            collectNetAsVarM collectDeclDir net
        collectDeclDir _ = return ()
        findDeclDir :: Identifier -> Direction
        findDeclDir ident =
            case Map.lookup ident declDirs of
                Nothing -> error $ "could not find decl dir of " ++ ident
                    ++ " among " ++ show declDirs
                Just dir -> dir

        toLHS :: Expr -> LHS
        toLHS expr =
            case exprToLHS expr of
                Just lhs -> lhs
                Nothing  -> error $ "trying to bind an " ++ inlineKind
                    ++ " output to " ++ show expr ++ " but that can't be an LHS"

        -- for instance arrays, a unique identifier to be used as a genvar
        loopVar = "_arr_" ++ key

        isArray = not $ null ranges
        [arrayRange@(arrayLeft, arrayRight)] = ranges

        -- wrap the given item in a generate loop if necessary
        wrapInstance :: Identifier -> [ModuleItem] -> ModuleItem
        wrapInstance blockName moduleItems =
            Generate $
            if not isArray then
                [item]
            else
                [ GenModuleItem (Genvar loopVar)
                , GenFor inits cond incr item
                ]
            where
                item = GenBlock blockName $ map GenModuleItem moduleItems
                inits = (loopVar, arrayLeft)
                cond = endianCondExpr arrayRange
                    (BinOp Ge (Ident loopVar) arrayRight)
                    (BinOp Le (Ident loopVar) arrayRight)
                incr = (loopVar, AsgnOp Add, step)
                step = endianCondExpr arrayRange
                    (UniOp UniSub $ RawNum 1) (RawNum 1)

-- used for modport array binding offset placeholders
pattern Tag :: Expr
pattern Tag = Ident "%"

modportBaseName :: Identifier -> Identifier
modportBaseName = (++) "_mbase_"

-- the dimensions of interface instance arrays are encoded as synthetic modports
-- during inlining, enabling subsequent modport bindings to implicitly use the
-- bounds of the interface instance array when the bounds are unspecified
pattern InstArrName :: Identifier
pattern InstArrName = "~instance_array_dimensions~"
pattern InstArrVal :: Expr -> Expr -> [ModportDecl]
pattern InstArrVal l r = [(Local, "l", l), (Local, "r", r)]
pattern InstArrKey :: Expr -> Expr
pattern InstArrKey expr = Dot (Bit expr (RawNum 0)) InstArrName
pattern InstArrEncoded :: Expr -> Expr -> ModuleItem
pattern InstArrEncoded l r = Modport InstArrName (InstArrVal l r)

-- encoding for normal declarations in the current module
pattern DeclVal :: [ModportDecl]
pattern DeclVal = [(Local, "~decl~", Nil)]

-- encoding for the interface type of an interface instantiation
pattern InterfaceTypeName :: Identifier
pattern InterfaceTypeName = "~interface_type~"
pattern InterfaceTypeVal :: Identifier -> [ModportDecl]
pattern InterfaceTypeVal x = [(Local, "~interface~type~", Ident x)]
pattern InterfaceTypeKey :: Expr -> Expr
pattern InterfaceTypeKey e = Dot e InterfaceTypeName
pattern InterfaceTypeEncoded :: Identifier -> ModuleItem
pattern InterfaceTypeEncoded x = Modport InterfaceTypeName (InterfaceTypeVal x)

-- determines the lower bound for the given slice
sliceLo :: PartSelectMode -> Range -> Expr
sliceLo NonIndexed (l, r) = endianCondExpr (l, r) r l
sliceLo IndexedPlus (base, _) = base
sliceLo IndexedMinus (base, len) = BinOp Add (BinOp Sub base len) (RawNum 1)

-- check for cases where an expression in an inlined part only resolves after
-- inlining, potentially hiding a design error
checkBeforeInline :: Scopes a -> Identifier -> [ModuleItem]
    -> (String -> String) -> [ModuleItem]
checkBeforeInline global partName items checkErrMsg =
    evalScoper $ scopeModuleItems scoper partName $ items
    where
        scoper = scopeModuleItem
            checkDecl checkModuleItem checkGenItem checkStmt

        checkDecl :: Decl -> Scoper () Decl
        checkDecl decl = do
            case decl of
                Variable  _ _ x _ _ -> insertElem x ()
                Net   _ _ _ _ x _ _ -> insertElem x ()
                Param     _ _ x _   -> insertElem x ()
                ParamType _   x _   -> insertElem x ()
                CommentDecl{} -> return ()
            traverseDeclExprsM checkExpr decl

        checkModuleItem :: ModuleItem -> Scoper () ModuleItem
        checkModuleItem item@(Instance _ _ x _ _) =
            insertElem x () >> traverseExprsM checkExpr item
        checkModuleItem item =
            traverseExprsM checkExpr item >>=
            traverseLHSsM  checkLHS

        checkGenItem :: GenItem -> Scoper () GenItem
        checkGenItem = traverseGenItemExprsM checkExpr

        checkStmt :: Stmt -> Scoper () Stmt
        checkStmt =
            traverseStmtExprsM checkExpr >=>
            traverseStmtLHSsM  checkLHS

        checkExpr :: Expr -> Scoper () Expr
        checkExpr = embedScopes checkExprResolutionId

        checkLHS :: LHS -> Scoper () LHS
        checkLHS = embedScopes checkLHSResolutionId

        checkLHSResolutionId :: Scopes () -> LHS -> LHS
        checkLHSResolutionId local lhs = checkExprResolution local expr lhs
            where expr = lhsToExpr lhs

        checkExprResolutionId :: Scopes () -> Expr -> Expr
        checkExprResolutionId local expr = checkExprResolution local expr expr

        -- error if the given expression resolves globally but not locally
        checkExprResolution :: Scopes () -> Expr -> a -> a
        checkExprResolution local expr =
            if exprResolves global expr && not (anyPrefixResolves local expr)
                then scopedError local $ checkErrMsg $ show expr
                else id

        -- check if hierarchical prefix of an expr exists in the given scope
        anyPrefixResolves :: Scopes () -> Expr -> Bool
        anyPrefixResolves local expr =
            exprResolves local expr ||
            case expr of
                Dot inner _ -> anyPrefixResolves local inner
                Bit inner _ -> anyPrefixResolves local inner
                _ -> False

        -- check if expr exists in the given scope
        exprResolves :: Scopes a -> Expr -> Bool
        exprResolves local (Ident x) =
            isJust (lookupElem local x) || isLoopVar local x
        exprResolves local expr =
            isJust (lookupElem local expr)
