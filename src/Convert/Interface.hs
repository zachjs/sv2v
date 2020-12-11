{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for interfaces
 -}

module Convert.Interface (convert) where

import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
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
    } deriving Eq
type PartInfos = Map.Map Identifier PartInfo

type ModportInstances = [(Identifier, Identifier)]
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
    if null $ extractModportInstances $ PartInfo Module ports items then
        Part attrs extern Module lifetime name ports items'
    else
        PackageItem $ Decl $ CommentDecl $
            "removed interface-using module: " ++ name
    where
        items' = evalScoper return traverseModuleItemM return return name items

        convertNested =
            scopeModuleItemT return traverseModuleItemM return return

        traverseModuleItemM :: ModuleItem -> Scoper [ModportDecl] ModuleItem
        traverseModuleItemM (Modport modportName modportDecls) =
            insertElem modportName modportDecls >> return (Generate [])
        traverseModuleItemM (instanceItem @ Instance{}) =
            if maybePartInfo == Nothing then
                return instanceItem
            else if partKind == Interface then
                -- inline instantiation of an interface
                convertNested $ Generate $ map GenModuleItem $
                    inlineInstance rs []
                    partItems instanceName paramBindings portBindings
            else if not $ null (extractModportInstances partInfo) then do
                modports <- embedScopes (\l () -> l) ()
                -- inline instantiation of a module
                convertNested $ Generate $ map GenModuleItem $
                    inlineInstance rs
                        (modportBindings modports)
                        partItems instanceName paramBindings portBindings
            else
                return instanceItem
            where
                Instance part rawParamBindings instanceName rs rawPortBindings =
                    instanceItem
                maybePartInfo = Map.lookup part parts
                Just partInfo = maybePartInfo
                PartInfo partKind partPorts partItems = partInfo

                partParams = parameterNames partItems
                paramBindings = resolveBindings partParams rawParamBindings
                portBindings = resolveBindings partPorts rawPortBindings

                modportInstances = extractModportInstances partInfo
                modportBindings modports = mapMaybe
                    (inferModportBinding modports modportInstances) $
                    map (second $ addImpliedSlice modports) portBindings
                second f = \(a, b) -> (a, f b)

        traverseModuleItemM other = return other

        -- add explicit slices for bindings of entire modport instance arrays
        addImpliedSlice :: Scopes [ModportDecl] -> Expr -> Expr
        addImpliedSlice modports (orig @ (Dot expr modportName)) =
            case lookupElem modports (InstArrKey expr) of
                Just (_, _, InstArrVal l r) ->
                    Dot (Range expr NonIndexed (l, r)) modportName
                _ -> orig
        addImpliedSlice modports expr =
            case lookupElem modports (InstArrKey expr) of
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
            else
                Nothing
            where
                bindingIsModport = lookupElem modports expr /= Nothing
                bindingIsBundle = lookupElem modports (Dot expr "") /= Nothing
                portIsBundle = null modportName
                modportName = case lookup portName modportInstances of
                    Just x -> x
                    Nothing -> error $ "can't deduce modport for interface "
                        ++ show expr ++ " bound to port " ++ portName

                foundModport modportE =
                    Just (findInstance modportE, qualifyModport modportE)

                findInstance :: Expr -> Expr
                findInstance e =
                    case lookupElem modports (Dot e "") of
                        Nothing -> case e of
                            Bit e' _ -> findInstance e'
                            Dot e' _ -> findInstance e'
                            _ -> error "internal invariant violated"
                        Just (accesses, _, _) -> accessesToExpr $ init accesses
                qualifyModport :: Expr -> Expr
                qualifyModport e =
                    case lookupElem modports e of
                        Just (accesses, _, _) -> accessesToExpr accesses
                        Nothing -> accessesToExpr $ init accesses
                            where Just (accesses, _, _) =
                                    lookupElem modports (Dot e "")
                accessesToExpr :: [Access] -> Expr
                accessesToExpr accesses =
                    foldl accessToExpr (Ident topName) rest
                    where Access topName Nil : rest = accesses
                accessToExpr :: Expr -> Access -> Expr
                accessToExpr e (Access x Nil) = Dot e x
                accessToExpr e (Access x i) = Bit (Dot e x) i

        -- expand a modport binding into a series of expression substitutions
        genSubstitutions :: Scopes [ModportDecl] -> Expr -> Expr -> Expr
            -> [(Expr, Expr)]
        genSubstitutions modports baseE instanceE modportE =
            (baseE, instanceE) :
            map toPortBinding modportDecls
            where
                a = lookupElem modports modportE
                b = lookupElem modports (Dot modportE "")
                Just (_, replacements, modportDecls) =
                    if a == Nothing then b else a
                toPortBinding (_, x, e) = (x', e')
                    where
                        x' = Dot baseE x
                        e' = prefixExpr e
                prefixExpr :: Expr -> Expr
                prefixExpr (Ident x) =
                    case Map.lookup x replacements of
                        Just replacement -> replacement
                        Nothing ->
                            if "_tmp_" `isPrefixOf` x
                                then Ident x
                                else Dot instanceE x
                prefixExpr other = traverseSinglyNestedExprs prefixExpr other

        -- association list of modport instances in the given module body
        extractModportInstances :: PartInfo -> ModportInstances
        extractModportInstances partInfo =
            execWriter $ mapM (collectDeclsM collectDecl) (pItems partInfo)
            where
                collectDecl :: Decl -> Writer ModportInstances ()
                collectDecl (Variable _ t x _ _) =
                    if maybeInfo == Nothing then
                        return ()
                    else if elem x (pPorts partInfo) then
                        tell [(x, info)]
                    else
                        error $ "Modport not in port list: " ++ show (t, x)
                            ++ ". Is this an interface missing a port list?"
                    where
                        maybeInfo = extractModportInfo t
                        Just info = maybeInfo
                collectDecl _ = return ()

        extractModportInfo :: Type -> Maybe Identifier
        extractModportInfo (InterfaceT "" Nothing []) = Just ""
        extractModportInfo (InterfaceT interfaceName (Just modportName) []) =
            if isInterface interfaceName
                then Just modportName
                else Nothing
        extractModportInfo (Alias interfaceName []) =
            if isInterface interfaceName
                then Just ""
                else Nothing
        extractModportInfo _ = Nothing

        isInterface :: Identifier -> Bool
        isInterface partName =
            case Map.lookup partName parts of
                Nothing -> False
                Just info -> pKind info == Interface

convertDescription _ other = other

-- produce the implicit modport decls for an interface bundle
impliedModport :: [ModuleItem] -> [ModportDecl]
impliedModport =
    execWriter . mapM (collectNestedModuleItemsM collectModportDecls)
    where
        collectModportDecls :: ModuleItem -> Writer [ModportDecl] ()
        collectModportDecls (MIPackageItem (Decl (Variable _ _ x _ _))) =
            tell [(Inout, x, Ident x)]
        collectModportDecls _ = return ()

-- convert an interface-bound module instantiation or an interface instantiation
-- into a series of equivalent inlined module items
inlineInstance :: [Range] -> [ModportBinding] -> [ModuleItem]
    -> Identifier -> [ParamBinding] -> [PortBinding] -> [ModuleItem]
inlineInstance ranges modportBindings items
    instanceName instanceParams instancePorts =
    comment :
    map (MIPackageItem . Decl) bindingBaseParams ++
    map (MIPackageItem . Decl) parameterBinds ++
    (wrapInstance $ GenBlock instanceName $ map GenModuleItem items')
    : portBindings
    where
        items' = evalScoper
            traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM ""
            $ map (traverseNestedModuleItems rewriteItem) $
            if null modportBindings
                then dimensionModport : bundleModport : items
                else items

        -- synthetic modports to be collected and removed after inlining
        bundleModport = Modport "" (impliedModport items)
        dimensionModport = if not isArray
            then MIPackageItem $ Decl $ CommentDecl "not an instance array"
            else InstArrEncoded arrayLeft arrayRight

        inlineKind =
            if null modportBindings
                then "interface"
                else "interface-using module"

        comment = MIPackageItem $ Decl $ CommentDecl $
            "expanded " ++ inlineKind ++ " instance: " ++ instanceName
        portBindings = mapMaybe portBindingItem $
            filter notSubstituted instancePorts
        notSubstituted :: PortBinding -> Bool
        notSubstituted (portName, _) =
            lookup portName modportBindings == Nothing

        rewriteItem :: ModuleItem -> ModuleItem
        rewriteItem =
            removeModportInstance .
            removeDeclDir .
            traverseDecls overrideParam

        traverseDeclM :: Decl -> Scoper Expr Decl
        traverseDeclM decl = do
            decl' <- traverseDeclExprsM substituteExprM decl
            case decl' of
                Variable  _ _ x _ _ -> insertElem x Nil
                Param     _ _ x e   -> insertElem x e
                ParamType _   x _   -> insertElem x Nil
                CommentDecl{} -> return ()
            return decl'

        traverseModuleItemM :: ModuleItem -> Scoper Expr ModuleItem
        traverseModuleItemM (item @ Modport{}) =
            traverseExprsM substituteExprM item
        traverseModuleItemM item =
            traverseExprsM traverseExprM item >>=
            traverseLHSsM  traverseLHSM

        traverseGenItemM :: GenItem -> Scoper Expr GenItem
        traverseGenItemM = traverseGenItemExprsM traverseExprM

        traverseStmtM :: Stmt -> Scoper Expr Stmt
        traverseStmtM =
            traverseStmtExprsM traverseExprM >=>
            traverseStmtLHSsM  traverseLHSM

        -- used for replacing usages of modports in the module being inlined
        modportSubstitutions = concatMap (fst . snd) modportBindings
        lhsReplacements = map (\(x, y) -> (toLHS x, toLHS y)) exprReplacements
        exprReplacements = filter ((/= Nil) . snd) modportSubstitutions
        -- LHSs are replaced using simple substitutions
        traverseLHSM :: LHS -> Scoper Expr LHS
        traverseLHSM lhs = do
            lhs' <- embedScopes tagLHS lhs
            return $ replaceLHS lhs'
        tagLHS :: Scopes Expr -> LHS -> LHS
        tagLHS scopes lhs =
            if lookupElem scopes lhs /= Nothing
                then LHSDot lhs "@"
                else traverseSinglyNestedLHSs (tagLHS scopes) lhs
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
        traverseExprM :: Expr -> Scoper Expr Expr
        traverseExprM expr = do
            expr' <- embedScopes (tagExpr False) expr
            return $ replaceExpr expr'
        substituteExprM :: Expr -> Scoper Expr Expr
        substituteExprM expr = do
            expr' <- embedScopes (tagExpr True) expr
            return $ replaceExpr expr'
        tagExpr :: Bool -> Scopes Expr -> Expr -> Expr
        tagExpr substitute scopes expr =
            case lookupElem scopes expr of
                Just (_, _, expr') ->
                    if substitute && expr' /= Nil
                        then Dot expr' "@"
                        else Dot expr "@"
                Nothing ->
                    traverseSinglyNestedExprs (tagExpr substitute scopes) expr
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
        replaceExpr' expr =
            case lookup expr exprReplacements of
                Just expr' -> expr'
                Nothing -> traverseSinglyNestedExprs replaceExpr' expr
        replaceArrTag :: Expr -> Expr -> Expr
        replaceArrTag replacement Tag = replacement
        replaceArrTag replacement expr =
            traverseSinglyNestedExprs (replaceArrTag replacement) expr

        removeModportInstance :: ModuleItem -> ModuleItem
        removeModportInstance (MIPackageItem (Decl (Variable d t x a e))) =
            MIPackageItem $ Decl $
            if maybeModportBinding == Nothing then
                Variable d t x a e
            else if makeBindingBaseExpr modportE == Nothing then
                CommentDecl $ "removed modport instance " ++ x
            else if null a then
                localparam (modportBaseName x) bindingBaseExpr
            else
                localparam (modportBaseName x) $
                    BinOp Sub bindingBaseExpr (sliceLo NonIndexed $ head a)
            where
                maybeModportBinding = lookup x modportBindings
                Just (_, modportE) = maybeModportBinding
                bindingBaseExpr = Ident $ bindingBaseName ++ x

        removeModportInstance other = other

        removeDeclDir :: ModuleItem -> ModuleItem
        removeDeclDir (MIPackageItem (Decl (Variable _ t x a e))) =
            MIPackageItem $ Decl $ Variable Local t' x a e
            where t' = case t of
                    Implicit Unspecified rs ->
                        IntegerVector TLogic Unspecified rs
                    _ -> t
        removeDeclDir other = other

        -- capture the lower bound for each modport array binding
        bindingBaseParams = map makeBindingBaseParam modportBindings
        makeBindingBaseParam :: ModportBinding -> Decl
        makeBindingBaseParam (portName, (_, modportE)) =
            case makeBindingBaseExpr modportE of
                Just expr -> localparam (bindingBaseName ++ portName) expr
                Nothing -> CommentDecl "no-op"
        bindingBaseName = "_sv2v_bind_base_" ++ shortHash instanceName ++ "_"
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

        paramTmp = "_tmp_" ++ (shortHash (items, instanceName)) ++ "_"

        parameterBinds = map makeParameterBind instanceParams
        makeParameterBind :: ParamBinding -> Decl
        makeParameterBind (x, Left t) =
            ParamType Localparam (paramTmp ++ x) (Just t)
        makeParameterBind (x, Right e) =
            Param Localparam (TypeOf e) (paramTmp ++ x) e

        overrideParam :: Decl -> Decl
        overrideParam (Param Parameter t x e) =
            case lookup x instanceParams of
                Nothing -> Param Localparam t x e
                Just (Right _) -> Param Localparam t x (Ident $ paramTmp ++ x)
                Just (Left t') -> error $ inlineKind ++ " param " ++ x
                        ++ " expected expr, found type: " ++ show t'
        overrideParam (ParamType Parameter x mt) =
            case lookup x instanceParams of
                Nothing -> ParamType Localparam x mt
                Just (Left _) ->
                    ParamType Localparam x (Just $ Alias (paramTmp ++ x) [])
                Just (Right e') -> error $ inlineKind ++ " param " ++ x
                        ++ " expected type, found expr: " ++ show e'
        overrideParam other = other

        portBindingItem :: PortBinding -> Maybe ModuleItem
        portBindingItem (_, Nil) = Nothing
        portBindingItem (ident, expr) =
            Just $ wrapInstance $ GenModuleItem $
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
        loopVar = "_sv2v_arr_" ++ shortHash (instanceName, ranges)

        isArray = not $ null ranges
        [arrayRange @ (arrayLeft, arrayRight)] = ranges

        -- wrap the given item in a generate loop if necessary
        wrapInstance :: GenItem -> ModuleItem
        wrapInstance item =
            Generate $
            if not isArray then
                [item]
            else
                [ GenModuleItem (Genvar loopVar)
                , GenFor inits cond incr item
                ]
            where
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
modportBaseName = (++) "_sv2v_base_"

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

type Binding t = (Identifier, t)
-- give a set of bindings explicit names
resolveBindings :: Show t => [Identifier] -> [Binding t] -> [Binding t]
resolveBindings available bindings =
    zipWith resolveBinding bindings [0..]
    where
        resolveBinding ("", e) idx =
            if idx < length available
                then (available !! idx, e)
                else error $ "binding " ++ show e ++ " is out of range "
                        ++ show available
        resolveBinding other _ = other

-- given a list of module items, produces the parameter names in order
parameterNames :: [ModuleItem] -> [Identifier]
parameterNames =
    execWriter . mapM (collectNestedModuleItemsM $ collectDeclsM collectDeclM)
    where
        collectDeclM :: Decl -> Writer [Identifier] ()
        collectDeclM (Param Parameter   _ x _) = tell [x]
        collectDeclM (ParamType Parameter x _) = tell [x]
        collectDeclM _ = return ()

-- determines the lower bound for the given slice
sliceLo :: PartSelectMode -> Range -> Expr
sliceLo NonIndexed (l, r) = endianCondExpr (l, r) r l
sliceLo IndexedPlus (base, _) = base
sliceLo IndexedMinus (base, len) = BinOp Add (BinOp Sub base len) (RawNum 1)
