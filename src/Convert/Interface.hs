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

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

data PartInfo = PartInfo
    { pKind :: PartKW
    , pPorts :: [Identifier]
    , pItems :: [ModuleItem]
    } deriving Eq
type PartInfos = Map.Map Identifier PartInfo

type ModportInstances = [(Identifier, (Identifier, Identifier))]
type ModportBinding = (Identifier, (Expr, Expr))

convert :: [AST] -> [AST]
convert =
    traverseFiles (collectDescriptionsM collectPart)
    (map . convertDescription)
    where
        -- we can only collect/map non-extern interfaces and modules
        collectPart :: Description -> Writer PartInfos ()
        collectPart (Part _ False kw _ name ports items) =
            tell $ Map.singleton name $ PartInfo kw ports items
        collectPart _ = return ()

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
        traverseModuleItemM (instanceItem @ (Instance _ _ _ [] _)) =
            if maybePartInfo == Nothing then
                return instanceItem
            else if partKind == Interface then
                -- inline instantiation of an interface
                convertNested $ Generate $ map GenModuleItem $
                    inlineInstance [] []
                    partItems instanceName paramBindings portBindings
            else if not $ null (extractModportInstances partInfo) then do
                modports <- embedScopes (\l () -> l) ()
                -- inline instantiation of a module
                convertNested $ Generate $ map GenModuleItem $
                    inlineInstance
                        (modportBindings modports)
                        (modportSubstitutions modports)
                        partItems instanceName paramBindings portBindings
            else
                return instanceItem
            where
                Instance part rawParamBindings instanceName [] rawPortBindings =
                    instanceItem
                maybePartInfo = Map.lookup part parts
                Just partInfo = maybePartInfo
                PartInfo partKind partPorts partItems = partInfo

                partParams = parameterNames partItems
                paramBindings = resolveBindings partParams rawParamBindings
                portBindings = resolveBindings partPorts rawPortBindings

                modportInstances = extractModportInstances partInfo
                modportBindings modports = mapMaybe
                    (inferModportBinding modports modportInstances) portBindings
                modportSubstitutions modports = concatMap
                    (expandModportBinding modports) (modportBindings modports)

        traverseModuleItemM other = return other

        -- determines the underlying modport and interface instances associated
        -- with the given port binding, if it is a modport binding
        inferModportBinding :: Scopes [ModportDecl] -> ModportInstances ->
            PortBinding -> Maybe ModportBinding
        inferModportBinding _ _ ("", _) =
            error "internal inferModportBinding invariant violated"
        inferModportBinding modports modportInstances (portName, expr) =
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
                    Just (_, x) -> x
                    Nothing -> error $ "can't deduce modport for interface "
                                    ++ " bound to port " ++ portName

                foundModport modportE =
                    Just (portName, (instanceE, modportE))
                    where instanceE = findInstance modportE
                findInstance :: Expr -> Expr
                findInstance e =
                    case lookupElem modports (Dot e "") of
                        Nothing -> case e of
                            Bit e' _ -> findInstance e'
                            Dot e' _ -> findInstance e'
                            _ -> error "internal invariant violated"
                        Just (accesses, _, _) ->
                            foldl accessToExpr (Ident topName) rest
                            where Access topName Nil : rest = init accesses
                accessToExpr :: Expr -> Access -> Expr
                accessToExpr e (Access x Nil) = Dot e x
                accessToExpr e (Access x i) = Bit (Dot e x) i

        -- expand a modport binding into a series of expression substitutions
        expandModportBinding :: Scopes [ModportDecl]
            -> ModportBinding -> [(Expr, Expr)]
        expandModportBinding modports (portName, (instanceE, modportE)) =
            (Ident portName, instanceE) :
            map toPortBinding modportDecls
            where
                a = lookupElem modports modportE
                b = lookupElem modports (Dot modportE "")
                Just (_, replacements, modportDecls) =
                    if a == Nothing then b else a
                toPortBinding (_, x, e) = (x', e')
                    where
                        x' = Dot (Ident portName) x
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

        extractModportInfo :: Type -> Maybe (Identifier, Identifier)
        extractModportInfo (InterfaceT "" Nothing []) = Just ("", "")
        extractModportInfo (InterfaceT interfaceName (Just modportName) []) =
            if isInterface interfaceName
                then Just (interfaceName, modportName)
                else Nothing
        extractModportInfo (Alias interfaceName []) =
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

-- produce the implicit modport decls for an interface bundle
impliedModport :: [ModuleItem] -> [ModportDecl]
impliedModport =
    execWriter . mapM (collectNestedModuleItemsM collectModportDecls)
    where
        collectModportDecls :: ModuleItem -> Writer [ModportDecl] ()
        collectModportDecls (MIPackageItem (Decl (Variable d _ x _ _))) =
            tell [(d', x, Ident x)]
            where d' = if d == Local then Inout else d
        collectModportDecls _ = return ()

-- convert an interface-bound module instantiation or an interface instantiation
-- into a series of equivalent inlined module items
inlineInstance :: [ModportBinding] -> [(Expr, Expr)] -> [ModuleItem]
    -> Identifier -> [ParamBinding] -> [PortBinding] -> [ModuleItem]
inlineInstance modportBindings modportSubstitutions items
    instanceName instanceParams instancePorts =
    comment :
    map (MIPackageItem . Decl) parameterBinds ++
    Generate [GenBlock instanceName $ map GenModuleItem items']
    : portBindings
    where
        items' = evalScoper
            traverseDeclM traverseModuleItemM traverseGenItemM traverseStmtM ""
            $ map (traverseNestedModuleItems rewriteItem) $
            if null modportBindings
                then Modport "" (impliedModport items) : items
                else items

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
            lookup (portName) modportBindings == Nothing

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
        replaceLHS lhs =
            case lookup lhs lhsReplacements of
                Just lhs' -> lhs'
                Nothing -> traverseSinglyNestedLHSs replaceLHS lhs
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
                Just (_, _, Nil) -> Dot expr "@"
                Just ([_, _], replacements, expr') ->
                    if substitute && Map.null replacements
                        then Dot expr' "@"
                        else Dot expr "@"
                Just (_, _, _) -> Dot expr "@"
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
        replaceExpr' expr =
            case lookup expr exprReplacements of
                Just expr' -> expr'
                Nothing -> traverseSinglyNestedExprs replaceExpr' expr

        removeModportInstance :: ModuleItem -> ModuleItem
        removeModportInstance (MIPackageItem (Decl (Variable d t x a e))) =
            MIPackageItem $ Decl $
            if lookup x modportBindings /= Nothing
                then CommentDecl $ "removed modport instance " ++ x
                else Variable d t x a e
        removeModportInstance other = other

        removeDeclDir :: ModuleItem -> ModuleItem
        removeDeclDir (MIPackageItem (Decl (Variable _ t x a e))) =
            MIPackageItem $ Decl $ Variable Local t' x a e
            where t' = case t of
                    Implicit Unspecified rs ->
                        IntegerVector TLogic Unspecified rs
                    _ -> t
        removeDeclDir other = other

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
            if findDeclDir ident == Input
                then bind (LHSDot (LHSIdent instanceName) ident) expr
                else bind (toLHS expr) (Dot (Ident instanceName) ident)
            where bind a b = Just $ Assign AssignOptionNone a b

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
