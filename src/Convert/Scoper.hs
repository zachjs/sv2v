{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Standardized scope traversal utilities
 -
 - This module provides a series of "scopers" which track the scope of blocks,
 - generate loops, tasks, and functions, and provides the ability to insert and
 - lookup elements in a scope-aware way. It also provides the ability to check
 - whether the current node is within a procedural context.
 -
 - The interfaces take in a mappers for each of: Decl, ModuleItem, GenItem, and
 - Stmt. Note that Function, Task, Always, Initial, and Final are NOT passed
 - through the ModuleItem mapper as those constructs only provide Stmts and
 - Decls. For the same reason, Decl ModuleItems are not passed through the
 - ModuleItem mapper.
 -
 - All of the mappers should not recursively traverse any of the items captured
 - by any of the other mappers. Scope resolution enforces data declaration
 - ordering.
 -}

module Convert.Scoper
    ( Scoper
    , ScoperT
    , evalScoper
    , evalScoperT
    , runScoper
    , runScoperT
    , partScoper
    , partScoperT
    , accessesToExpr
    , replaceInType
    , replaceInExpr
    , scopeExpr
    , scopeType
    , insertElem
    , injectItem
    , injectDecl
    , lookupElem
    , lookupElemM
    , Access(..)
    , ScopeKey
    , Scopes
    , extractMapping
    , embedScopes
    , withinProcedure
    , withinProcedureM
    , isLoopVar
    , isLoopVarM
    , lookupLocalIdent
    , lookupLocalIdentM
    , scopeModuleItemT
    , Replacements
    , LookupResult
    ) where

import Control.Monad.State.Strict
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

-- user monad aliases
type Scoper a = State (Scopes a)
type ScoperT a m = StateT (Scopes a) m

-- one tier of scope construction
data Tier = Tier
    { tierName :: Identifier
    , tierIndex :: Identifier
    } deriving (Eq, Show)

-- one layer of scope inspection
data Access = Access
    { accessName :: Identifier
    , accessIndex :: Expr
    } deriving (Eq, Show)

type Mapping a = Map.Map Identifier (Entry a)

data Entry a = Entry
    { eElement :: Maybe a
    , eIndex :: Identifier
    , eMapping :: Mapping a
    } deriving Show

data Scopes a = Scopes
    { sCurrent :: [Tier]
    , sMapping :: Mapping a
    , sProcedure :: Bool
    , sInjectedItems :: [ModuleItem]
    , sInjectedDecls :: [Decl]
    } deriving Show

extractMapping :: Scopes a -> Map.Map Identifier a
extractMapping =
    Map.mapMaybe eElement .
    eMapping . snd .
    Map.findMin . sMapping

embedScopes :: Monad m => (Scopes a -> b -> c) -> b -> ScoperT a m c
embedScopes func x = do
    scopes <- get
    return $ func scopes x

setScope :: [Tier] -> Entry a -> Mapping a -> Mapping a
setScope [] _ = error "setScope invariant violated"
setScope [Tier name _] newEntry =
    Map.insert name newEntry
setScope (Tier name _ : tiers) newEntry =
    Map.adjust adjustment name
    where
        adjustment entry =
            entry { eMapping = setScope tiers newEntry (eMapping entry) }

enterScope :: Monad m => Identifier -> Identifier -> ScoperT a m ()
enterScope name index = do
    s <- get
    let current' = sCurrent s ++ [Tier name index]
    let existingResult = lookupLocalIdent s name
    let existingElement = fmap thd3 existingResult
    let entry = Entry existingElement index Map.empty
    let mapping' = setScope current' entry $ sMapping s
    put $ s { sCurrent = current', sMapping = mapping'}
    where thd3 (_, _, c) = c

exitScope :: Monad m => ScoperT a m ()
exitScope = modify' $ \s -> s { sCurrent = init $ sCurrent s }

enterProcedure :: Monad m => ScoperT a m ()
enterProcedure = modify' $ \s -> s { sProcedure = True }

exitProcedure :: Monad m => ScoperT a m ()
exitProcedure = modify' $ \s -> s { sProcedure = False }

exprToAccesses :: [Access] -> Expr -> Maybe [Access]
exprToAccesses accesses (Ident x) =
    Just $ Access x Nil : accesses
exprToAccesses accesses (Bit (Ident x) y) =
    Just $ Access x y : accesses
exprToAccesses accesses (Bit (Dot e x) y) =
    exprToAccesses (Access x y : accesses) e
exprToAccesses accesses (Dot e x) =
    exprToAccesses (Access x Nil : accesses) e
exprToAccesses _ _ = Nothing

accessesToExpr :: [Access] -> Expr
accessesToExpr accesses =
    foldl accessToExpr (Ident topName) rest
    where Access topName Nil : rest = accesses

accessToExpr :: Expr -> Access -> Expr
accessToExpr e (Access x Nil) = Dot e x
accessToExpr e (Access x i) = Bit (Dot e x) i

replaceInType :: Replacements -> Type -> Type
replaceInType replacements =
    if Map.null replacements
        then id
        else replaceInType' replacements

replaceInType' :: Replacements -> Type -> Type
replaceInType' replacements =
    traverseNestedTypes $ traverseTypeExprs $ replaceInExpr' replacements

replaceInExpr :: Replacements -> Expr -> Expr
replaceInExpr replacements =
    if Map.null replacements
        then id
        else replaceInExpr' replacements

replaceInExpr' :: Replacements -> Expr -> Expr
replaceInExpr' replacements (Ident x) =
    Map.findWithDefault (Ident x) x replacements
replaceInExpr' replacements other =
    traverseExprTypes (replaceInType' replacements) $
    traverseSinglyNestedExprs (replaceInExpr' replacements) other

-- rewrite an expression so that any identifiers it contains unambiguously refer
-- refer to currently visible declarations so it can be substituted elsewhere
scopeExpr :: Monad m => Expr -> ScoperT a m Expr
scopeExpr expr = do
    expr' <- traverseSinglyNestedExprsM scopeExpr expr
                >>= traverseExprTypesM scopeType
    details <- lookupElemM expr'
    case details of
        Just (accesses, _, _) -> return $ accessesToExpr accesses
        _ -> return expr'
scopeType :: Monad m => Type -> ScoperT a m Type
scopeType = traverseNestedTypesM $ traverseTypeExprsM scopeExpr

{-# INLINABLE scopeExpr #-}
{-# INLINABLE scopeType #-}

class ScopePath k where
    toTiers :: Scopes a -> k -> [Tier]

instance ScopePath Identifier where
    toTiers scopes name = sCurrent scopes ++ [Tier name ""]

instance ScopePath [Access] where
    toTiers _ = map toTier
        where
            toTier :: Access -> Tier
            toTier (Access x Nil) = Tier x ""
            toTier (Access x iy) = Tier x y
                where Ident y = iy

insertElem :: Monad m => ScopePath k => k -> a -> ScoperT a m ()
insertElem key element = do
    s <- get
    let mapping = sMapping s
    let entry = Entry (Just element) "" Map.empty
    let mapping' = setScope (toTiers s key) entry mapping
    put $ s { sMapping = mapping' }

injectItem :: Monad m => ModuleItem -> ScoperT a m ()
injectItem item =
    modify' $ \s -> s { sInjectedItems = item : sInjectedItems s }

injectDecl :: Monad m => Decl -> ScoperT a m ()
injectDecl decl =
    modify' $ \s -> s { sInjectedDecls = decl : sInjectedDecls s }

consumeInjectedItems :: Monad m => ScoperT a m [ModuleItem]
consumeInjectedItems = do
    injected <- gets sInjectedItems
    when (not $ null injected) $
        modify' $ \s -> s { sInjectedItems = [] }
    return $ reverse injected

consumeInjectedDecls :: Monad m => ScoperT a m [Decl]
consumeInjectedDecls = do
    injected <- gets sInjectedDecls
    when (not $ null injected) $
        modify' $ \s -> s { sInjectedDecls = [] }
    return $ reverse injected

type Replacements = Map.Map Identifier Expr

-- lookup accesses by direct match (no search)
directResolve :: Mapping a -> [Access] -> Maybe (Replacements, a)
directResolve _ [] = Nothing
directResolve mapping [Access x Nil] = do
    Entry maybeElement _ _ <- Map.lookup x mapping
    fmap (Map.empty, ) maybeElement
directResolve _ [_] = Nothing
directResolve mapping (Access x Nil : rest) = do
    Entry _ "" subMapping <- Map.lookup x mapping
    directResolve subMapping rest
directResolve mapping (Access x e : rest) = do
    Entry _ (index @ (_ : _)) subMapping <- Map.lookup x mapping
    (replacements, element) <- directResolve subMapping rest
    let replacements' = Map.insert index e replacements
    Just (replacements', element)

-- lookup accesses given a current scope prefix
resolveInScope :: Mapping a -> [Tier] -> [Access] -> LookupResult a
resolveInScope mapping [] accesses = do
    (replacements, element) <- directResolve mapping accesses
    Just (accesses, replacements, element)
resolveInScope mapping (Tier x y : rest) accesses = do
    Entry _ _ subMapping <- Map.lookup x mapping
    let deep = resolveInScope subMapping rest accesses
    let side = resolveInScope subMapping [] accesses
    let chosen = if isNothing deep then side else deep
    (accesses', replacements, element) <- chosen
    if null y
        then Just (Access x Nil : accesses', replacements, element)
        else do
            let replacements' = Map.insert y (Ident y) replacements
            Just (Access x (Ident y) : accesses', replacements', element)

type LookupResult a = Maybe ([Access], Replacements, a)

class ScopeKey k where
    lookupElem :: Scopes a -> k -> LookupResult a
    lookupElemM :: Monad m => k -> ScoperT a m (LookupResult a)
    lookupElemM = embedScopes lookupElem

instance ScopeKey Expr where
    lookupElem scopes = join . fmap (lookupAccesses scopes) . exprToAccesses []

instance ScopeKey LHS where
    lookupElem scopes = lookupElem scopes . lhsToExpr

instance ScopeKey Identifier where
    lookupElem scopes ident = lookupAccesses scopes [Access ident Nil]

lookupAccesses :: Scopes a -> [Access] -> LookupResult a
lookupAccesses scopes accesses = do
    let deep = resolveInScope (sMapping scopes) (sCurrent scopes) accesses
    let side = resolveInScope (sMapping scopes) [] accesses
    if isNothing deep then side else deep

lookupLocalIdent :: Scopes a -> Identifier -> LookupResult a
lookupLocalIdent scopes ident = do
    (replacements, element) <- directResolve (sMapping scopes) accesses
    Just (accesses, replacements, element)
    where
        accesses = map toAccess (sCurrent scopes) ++ [Access ident Nil]
        toAccess :: Tier -> Access
        toAccess (Tier x "") = Access x Nil
        toAccess (Tier x y) = Access x (Ident y)

lookupLocalIdentM :: Monad m => Identifier -> ScoperT a m (LookupResult a)
lookupLocalIdentM = embedScopes lookupLocalIdent

withinProcedureM :: Monad m => ScoperT a m Bool
withinProcedureM = gets sProcedure

withinProcedure :: Scopes a -> Bool
withinProcedure = sProcedure

isLoopVar :: Scopes a -> Identifier -> Bool
isLoopVar scopes x = any matches $ sCurrent scopes
    where matches = (== x) . tierIndex

isLoopVarM :: Monad m => Identifier -> ScoperT a m Bool
isLoopVarM = embedScopes isLoopVar

evalScoper
    :: MapperM (Scoper a) Decl
    -> MapperM (Scoper a) ModuleItem
    -> MapperM (Scoper a) GenItem
    -> MapperM (Scoper a) Stmt
    -> Identifier
    -> [ModuleItem]
    -> [ModuleItem]
evalScoper declMapper moduleItemMapper genItemMapper stmtMapper topName items =
    runIdentity $ evalScoperT
    declMapper moduleItemMapper genItemMapper stmtMapper topName items

evalScoperT
    :: forall a m. Monad m
    => MapperM (ScoperT a m) Decl
    -> MapperM (ScoperT a m) ModuleItem
    -> MapperM (ScoperT a m) GenItem
    -> MapperM (ScoperT a m) Stmt
    -> Identifier
    -> [ModuleItem]
    -> m [ModuleItem]
evalScoperT declMapper moduleItemMapper genItemMapper stmtMapper topName items = do
    (items', _) <- runScoperT
        declMapper moduleItemMapper genItemMapper stmtMapper
        topName items
    return items'

runScoper
    :: MapperM (Scoper a) Decl
    -> MapperM (Scoper a) ModuleItem
    -> MapperM (Scoper a) GenItem
    -> MapperM (Scoper a) Stmt
    -> Identifier
    -> [ModuleItem]
    -> ([ModuleItem], Scopes a)
runScoper declMapper moduleItemMapper genItemMapper stmtMapper topName items =
    runIdentity $ runScoperT
    declMapper moduleItemMapper genItemMapper stmtMapper topName items

runScoperT
    :: forall a m. Monad m
    => MapperM (ScoperT a m) Decl
    -> MapperM (ScoperT a m) ModuleItem
    -> MapperM (ScoperT a m) GenItem
    -> MapperM (ScoperT a m) Stmt
    -> Identifier
    -> [ModuleItem]
    -> m ([ModuleItem], Scopes a)
runScoperT declMapper moduleItemMapper genItemMapper stmtMapper topName items =
    runStateT operation initialState
    where
        operation :: ScoperT a m [ModuleItem]
        operation = do
            enterScope topName ""
            mapM wrappedModuleItemMapper items
        initialState = Scopes [] Map.empty False [] []

        wrappedModuleItemMapper = scopeModuleItemT
            declMapper moduleItemMapper genItemMapper stmtMapper

scopeModuleItemT
    :: forall a m. Monad m
    => MapperM (ScoperT a m) Decl
    -> MapperM (ScoperT a m) ModuleItem
    -> MapperM (ScoperT a m) GenItem
    -> MapperM (ScoperT a m) Stmt
    -> ModuleItem
    -> ScoperT a m ModuleItem
scopeModuleItemT declMapper moduleItemMapper genItemMapper stmtMapper =
    wrappedModuleItemMapper
    where
        fullStmtMapper :: Stmt -> ScoperT a m Stmt
        fullStmtMapper (Block kw name decls stmts) = do
            enterScope name ""
            decls' <- fmap concat $ mapM declMapper' decls
            stmts' <- mapM fullStmtMapper stmts
            exitScope
            return $ Block kw name decls' stmts'
        -- TODO: Do we need to support the various procedural loops?
        fullStmtMapper stmt = do
            stmt' <- stmtMapper stmt
            injected <- consumeInjectedDecls
            if null injected
                then traverseSinglyNestedStmtsM fullStmtMapper stmt'
                else fullStmtMapper $ Block Seq "" injected [stmt']

        -- converts a decl and adds decls injected during conversion
        declMapper' :: Decl -> ScoperT a m [Decl]
        declMapper' decl = do
            decl' <- declMapper decl
            injected <- consumeInjectedDecls
            if null injected
                then return [decl']
                else do
                    injected' <- mapM declMapper injected
                    return $ injected' ++ [decl']

        mapTFDecls :: [Decl] -> ScoperT a m [Decl]
        mapTFDecls = mapTFDecls' 0
            where
                mapTFDecls' :: Int -> [Decl] -> ScoperT a m [Decl]
                mapTFDecls' _ [] = return []
                mapTFDecls' idx (decl : decls) =
                    case argIdxDecl decl of
                        Nothing -> do
                            decl' <- declMapper' decl
                            decls' <- mapTFDecls' idx decls
                            return $ decl' ++ decls'
                        Just declFunc -> do
                            _ <- declMapper $ declFunc idx
                            decl' <- declMapper' decl
                            decls' <- mapTFDecls' (idx + 1) decls
                            return $ decl' ++ decls'

                argIdxDecl :: Decl -> Maybe (Int -> Decl)
                argIdxDecl (Variable d t _ a e) =
                    if d == Local
                        then Nothing
                        else Just $ \i -> Variable d t (show i) a e
                argIdxDecl Param{} = Nothing
                argIdxDecl ParamType{} = Nothing
                argIdxDecl CommentDecl{} = Nothing

        redirectTFDecl :: Type -> Identifier -> ScoperT a m (Type, Identifier)
        redirectTFDecl typ ident = do
            res <- declMapper $ Variable Local typ ident [] Nil
            let Variable Local newType newName [] Nil = res
            return (newType, newName)

        wrappedModuleItemMapper :: ModuleItem -> ScoperT a m ModuleItem
        wrappedModuleItemMapper item = do
            item' <- fullModuleItemMapper item
            injected <- consumeInjectedItems
            if null injected
                then return item'
                else do
                    injected' <- mapM fullModuleItemMapper injected
                    return $ Generate $ map GenModuleItem $ injected' ++ [item']
        fullModuleItemMapper :: ModuleItem -> ScoperT a m ModuleItem
        fullModuleItemMapper (MIPackageItem (Function ml t x decls stmts)) = do
            (t', x') <- redirectTFDecl t x
            enterProcedure
            enterScope x' ""
            decls' <- mapTFDecls decls
            stmts' <- mapM fullStmtMapper stmts
            exitScope
            exitProcedure
            return $ MIPackageItem $ Function ml t' x' decls' stmts'
        fullModuleItemMapper (MIPackageItem (Task ml x decls stmts)) = do
            (_, x') <- redirectTFDecl (Implicit Unspecified []) x
            enterProcedure
            enterScope x' ""
            decls' <- mapTFDecls decls
            stmts' <- mapM fullStmtMapper stmts
            exitScope
            exitProcedure
            return $ MIPackageItem $ Task ml x' decls' stmts'
        fullModuleItemMapper (MIPackageItem (Decl decl)) =
            declMapper decl >>= return . MIPackageItem . Decl
        fullModuleItemMapper (AlwaysC kw stmt) = do
            enterProcedure
            stmt' <- fullStmtMapper stmt
            exitProcedure
            return $ AlwaysC kw stmt'
        fullModuleItemMapper (Initial stmt) = do
            enterProcedure
            stmt' <- fullStmtMapper stmt
            exitProcedure
            return $ Initial stmt'
        fullModuleItemMapper (Final stmt) = do
            enterProcedure
            stmt' <- fullStmtMapper stmt
            exitProcedure
            return $ Final stmt'
        fullModuleItemMapper (Generate genItems) =
            mapM fullGenItemMapper genItems >>= return . Generate
        fullModuleItemMapper (MIAttr attr item) =
            fullModuleItemMapper item >>= return . MIAttr attr
        fullModuleItemMapper item = moduleItemMapper item

        -- TODO: This doesn't yet support implicit naming of generate blocks as
        -- blocks as described in Section 27.6.
        fullGenItemMapper :: GenItem -> ScoperT a m GenItem
        fullGenItemMapper genItem = do
            genItem' <- genItemMapper genItem
            injected <- consumeInjectedItems
            if null injected
                then scopeGenItemMapper genItem'
                else do
                    injected' <- mapM fullModuleItemMapper injected
                    genItem'' <- scopeGenItemMapper genItem'
                    let genItems = map GenModuleItem injected' ++ [genItem'']
                    return $ GenBlock "" genItems
        scopeGenItemMapper :: GenItem -> ScoperT a m GenItem
        scopeGenItemMapper (GenFor (index, a) b c genItem) = do
            genItem' <- scopeGenItemBranchMapper index genItem
            return $ GenFor (index, a) b c genItem'
        scopeGenItemMapper (GenIf cond thenItem elseItem) = do
            thenItem' <- scopeGenItemBranchMapper "" thenItem
            elseItem' <- scopeGenItemBranchMapper "" elseItem
            return $ GenIf cond thenItem' elseItem'
        scopeGenItemMapper (GenBlock name genItems) = do
            enterScope name ""
            genItems' <- mapM fullGenItemMapper genItems
            exitScope
            return $ GenBlock name genItems'
        scopeGenItemMapper (GenModuleItem moduleItem) =
            wrappedModuleItemMapper moduleItem >>= return . GenModuleItem
        scopeGenItemMapper genItem =
            traverseSinglyNestedGenItemsM fullGenItemMapper genItem

        scopeGenItemBranchMapper :: Identifier -> GenItem -> ScoperT a m GenItem
        scopeGenItemBranchMapper index (GenBlock name genItems) = do
            enterScope name index
            genItems' <- mapM fullGenItemMapper genItems
            exitScope
            return $ GenBlock name genItems'
        scopeGenItemBranchMapper index genItem = do
            enterScope "" index
            genItem' <- fullGenItemMapper genItem
            exitScope
            return genItem'

partScoper
    :: MapperM (Scoper a) Decl
    -> MapperM (Scoper a) ModuleItem
    -> MapperM (Scoper a) GenItem
    -> MapperM (Scoper a) Stmt
    -> Description
    -> Description
partScoper declMapper moduleItemMapper genItemMapper stmtMapper part =
    runIdentity $ partScoperT
        declMapper moduleItemMapper genItemMapper stmtMapper part

partScoperT
    :: Monad m
    => MapperM (ScoperT a m) Decl
    -> MapperM (ScoperT a m) ModuleItem
    -> MapperM (ScoperT a m) GenItem
    -> MapperM (ScoperT a m) Stmt
    -> Description
    -> m Description
partScoperT declMapper moduleItemMapper genItemMapper stmtMapper =
    mapper
    where
        operation = evalScoperT
            declMapper moduleItemMapper genItemMapper stmtMapper
        mapper (Part attrs extern kw liftetime name ports items) = do
            items' <- operation name items
            return $ Part attrs extern kw liftetime name ports items'
        mapper description = return description
