{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `always_latch`, `always_comb`, and `always_ff`
 -
 - `always_comb` and `always_latch` become `always @*`, or produce an explicit
 - sensitivity list if they need to pick up sensitivities from the functions
 - they call. `always_ff` simply becomes `always`.
 -
 - TODO: `always_comb` blocks do not run at time zero
 -}

module Convert.AlwaysKW (convert) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Maybe (fromMaybe, mapMaybe)

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions traverseDescription

traverseDescription :: Description -> Description
traverseDescription description@Part{} =
    evalState (evalScoperT $ scopePart op description) mempty
    where op = traverseModuleItem >=> scoper
traverseDescription description = description

type SC = ScoperT Kind (State (Any, [Expr]))

type PortDir = (Identifier, Direction)

data Kind
    = Const Expr
    | Var
    | Proc [Expr] [PortDir]
    deriving Eq

scoper :: ModuleItem -> SC ModuleItem
scoper = scopeModuleItem traverseDecl return traverseGenItem traverseStmt

-- track declarations and visit expressions they contain
traverseDecl :: Decl -> SC Decl
traverseDecl decl = do
    case decl of
        Param    s _ x e   -> do
            -- handle references to local constants
            e' <- if s == Localparam
                    then scopeExpr e
                    else return Nil
            insertElem x $ Const e'
        ParamType  _ x _   -> insertElem x $ Const Nil
        Variable _ _ x _ _ -> do
            -- don't let the second visit of a function or task overwrite the
            -- Proc entry that was just generated
            details <- lookupLocalIdentM x
            case details of
                Just (_, _, Proc{}) -> return ()
                _ -> insertElem x Var
        Net  _ _ _ _ x _ _ -> insertElem x Var
        CommentDecl{} -> return ()
    traverseDeclExprsM traverseExpr decl

-- track expressions and subroutines in a statement
traverseStmt :: Stmt -> SC Stmt
traverseStmt (Subroutine expr args) =
    traverseCall Subroutine expr args
traverseStmt stmt = traverseStmtExprsM traverseExpr stmt

-- visit tasks, functions, and always blocks in generate scopes
traverseGenItem :: GenItem -> SC GenItem
traverseGenItem (GenModuleItem item) =
    traverseModuleItem item >>= return . GenModuleItem
traverseGenItem other = return other

-- identify variables referenced within an expression
traverseExpr :: Expr -> SC Expr
traverseExpr (Call expr args) =
    traverseCall Call expr args
traverseExpr expr = do
    prefix <- embedScopes longestStaticPrefix expr
    case prefix of
        Just expr' -> push (Any False, [expr']) >> return expr
        _ -> traverseSinglyNestedExprsM traverseExpr expr

-- turn a reference to a variable into a canonicalized longest static prefix, if
-- possible, per IEEE 1800-2017 Section 11.5.3
longestStaticPrefix :: Scopes Kind -> Expr -> Maybe Expr
longestStaticPrefix scopes expr@Ident{} =
    asVar scopes expr
longestStaticPrefix scopes (Range expr mode (l, r)) = do
    expr' <- longestStaticPrefix scopes expr
    l' <- asConst scopes l
    r' <- asConst scopes r
    Just $ Range expr' mode (l', r')
longestStaticPrefix scopes (Bit expr idx) = do
    expr' <- longestStaticPrefix scopes expr
    idx' <- asConst scopes idx
    Just $ Bit expr' idx'
longestStaticPrefix scopes orig@(Dot expr field) =
    case asVar scopes orig of
        Just orig' -> Just orig'
        _ -> do
            expr' <- longestStaticPrefix scopes expr
            Just $ Dot expr' field
longestStaticPrefix _ _ =
    Nothing

-- lookup an expression as an outwardly-visible variable
asVar :: Scopes Kind -> Expr -> Maybe Expr
asVar scopes expr = do
    (accesses, _, Var) <- lookupElem scopes expr
    if visible accesses
        then Just $ accessesToExpr accesses
        else Nothing

visible :: [Access] -> Bool
visible = not . elem (Access "" Nil)

-- lookup an expression as a hoist-able constant
asConst :: Scopes Kind -> Expr -> Maybe Expr
asConst scopes expr =
    case runWriter $ asConstRaw scopes expr of
        (expr', Any False) -> Just expr'
        _ -> Nothing

asConstRaw :: Scopes Kind -> Expr -> Writer Any Expr
asConstRaw scopes expr =
    case lookupElem scopes expr of
        Just (accesses@[_, _], _, Const{}) -> return $ accessesToExpr accesses
        Just (_, _, Const Nil) -> recurse
        Just (_, _, Const expr') -> asConstRaw scopes expr'
        Just{} -> tell (Any True) >> return Nil
        Nothing -> recurse
    where
        recurse = traverseSinglyNestedExprsM (asConstRaw scopes) expr

-- special handling for subroutine invocations and function calls
traverseCall :: (Expr -> Args -> a) -> Expr -> Args -> SC a
traverseCall constructor expr args = do
    details <- lookupElemM expr
    expr' <- traverseExpr expr
    args' <- case details of
        Just (_, _, Proc exprs ps) -> do
            when (not $ null exprs) $
                push (Any True, exprs)
            traverseArgs ps args
        _ -> traverseArgs [] args
    return $ constructor expr' args'

-- treats output ports as assignment-like contexts
traverseArgs :: [PortDir] -> Args -> SC Args
traverseArgs ps (Args pnArgs kwArgs) = do
    pnArgs' <- zipWithM usingPN [0..] pnArgs
    kwArgs' <- mapM usingKW kwArgs
    return (Args pnArgs' kwArgs')
    where

        usingPN :: Int -> Expr -> SC Expr
        usingPN key val = do
            if dir == Output
                then return val
                else traverseExpr val
            where dir = if key < length ps
                            then snd $ ps !! key
                            else Input

        usingKW :: (Identifier, Expr) -> SC (Identifier, Expr)
        usingKW (key, val) = do
            val' <- if dir == Output
                        then return val
                        else traverseExpr val
            return (key, val')
            where dir = fromMaybe Input $ lookup key ps

-- append to the non-local expression state
push :: (Any, [Expr]) -> SC ()
push x = lift $ modify' (x <>)

-- custom traversal which converts SystemVerilog `always` keywords and tracks
-- information about task and functions
traverseModuleItem :: ModuleItem -> SC ModuleItem
traverseModuleItem (AlwaysC AlwaysLatch stmt) = do
    e <- fmap toEvent $ findNonLocals $ Initial stmt
    return $ AlwaysC Always $ Timing (Event e) stmt
traverseModuleItem (AlwaysC AlwaysComb stmt) = do
    e <- fmap toEvent $ findNonLocals $ Initial stmt
    return $ AlwaysC Always $ Timing (Event e) stmt
traverseModuleItem (AlwaysC AlwaysFF stmt) =
    return $ AlwaysC Always stmt
traverseModuleItem item@(MIPackageItem (Function _ _ x decls _)) = do
    (_, s) <- findNonLocals item
    insertElem x $ Proc s (ports decls)
    return item
traverseModuleItem item@(MIPackageItem (Task _ x decls _)) = do
    insertElem x $ Proc [] (ports decls)
    return item
traverseModuleItem other = return other

toEvent :: (Bool, [Expr]) -> Event
toEvent (False, _) = EventStar
toEvent (True, exprs) =
    EventExpr $ foldl1 EventExprOr $ map (EventExprEdge NoEdge) exprs

-- turn a list of port declarations into a port direction map
ports :: [Decl] -> [PortDir]
ports = filter ((/= Local) . snd) . map port

port :: Decl -> PortDir
port (Variable d _ x _ _) = (x, d)
port (Net  d _ _ _ x _ _) = (x, d)
port _ = ("", Local)

-- get a list of non-local variables referenced within a module item, and
-- whether or not this module item references any functions which themselves
-- reference non-local variables
findNonLocals :: ModuleItem -> SC (Bool, [Expr])
findNonLocals item = do
    scopes <- get
    lift $ put mempty
    _ <- scoper item
    (anys, exprs) <- lift get
    let nonLocals = mapMaybe (longestStaticPrefix scopes) exprs
    return (getAny anys, nonLocals)
