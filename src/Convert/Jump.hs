{-# LANGUAGE PatternSynonyms #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `return`, `break`, and `continue`
 -
 - Because Verilog-2005 has no jumping statements, this conversion ends up
 - producing significantly more verbose code to achieve the same control flow.
 -}

module Convert.Jump (convert) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Convert.Traverse
import Language.SystemVerilog.AST

data Info = Info
    { sLoopDepth :: Int
    , sHasJump :: Bool
    , sReturnAllowed :: Bool
    , sJumpAllowed :: Bool
    }

initialState :: Info
initialState = Info
    { sLoopDepth = 0
    , sHasJump = False
    , sReturnAllowed = False
    , sJumpAllowed = True
    }

initialStateTF :: Info
initialStateTF = initialState { sReturnAllowed = True }

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems convertModuleItem

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem (MIPackageItem (Function ml t f decls stmtsOrig)) =
    MIPackageItem $ Function ml t f decls' stmts''
    where
        stmts = if t == Void
                then stmtsOrig
                else map (traverseNestedStmts convertReturn) stmtsOrig
        convertReturn :: Stmt -> Stmt
        convertReturn (Return Nil) = Return Nil
        convertReturn (Return e) =
            Block Seq "" []
            [ asgn f e
            , Return Nil
            ]
        convertReturn other = other
        stmts' = evalState (convertStmts stmts) initialStateTF
        (decls', stmts'') = addJumpStateDeclTF decls stmts'
convertModuleItem (MIPackageItem (Task ml f decls stmts)) =
    MIPackageItem $ Task ml f decls' stmts''
    where
        stmts' = evalState (convertStmts stmts) initialStateTF
        (decls', stmts'') = addJumpStateDeclTF decls stmts'
convertModuleItem (Initial    stmt) = convertMIStmt Initial      stmt
convertModuleItem (Final      stmt) = convertMIStmt Final        stmt
convertModuleItem (AlwaysC kw stmt) = convertMIStmt (AlwaysC kw) stmt
convertModuleItem other = other

convertMIStmt :: (Stmt -> ModuleItem) -> Stmt -> ModuleItem
convertMIStmt constructor (Timing c stmt) =
    convertMIStmt (constructor . Timing c) stmt
convertMIStmt constructor stmt =
    constructor stmt''
    where
        stmt' = evalState (convertStmt stmt) initialState
        stmt'' = addJumpStateDeclStmt stmt'

-- adds a declaration of the jump state variable if it is needed; if the jump
-- state is not used at all, then it is removed from the given statements
-- entirely
addJumpStateDeclTF :: [Decl] -> [Stmt] -> ([Decl], [Stmt])
addJumpStateDeclTF decls stmts =
    if uses && not declares then
        ( decls ++
            [Variable Local jumpStateType jumpState [] jsNone]
        , stmts )
    else if uses then
        (decls, stmts)
    else
        (decls, map (traverseNestedStmts removeJumpState) stmts)
    where
        dummyStmt = Block Seq "" decls stmts
        writesJumpState f = elem jumpState $ execWriter $
            collectNestedStmtsM f dummyStmt
        declares = writesJumpState $ collectStmtDeclsM collectVarM
        uses = writesJumpState $
            collectStmtExprsM $ collectNestedExprsM collectExprIdentM
        collectVarM :: Decl -> Writer [String] ()
        collectVarM (Variable Local _ ident _ _) = tell [ident]
        collectVarM _ = return ()
        collectExprIdentM :: Expr -> Writer [String] ()
        collectExprIdentM (Ident ident) = tell [ident]
        collectExprIdentM _ = return ()
addJumpStateDeclStmt :: Stmt -> Stmt
addJumpStateDeclStmt stmt =
    if null decls
        then stmt'
        else Block Seq "" decls [stmt']
    where (decls, [stmt']) = addJumpStateDeclTF [] [stmt]

removeJumpState :: Stmt -> Stmt
removeJumpState orig@(Asgn _ _ (LHSIdent ident) _) =
    if ident == jumpState
        then Null
        else orig
removeJumpState other = other

convertStmts :: [Stmt] -> State Info [Stmt]
convertStmts stmts = do
    res <- convertStmt $ Block Seq "" [] stmts
    let Block Seq "" [] stmts' = res
    return stmts'


pattern SimpleLoopInits :: Identifier -> [(LHS, Expr)]
pattern SimpleLoopInits var <- [(LHSIdent var, _)]

pattern SimpleLoopIncrs :: Identifier -> [(LHS, AsgnOp, Expr)]
pattern SimpleLoopIncrs var <- [(LHSIdent var, _, _)]

-- check if an expression contains a reference to the given identifier
usesIdent :: Identifier -> Expr -> Writer Any ()
usesIdent x (Ident y)
    | x == y = tell $ Any True
    | otherwise = return ()
usesIdent x expr =
    collectSinglyNestedExprsM (usesIdent x) expr

-- identifies loops which could likely be statically unrolled, and so may
-- benefit from avoiding complicating the loop guard with jump state, assuming
-- the guard and incrementation do not have side effects
simpleLoopVar :: [(LHS, Expr)] -> Expr -> [(LHS, AsgnOp, Expr)] -> Identifier
simpleLoopVar (SimpleLoopInits var1) comp (SimpleLoopIncrs var3)
    | var1 == var3, getAny $ execWriter $ usesIdent var1 comp = var1
simpleLoopVar _ _ _ = ""

-- rewrites the given statement, and returns the type of any unfinished jump
convertStmt :: Stmt -> State Info Stmt

convertStmt (Block Par x decls stmts) = do
    -- break, continue, and return disallowed in fork-join
    jumpAllowed <- gets sJumpAllowed
    modify $ \s -> s { sJumpAllowed = False }
    stmts' <- mapM convertStmt stmts
    modify $ \s -> s { sJumpAllowed = jumpAllowed }
    return $ Block Par x decls stmts'

convertStmt (Block Seq ""
    decls@[CommentDecl{}, Variable Local _ var0 [] Nil]
    [comment@CommentStmt{}, For inits comp incr stmt])
    | var1@(_ : _) <- simpleLoopVar inits comp incr, var0 == var1 =
    convertLoop (Just var1) loop comp incr stmt
        >>= return . Block Seq "" decls . (comment :) . pure
    where
        loop c i s = For inits c i s

convertStmt (Block Seq x decls stmts) =
    step stmts >>= return . Block Seq x decls
    where
        step :: [Stmt] -> State Info [Stmt]
        step [] = return []
        step (s : ss) = do
            hasJump <- gets sHasJump
            loopDepth <- gets sLoopDepth
            modify $ \st -> st { sHasJump = False }
            s' <- convertStmt s
            currHasJump <- gets sHasJump
            currLoopDepth <- gets sLoopDepth
            assertMsg (loopDepth == currLoopDepth) "loop depth invariant failed"
            modify $ \st -> st { sHasJump = hasJump || currHasJump }
            ss' <- step ss
            if currHasJump && not (null ss)
            then do
                let comp = BinOp Eq (Ident jumpState) jsNone
                let stmt = Block Seq "" [] ss'
                return [s', If NoCheck comp stmt Null]
            else do
                return $ s' : ss'

convertStmt (If unique expr thenStmt elseStmt) = do
    (thenStmt', thenHasJump) <- convertSubStmt thenStmt
    (elseStmt', elseHasJump) <- convertSubStmt elseStmt
    modify $ \s -> s { sHasJump = thenHasJump || elseHasJump }
    return $ If unique expr thenStmt' elseStmt'

convertStmt (Case unique kw expr cases) = do
    results <- mapM convertSubStmt $ map snd cases
    let (stmts', hasJumps) = unzip results
    let cases' = zip (map fst cases) stmts'
    let hasJump = foldl (||) False hasJumps
    modify $ \s -> s { sHasJump = hasJump }
    return $ Case unique kw expr cases'

convertStmt (For inits comp incr stmt)
    | var@(_ : _) <- simpleLoopVar inits comp incr =
    convertLoop (Just var) loop comp incr stmt
    where
        loop c i s = For inits c i s
convertStmt (For inits comp incr stmt) =
    convertLoop Nothing loop comp incr stmt
    where loop c i s = For inits c i s
convertStmt (While comp stmt) =
    convertLoop Nothing loop comp [] stmt
    where loop c _ s = While c s

convertStmt (Continue) = do
    loopDepth <- gets sLoopDepth
    jumpAllowed <- gets sJumpAllowed
    assertMsg (loopDepth > 0) "encountered continue outside of loop"
    assertMsg jumpAllowed "encountered continue inside fork-join"
    modify $ \s -> s { sHasJump = True }
    return $ asgn jumpState jsContinue
convertStmt (Break) = do
    loopDepth <- gets sLoopDepth
    jumpAllowed <- gets sJumpAllowed
    assertMsg (loopDepth > 0) "encountered break outside of loop"
    assertMsg jumpAllowed "encountered break inside fork-join"
    modify $ \s -> s { sHasJump = True }
    return $ asgn jumpState jsBreak
convertStmt (Return e) = do
    jumpAllowed <- gets sJumpAllowed
    returnAllowed <- gets sReturnAllowed
    assertMsg jumpAllowed "encountered return inside fork-join"
    assertMsg returnAllowed "encountered return outside of task or function"
    assertMsg (e == Nil) "non-void return inside task or void function"
    modify $ \s -> s { sHasJump = True }
    return $ asgn jumpState jsReturn

convertStmt (RepeatL expr stmt) = do
    loopDepth <- gets sLoopDepth
    modify $ \s -> s { sLoopDepth = loopDepth + 1 }
    stmt' <- convertStmt stmt
    hasJump <- gets sHasJump
    assertMsg (not hasJump) "jumps not supported within repeat loops"
    modify $ \s -> s { sLoopDepth = loopDepth }
    return $ RepeatL expr stmt'
convertStmt (Forever stmt) = do
    loopDepth <- gets sLoopDepth
    modify $ \s -> s { sLoopDepth = loopDepth + 1 }
    stmt' <- convertStmt stmt
    hasJump <- gets sHasJump
    assertMsg (not hasJump) "jumps not supported within forever loops"
    modify $ \s -> s { sLoopDepth = loopDepth }
    return $ Forever stmt'

convertStmt (Timing timing stmt) =
    convertStmt stmt >>= return . Timing timing
convertStmt (StmtAttr attr stmt) =
    convertStmt stmt >>= return . StmtAttr attr

convertStmt (Foreach{}) = return $
    error "foreach should have been elaborated already"

convertStmt other = return other

-- convert a statement on its own without changing the state, but returning
-- whether or not the statement contains a jump; used to reconcile across
-- branching statements
convertSubStmt :: Stmt -> State Info (Stmt, Bool)
convertSubStmt stmt = do
    origState <- get
    stmt' <- convertStmt stmt
    hasJump <- gets sHasJump
    put origState
    return (stmt', hasJump)

type Incr = (LHS, AsgnOp, Expr)

convertLoop :: Maybe Identifier -> (Expr -> [Incr] -> Stmt -> Stmt) -> Expr
    -> [Incr] -> Stmt -> State Info Stmt
convertLoop localInfo loop comp incr stmt = do
    -- save the loop state and increment loop depth
    Info { sLoopDepth = origLoopDepth, sHasJump = origHasJump } <- get
    assertMsg (not origHasJump) "has jump invariant failed"
    modify $ \s -> s { sLoopDepth = origLoopDepth + 1 }
    -- convert the loop body
    stmt' <- convertStmt stmt
    -- restore the loop state
    Info { sLoopDepth = afterLoopDepth, sHasJump = afterHasJump } <- get
    assertMsg (origLoopDepth + 1 == afterLoopDepth) "loop depth invariant failed"
    modify $ \s -> s { sLoopDepth = origLoopDepth }

    let useBreakVar = local && not (null localVar)
    let breakVarDeclRaw = Variable Local (TypeOf $ Ident localVar) breakVar [] Nil
    let breakVarDecl = if useBreakVar then breakVarDeclRaw else CommentDecl "no-op"
    let updateBreakVar = if useBreakVar then asgn breakVar $ Ident localVar else Null

    let keepRunning = BinOp Lt (Ident jumpState) jsBreak
    let pushBreakVar = if useBreakVar
                        then If NoCheck (UniOp LogNot keepRunning)
                                (asgn localVar $ Ident breakVar) Null
                        else Null
    let comp' = if local then comp else BinOp LogAnd comp keepRunning
    let incr' = if local then incr else map (stubIncr keepRunning) incr
    let body = Block Seq "" [] $
                [ asgn jumpState jsNone
                , stmt'
                ]
    let body' = if local
                    then If NoCheck keepRunning
                            (Block Seq "" [] [body, updateBreakVar]) Null
                    else body
    let jsStackIdent = jumpState ++ "_" ++ show origLoopDepth
    let jsStackDecl = Variable Local jumpStateType jsStackIdent []
                        (Ident jumpState)
    let jsStackRestore = If NoCheck
                (BinOp Ne (Ident jumpState) jsReturn)
                (asgn jumpState (Ident jsStackIdent))
                Null
    let jsCheckReturn = If NoCheck
                (BinOp Ne (Ident jumpState) jsReturn)
                (asgn jumpState jsNone)
                Null

    return $
        if not afterHasJump then
            loop comp incr stmt'
        else if origLoopDepth == 0 then
            Block Seq "" [ breakVarDecl ]
                [ loop comp' incr' body'
                , pushBreakVar
                , jsCheckReturn
                ]
        else
            Block Seq ""
                [ breakVarDecl, jsStackDecl ]
                [ loop comp' incr' body'
                , pushBreakVar
                , jsStackRestore
                ]

    where
        breakVar = "_sv2v_value_on_break"
        local = localInfo /= Nothing
        Just localVar = localInfo

stubIncr :: Expr -> Incr -> Incr
stubIncr keepRunning (lhs, AsgnOpEq, expr) =
    (lhs, AsgnOpEq, expr')
    where expr' = Mux keepRunning expr (lhsToExpr lhs)
stubIncr keepRunning (lhs, op, expr) =
    stubIncr keepRunning (lhs, AsgnOpEq, expr')
    where
        AsgnOp binop = op
        expr' = BinOp binop (lhsToExpr lhs) expr

jumpStateType :: Type
jumpStateType = IntegerVector TBit Unspecified [(RawNum 0, RawNum 1)]

jumpState :: String
jumpState = "_sv2v_jump"

jsVal :: Integer -> Expr
jsVal n = Number $ Based 2 False Binary n 0

-- keep running the loop/function normally
jsNone :: Expr
jsNone = jsVal 0
-- skip to the next iteration of the loop (continue)
jsContinue :: Expr
jsContinue = jsVal 1
-- stop running the loop immediately (break)
jsBreak :: Expr
jsBreak = jsVal 2
-- stop running the function immediately (return)
jsReturn :: Expr
jsReturn = jsVal 3


assertMsg :: Bool -> String -> State Info ()
assertMsg True _ = return ()
assertMsg False msg = error msg

asgn :: Identifier -> Expr -> Stmt
asgn x e = Asgn AsgnOpEq Nothing (LHSIdent x) e
