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
        stmts = map (traverseNestedStmts convertReturn) stmtsOrig
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
        dummyModuleItem = Initial $ Block Seq "" decls stmts
        declares = elem jumpState $ execWriter $
            collectDeclsM collectVarM dummyModuleItem
        uses = elem jumpState $ execWriter $
            collectExprsM (collectNestedExprsM collectExprIdentM) dummyModuleItem
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
removeJumpState (orig @ (Asgn _ _ (LHSIdent ident) _)) =
    if ident == jumpState
        then Null
        else orig
removeJumpState other = other

convertStmts :: [Stmt] -> State Info [Stmt]
convertStmts stmts = do
    res <- convertStmt $ Block Seq "" [] stmts
    let Block Seq "" [] stmts' = res
    return stmts'


pattern SimpleLoopInits :: Type -> Identifier -> Expr
    -> Either [Decl] [(LHS, Expr)]
pattern SimpleLoopInits typ var expr = Left [Variable Local typ var [] expr]

pattern SimpleLoopGuard :: BinOp -> Identifier -> Expr -> Expr
pattern SimpleLoopGuard cmp var bound = BinOp cmp (Ident var) bound

pattern SimpleLoopIncrs :: Identifier -> AsgnOp -> Expr -> [(LHS, AsgnOp, Expr)]
pattern SimpleLoopIncrs var op step = [(LHSIdent var, op, step)]

-- rewrites the given statement, and returns the type of any unfinished jump
convertStmt :: Stmt -> State Info Stmt

convertStmt (Block Par x decls stmts) = do
    -- break, continue, and return disallowed in fork-join
    jumpAllowed <- gets sJumpAllowed
    returnAllowed <- gets sReturnAllowed
    modify $ \s -> s { sJumpAllowed = False, sReturnAllowed = False }
    stmts' <- mapM convertStmt stmts
    modify $ \s -> s { sJumpAllowed = jumpAllowed, sReturnAllowed = returnAllowed }
    return $ Block Par x decls stmts'

convertStmt (Block Seq x decls stmts) = do
    stmts' <- step stmts
    return $ Block Seq x decls $ filter (/= Null) stmts'
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

convertStmt (For
    (inits @ (SimpleLoopInits _ var1 _))
    (comp @ (SimpleLoopGuard _ var2 _))
    (incr @ (SimpleLoopIncrs var3 _ _))
    stmt) =
    if var1 /= var2 || var2 /= var3
        then convertLoop False loop comp stmt
        else convertLoop True  loop comp stmt
    where loop c s = For inits c incr s
convertStmt (For inits comp incr stmt) =
    convertLoop False loop comp stmt
    where loop c s = For inits c incr s
convertStmt (While comp stmt) =
    convertLoop False While comp stmt
convertStmt (DoWhile comp stmt) =
    convertLoop False DoWhile comp stmt

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
convertStmt (Return Nil) = do
    jumpAllowed <- gets sJumpAllowed
    returnAllowed <- gets sReturnAllowed
    assertMsg jumpAllowed "encountered return inside fork-join"
    assertMsg returnAllowed "encountered return outside of task or function"
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

convertStmt (Return{}) = return $
    error "non-void return should have been elaborated already"
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

convertLoop :: Bool -> (Expr -> Stmt -> Stmt) -> Expr -> Stmt -> State Info Stmt
convertLoop local loop comp stmt = do
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

    let keepRunning = BinOp Lt (Ident jumpState) jsBreak
    let comp' = if local then comp else BinOp LogAnd comp keepRunning
    let body = Block Seq "" [] $
                [ asgn jumpState jsNone
                , stmt'
                ]
    let body' = if local then If NoCheck keepRunning body Null else body
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
            loop comp stmt'
        else if origLoopDepth == 0 then
            Block Seq "" []
                [ loop comp' body'
                , jsCheckReturn
                ]
        else
            Block Seq ""
                [ jsStackDecl ]
                [ loop comp' body'
                , jsStackRestore
                ]

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
