{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `return`, `break`, and `continue`
 -
 - Because Verilog-2005 has no jumping statements, this conversion ends up
 - producing significantly more verbose code to acheive the same control flow.
 -}

module Convert.Jump (convert) where

import Control.Monad.State

import Convert.Traverse
import Language.SystemVerilog.AST

data JumpType
    = JTNone
    | JTContinue
    | JTBreak
    | JTReturn
    deriving (Eq, Ord, Show)

data Info = Info
    { sJumpType :: JumpType
    , sLoopID :: Identifier
    }

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions $ traverseModuleItems convertModuleItem

convertModuleItem :: ModuleItem -> ModuleItem
convertModuleItem (MIPackageItem (Function ml t f decls stmtsOrig)) =
    if sJumpType finalState == JTNone || sJumpType finalState == JTReturn
        then MIPackageItem $ Function ml t f decls stmts'
        else error "illegal jump statement within task"
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
        initialState = Info { sJumpType = JTNone, sLoopID = "" }
        (stmts', finalState) = runState (convertStmts stmts) initialState
convertModuleItem (MIPackageItem (Task ml f decls stmts)) =
    if sJumpType finalState == JTNone || sJumpType finalState == JTReturn
        then MIPackageItem $ Task ml f decls $ stmts'
        else error "illegal jump statement within task"
    where
        initialState = Info { sJumpType = JTNone, sLoopID = "" }
        (stmts', finalState) = runState (convertStmts stmts) initialState
convertModuleItem (Initial stmt) =
    if sJumpType finalState == JTNone
        then Initial stmt'
        else error "illegal jump statement within initial construct"
    where
        initialState = Info { sJumpType = JTNone, sLoopID = "" }
        (stmt', finalState) = runState (convertStmt stmt) initialState
convertModuleItem (Final stmt) =
    if sJumpType finalState == JTNone
        then Final stmt'
        else error "illegal jump statement within final construct"
    where
        initialState = Info { sJumpType = JTNone, sLoopID = "" }
        (stmt', finalState) = runState (convertStmt stmt) initialState
convertModuleItem (AlwaysC kw stmt) =
    if sJumpType finalState == JTNone
        then AlwaysC kw stmt'
        else error "illegal jump statement within always construct"
    where
        initialState = Info { sJumpType = JTNone, sLoopID = "" }
        (stmt', finalState) = runState (convertStmt stmt) initialState
convertModuleItem other = other

convertStmts :: [Stmt] -> State Info [Stmt]
convertStmts stmts = do
    res <- convertStmt $ Block Seq "" [] stmts
    let Block Seq "" [] stmts' = res
    return stmts'


-- rewrites the given statement, and returns the type of any unfinished jump
convertStmt :: Stmt -> State Info Stmt

convertStmt (Block Par x decls stmts) = do
    -- break, continue, and return disallowed in fork-join
    modify $ \s -> s { sLoopID = "" }
    loopID <- gets sLoopID
    stmts' <- mapM convertStmt stmts
    modify $ \s -> s { sLoopID = loopID }
    return $ Block Par x decls stmts'

convertStmt (Block Seq x decls stmts) = do
    stmts' <- step stmts
    return $ Block Seq x decls $ filter (/= Null) stmts'
    where
        step :: [Stmt] -> State Info [Stmt]
        step [] = return []
        step (s : ss) = do
            jt <- gets sJumpType
            loopID <- gets sLoopID
            if jt == JTNone then do
                s' <- convertStmt s
                jt' <- gets sJumpType
                if jt' == JTNone || not (isBranch s) || null loopID then do
                    ss' <- step ss
                    return $ s' : ss'
                else do
                    modify $ \t -> t { sJumpType = JTNone }
                    ss' <- step ss
                    let comp = BinOp Eq (Ident loopID) runLoop
                    let stmt = Block Seq "" [] ss'
                    modify $ \t -> t { sJumpType = jt' }
                    return [s', If Nothing comp stmt Null]
            else do
                return [Null]
        isBranch :: Stmt -> Bool
        isBranch (If{}) = True
        isBranch (Case{}) = True
        isBranch _ = False

convertStmt (If unique expr thenStmt elseStmt) = do
    (thenStmt', thenJT) <- convertSubStmt thenStmt
    (elseStmt', elseJT) <- convertSubStmt elseStmt
    let newJT = max thenJT elseJT
    modify $ \s -> s { sJumpType = newJT }
    return $ If unique expr thenStmt' elseStmt'

convertStmt (Case unique kw expr cases mdef) = do
    (mdef', mdefJT) <-
        case mdef of
            Nothing -> return (Nothing, JTNone)
            Just stmt -> do
                (stmt', jt) <- convertSubStmt stmt
                return (Just stmt', jt)
    results <- mapM convertSubStmt $ map snd cases
    let (stmts', jts) = unzip results
    let cases' = zip (map fst cases) stmts'
    let newJT = foldl max mdefJT jts
    modify $ \s -> s { sJumpType = newJT }
    return $ Case unique kw expr cases' mdef'

convertStmt (For inits comp incr stmt) =
    convertLoop loop comp stmt
    where loop c s = For inits c incr s
convertStmt (While comp stmt) =
    convertLoop While comp stmt
convertStmt (DoWhile comp stmt) =
    convertLoop DoWhile comp stmt

convertStmt (Continue) = do
    loopID <- gets sLoopID
    modify $ \s -> s { sJumpType = JTContinue }
    assertMsg (not $ null loopID) "encountered continue outside of loop"
    return $ asgn loopID continueLoop
convertStmt (Break) = do
    loopID <- gets sLoopID
    modify $ \s -> s { sJumpType = JTBreak }
    assertMsg (not $ null loopID) "encountered break outside of loop"
    return $ asgn loopID exitLoop
convertStmt (Return Nil) = do
    loopID <- gets sLoopID
    modify $ \s -> s { sJumpType = JTReturn }
    if null loopID
        then return Null
        else return $ asgn loopID exitLoop

convertStmt (RepeatL expr stmt) = do
    modify $ \s -> s { sLoopID = "repeat" }
    stmt' <- convertStmt stmt
    jt <- gets sJumpType
    assertMsg (jt == JTNone) "jumps not supported within repeat loops"
    return $ RepeatL expr stmt'
convertStmt (Forever stmt) = do
    modify $ \s -> s { sLoopID = "forever" }
    stmt' <- convertStmt stmt
    jt <- gets sJumpType
    assertMsg (jt == JTNone) "jumps not supported within forever loops"
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


-- convert a statement on its own without changing the state, but returning the
-- resulting jump type; used to reconcile across branching statements
convertSubStmt :: Stmt -> State Info (Stmt, JumpType)
convertSubStmt stmt = do
    origState <- get
    stmt' <- convertStmt stmt
    jt <- gets sJumpType
    put origState
    if sJumpType origState == JTNone
        then return (stmt', jt)
        else error $ "convertStmt invariant failed on: " ++ show stmt

convertLoop :: (Expr -> Stmt -> Stmt) -> Expr -> Stmt -> State Info Stmt
convertLoop loop comp stmt = do
    Info { sJumpType = origJT, sLoopID = origLoopID } <- get
    let loopID = (++) "_sv2v_loop_" $ shortHash $ loop comp stmt
    modify $ \s -> s { sLoopID = loopID }
    stmt' <- convertStmt stmt
    jt <- gets sJumpType
    let afterJT = if jt == JTReturn then jt else origJT
    put $ Info { sJumpType = afterJT, sLoopID = origLoopID }
    let comp' = BinOp LogAnd (BinOp Ne (Ident loopID) exitLoop) comp
    return $ if jt == JTNone
        then loop comp stmt'
        else Block Seq ""
            [ Variable Local loopStateType loopID [] (Just runLoop)
            ]
            [ loop comp' $ Block Seq "" []
                [ asgn loopID runLoop
                , stmt'
                ]
            , if afterJT == JTReturn && origLoopID /= ""
                then asgn origLoopID exitLoop
                else Null
            ]
    where loopStateType = IntegerVector TBit Unspecified [(Number "0", Number "1")]


-- stop running the loop immediately (break or return)
exitLoop :: Expr
exitLoop = Number "0"
-- keep running the loop normally
runLoop :: Expr
runLoop = Number "1"
-- skip to the next iteration of the loop (continue)
continueLoop :: Expr
continueLoop = Number "2"


assertMsg :: Bool -> String -> State Info ()
assertMsg True _ = return ()
assertMsg False msg = error msg

asgn :: Identifier -> Expr -> Stmt
asgn x e = AsgnBlk AsgnOpEq (LHSIdent x) e
