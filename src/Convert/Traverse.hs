{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Utilities for traversing AST transformations.
 -}

module Convert.Traverse
( MapperM
, Mapper
, unmonad
, collectify
, traverseDescriptionsM
, traverseDescriptions
, collectDescriptionsM
, traverseModuleItemsM
, traverseModuleItems
, collectModuleItemsM
, traverseStmtsM
, traverseStmts
, collectStmtsM
, traverseStmtLHSsM
, traverseStmtLHSs
, collectStmtLHSsM
, traverseExprsM
, traverseExprs
, collectExprsM
, traverseLHSsM
, traverseLHSs
, collectLHSsM
, traverseDeclsM
, traverseDecls
, collectDeclsM
, traverseTypesM
, traverseTypes
, collectTypesM
, traverseGenItemsM
, traverseGenItems
, collectGenItemsM
, traverseAsgnsM
, traverseAsgns
, collectAsgnsM
, traverseNestedModuleItemsM
, traverseNestedModuleItems
, collectNestedModuleItemsM
, traverseNestedStmts
, traverseNestedExprs
, collectNestedExprsM
, traverseNestedLHSsM
, traverseNestedLHSs
, collectNestedLHSsM
) where

import Control.Monad.State
import Language.SystemVerilog.AST

type MapperM m t = t -> m t
type Mapper t = t -> t
type CollectorM m t = t -> m ()

unmonad :: (MapperM (State ()) a -> MapperM (State ()) b) -> Mapper a -> Mapper b
unmonad traverser mapper thing =
    evalState (traverser (return . mapper) thing) ()

collectify :: Monad m => (MapperM m a -> MapperM m b) -> CollectorM m a -> CollectorM m b
collectify traverser collector thing =
    traverser mapper thing >>= \_ -> return ()
    where mapper x = collector x >>= \() -> return x

traverseDescriptionsM :: Monad m => MapperM m Description -> MapperM m AST
traverseDescriptionsM mapper descriptions =
    mapM mapper descriptions

traverseDescriptions :: Mapper Description -> Mapper AST
traverseDescriptions = unmonad traverseDescriptionsM
collectDescriptionsM :: Monad m => CollectorM m Description -> CollectorM m AST
collectDescriptionsM = collectify traverseDescriptionsM

maybeDo :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeDo _ Nothing = return Nothing
maybeDo fun (Just val) = fun val >>= return . Just

traverseModuleItemsM :: Monad m => MapperM m ModuleItem -> MapperM m Description
traverseModuleItemsM mapper (Part kw name ports items) =
    mapM fullMapper items >>= return . Part kw name ports
    where
        fullMapper (Generate genItems) =
            mapM fullGenItemMapper genItems >>= mapper . Generate
        fullMapper other = mapper other
        fullGenItemMapper = traverseNestedGenItemsM genItemMapper
        genItemMapper (GenModuleItem moduleItem) = do
            moduleItem' <- fullMapper moduleItem
            return $ case moduleItem' of
                Generate subItems -> GenBlock Nothing subItems
                _ -> GenModuleItem moduleItem'
        genItemMapper other = return other
traverseModuleItemsM mapper (PackageItem packageItem) = do
    let item = MIPackageItem packageItem
    Part Module "DNE" [] [item'] <-
        traverseModuleItemsM mapper (Part Module "DNE" [] [item])
    return $ case item' of
        MIPackageItem packageItem' -> PackageItem packageItem'
        other -> error $ "encountered bad package module item: " ++ show other
traverseModuleItemsM _ (Directive str) = return $ Directive str

traverseModuleItems :: Mapper ModuleItem -> Mapper Description
traverseModuleItems = unmonad traverseModuleItemsM
collectModuleItemsM :: Monad m => CollectorM m ModuleItem -> CollectorM m Description
collectModuleItemsM = collectify traverseModuleItemsM

traverseStmtsM :: Monad m => MapperM m Stmt -> MapperM m ModuleItem
traverseStmtsM mapper = moduleItemMapper
    where
        moduleItemMapper (AlwaysC kw stmt) =
            fullMapper stmt >>= return . AlwaysC kw
        moduleItemMapper (MIPackageItem (Function lifetime ret name decls stmts)) = do
            stmts' <- mapM fullMapper stmts
            return $ MIPackageItem $ Function lifetime ret name decls stmts'
        moduleItemMapper (MIPackageItem (Task lifetime name decls stmts)) = do
            stmts' <- mapM fullMapper stmts
            return $ MIPackageItem $ Task lifetime name decls stmts'
        moduleItemMapper (Initial stmt) =
            fullMapper stmt >>= return . Initial
        moduleItemMapper other = return $ other
        fullMapper = traverseNestedStmtsM mapper

traverseStmts :: Mapper Stmt -> Mapper ModuleItem
traverseStmts = unmonad traverseStmtsM
collectStmtsM :: Monad m => CollectorM m Stmt -> CollectorM m ModuleItem
collectStmtsM = collectify traverseStmtsM

-- private utility for turning a thing which maps over a single lever of
-- statements into one that maps over the nested statements first, then the
-- higher levels up
traverseNestedStmtsM :: Monad m => MapperM m Stmt -> MapperM m Stmt
traverseNestedStmtsM mapper = fullMapper
    where
        fullMapper stmt = mapper stmt >>= cs
        cs (Block name decls stmts) =
            mapM fullMapper stmts >>= return . Block name decls
        cs (Case u kw expr cases def) = do
            caseStmts <- mapM fullMapper $ map snd cases
            let cases' = zip (map fst cases) caseStmts
            def' <- maybeDo fullMapper def
            return $ Case u kw expr cases' def'
        cs (AsgnBlk op lhs expr) = return $ AsgnBlk op lhs expr
        cs (Asgn       lhs expr) = return $ Asgn       lhs expr
        cs (For a b c stmt) = fullMapper stmt >>= return . For a b c
        cs (While   e stmt) = fullMapper stmt >>= return . While   e
        cs (RepeatL e stmt) = fullMapper stmt >>= return . RepeatL e
        cs (DoWhile e stmt) = fullMapper stmt >>= return . DoWhile e
        cs (Forever   stmt) = fullMapper stmt >>= return . Forever
        cs (If e s1 s2) = do
            s1' <- fullMapper s1
            s2' <- fullMapper s2
            return $ If e s1' s2'
        cs (Timing event stmt) = fullMapper stmt >>= return . Timing event
        cs (Return expr) = return $ Return expr
        cs (Subroutine f exprs) = return $ Subroutine f exprs
        cs (Null) = return Null

traverseStmtLHSsM :: Monad m => MapperM m LHS -> MapperM m Stmt
traverseStmtLHSsM mapper = traverseNestedStmtsM stmtMapper
    where
        fullMapper = mapper
        stmtMapper (Timing (Event sense) stmt) = do
            sense' <- senseMapper sense
            return $ Timing (Event sense') stmt
        stmtMapper (AsgnBlk op lhs expr) = fullMapper lhs >>= \lhs' -> return $ AsgnBlk op lhs' expr
        stmtMapper (Asgn       lhs expr) = fullMapper lhs >>= \lhs' -> return $ Asgn       lhs' expr
        stmtMapper other = return other
        senseMapper (Sense        lhs) = fullMapper lhs >>= return . Sense
        senseMapper (SensePosedge lhs) = fullMapper lhs >>= return . SensePosedge
        senseMapper (SenseNegedge lhs) = fullMapper lhs >>= return . SenseNegedge
        senseMapper (SenseOr    s1 s2) = do
            s1' <- senseMapper s1
            s2' <- senseMapper s2
            return $ SenseOr s1' s2'
        senseMapper (SenseStar       ) = return SenseStar

traverseStmtLHSs :: Mapper LHS -> Mapper Stmt
traverseStmtLHSs = unmonad traverseStmtLHSsM
collectStmtLHSsM :: Monad m => CollectorM m LHS -> CollectorM m Stmt
collectStmtLHSsM = collectify traverseStmtLHSsM

traverseNestedExprsM :: Monad m => MapperM m Expr -> MapperM m Expr
traverseNestedExprsM mapper = exprMapper
    where
        exprMapper e = mapper e >>= em
        maybeExprMapper Nothing = return Nothing
        maybeExprMapper (Just e) =
            exprMapper e >>= return . Just
        em (String     s) = return $ String    s
        em (Number     s) = return $ Number    s
        em (ConstBool  b) = return $ ConstBool b
        em (Ident      i) = return $ Ident     i
        em (Range e (e1, e2)) = do
            e' <- exprMapper e
            e1' <- exprMapper e1
            e2' <- exprMapper e2
            return $ Range e' (e1', e2')
        em (Bit   e1 e2) = do
            e1' <- exprMapper e1
            e2' <- exprMapper e2
            return $ Bit e1' e2'
        em (Repeat     e l) = do
            e' <- exprMapper e
            l' <- mapM exprMapper l
            return $ Repeat e' l'
        em (Concat     l) =
            mapM exprMapper l >>= return . Concat
        em (Call       f l) =
            mapM maybeExprMapper l >>= return . Call f
        em (UniOp      o e) =
            exprMapper e >>= return . UniOp o
        em (BinOp      o e1 e2) = do
            e1' <- exprMapper e1
            e2' <- exprMapper e2
            return $ BinOp o e1' e2'
        em (Mux        e1 e2 e3) = do
            e1' <- exprMapper e1
            e2' <- exprMapper e2
            e3' <- exprMapper e3
            return $ Mux e1' e2' e3'
        em (Cast       t e) =
            exprMapper e >>= return . Cast t
        em (Access e x) =
            exprMapper e >>= \e' -> return $ Access e' x
        em (Pattern l) = do
            let names = map fst l
            exprs <- mapM exprMapper $ map snd l
            return $ Pattern $ zip names exprs


traverseExprsM :: Monad m => MapperM m Expr -> MapperM m ModuleItem
traverseExprsM mapper = moduleItemMapper
    where

    rangeMapper (a, b) = do
        a' <- exprMapper a
        b' <- exprMapper b
        return (a', b')

    maybeExprMapper Nothing = return Nothing
    maybeExprMapper (Just e) =
        exprMapper e >>= return . Just

    declMapper (Parameter  t x e) =
        exprMapper e >>= return . Parameter  t x
    declMapper (Localparam t x e) =
        exprMapper e >>= return . Localparam t x
    declMapper (Variable d t x a me) = do
        a' <- mapM rangeMapper a
        me' <- maybeExprMapper me
        return $ Variable d t x a' me'

    exprMapper = mapper

    caseMapper (exprs, stmt) = do
        exprs' <- mapM exprMapper exprs
        return (exprs', stmt)
    stmtMapper = traverseNestedStmtsM flatStmtMapper
    flatStmtMapper (Block name decls stmts) = do
        decls' <- mapM declMapper decls
        return $ Block name decls' stmts
    flatStmtMapper (Case u kw e cases def) = do
        e' <- exprMapper e
        cases' <- mapM caseMapper cases
        return $ Case u kw e' cases' def
    flatStmtMapper (AsgnBlk op lhs expr) =
        exprMapper expr >>= return . AsgnBlk op lhs
    flatStmtMapper (Asgn       lhs expr) =
        exprMapper expr >>= return . Asgn       lhs
    flatStmtMapper (For (x1, e1) cc (x2, e2) stmt) = do
        e1' <- exprMapper e1
        e2' <- exprMapper e2
        cc' <- exprMapper cc
        return $ For (x1, e1') cc' (x2, e2') stmt
    flatStmtMapper (While   e stmt) =
        exprMapper e >>= \e' -> return $ While   e' stmt
    flatStmtMapper (RepeatL e stmt) =
        exprMapper e >>= \e' -> return $ RepeatL e' stmt
    flatStmtMapper (DoWhile e stmt) =
        exprMapper e >>= \e' -> return $ DoWhile e' stmt
    flatStmtMapper (Forever   stmt) = return $ Forever stmt
    flatStmtMapper (If cc s1 s2) =
        exprMapper cc >>= \cc' -> return $ If cc' s1 s2
    flatStmtMapper (Timing event stmt) = return $ Timing event stmt
    flatStmtMapper (Subroutine f exprs) =
        mapM maybeExprMapper exprs >>= return . Subroutine f
    flatStmtMapper (Return expr) =
        exprMapper expr >>= return . Return
    flatStmtMapper (Null) = return Null

    portBindingMapper (p, me) =
        maybeExprMapper me >>= \me' -> return (p, me')

    moduleItemMapper (MIDecl decl) =
        declMapper decl >>= return . MIDecl
    moduleItemMapper (Assign lhs expr) =
        exprMapper expr >>= return . Assign lhs
    moduleItemMapper (Defparam lhs expr) =
        exprMapper expr >>= return . Defparam lhs
    moduleItemMapper (AlwaysC kw stmt) =
        stmtMapper stmt >>= return . AlwaysC kw
    moduleItemMapper (Initial stmt) =
        stmtMapper stmt >>= return . Initial
    moduleItemMapper (MIPackageItem (Function lifetime ret f decls stmts)) = do
        decls' <- mapM declMapper decls
        stmts' <- mapM stmtMapper stmts
        return $ MIPackageItem $ Function lifetime ret f decls' stmts'
    moduleItemMapper (MIPackageItem (Task lifetime f decls stmts)) = do
        decls' <- mapM declMapper decls
        stmts' <- mapM stmtMapper stmts
        return $ MIPackageItem $ Task lifetime f decls' stmts'
    moduleItemMapper (Instance m params x l) = do
        l' <- mapM portBindingMapper l
        return $ Instance m params x l'
    moduleItemMapper (Modport x l) =
        mapM modportDeclMapper l >>= return . Modport x
    moduleItemMapper (NInputGate  kw x lhs exprs) = do
        exprs' <- mapM exprMapper exprs
        return $ NInputGate kw x lhs exprs'
    moduleItemMapper (NOutputGate kw x lhss expr) =
        exprMapper expr >>= return . NOutputGate kw x lhss
    moduleItemMapper (Genvar   x) = return $ Genvar   x
    moduleItemMapper (Generate x) = return $ Generate x
    moduleItemMapper (MIPackageItem (Typedef t x)) =
        return $ MIPackageItem $ Typedef t x
    moduleItemMapper (MIPackageItem (Comment c)) =
        return $ MIPackageItem $ Comment c

    modportDeclMapper (dir, ident, Just e) = do
        e' <- exprMapper e
        return (dir, ident, Just e')
    modportDeclMapper other = return other

traverseExprs :: Mapper Expr -> Mapper ModuleItem
traverseExprs = unmonad traverseExprsM
collectExprsM :: Monad m => CollectorM m Expr -> CollectorM m ModuleItem
collectExprsM = collectify traverseExprsM

traverseLHSsM :: Monad m => MapperM m LHS -> MapperM m ModuleItem
traverseLHSsM mapper item =
    traverseStmtsM (traverseStmtLHSsM mapper) item >>= traverseModuleItemLHSsM
    where
        traverseModuleItemLHSsM (Assign lhs expr) = do
            lhs' <- mapper lhs
            return $ Assign lhs' expr
        traverseModuleItemLHSsM (Defparam lhs expr) = do
            lhs' <- mapper lhs
            return $ Defparam lhs' expr
        traverseModuleItemLHSsM (NOutputGate kw x lhss expr) = do
            lhss' <- mapM mapper lhss
            return $ NOutputGate kw x lhss' expr
        traverseModuleItemLHSsM (NInputGate  kw x lhs exprs) = do
            lhs' <- mapper lhs
            return $ NInputGate kw x lhs' exprs
        traverseModuleItemLHSsM other = return other

traverseLHSs :: Mapper LHS -> Mapper ModuleItem
traverseLHSs = unmonad traverseLHSsM
collectLHSsM :: Monad m => CollectorM m LHS -> CollectorM m ModuleItem
collectLHSsM = collectify traverseLHSsM

traverseNestedLHSsM :: Monad m => MapperM m LHS -> MapperM m LHS
traverseNestedLHSsM mapper = fullMapper
    where
        fullMapper lhs = tl lhs >>= mapper
        tl (LHSIdent  x   ) = return $ LHSIdent x
        tl (LHSBit    l e ) = fullMapper l >>= \l' -> return $ LHSBit    l' e
        tl (LHSRange  l r ) = fullMapper l >>= \l' -> return $ LHSRange  l' r
        tl (LHSDot    l x ) = fullMapper l >>= \l' -> return $ LHSDot    l' x
        tl (LHSConcat lhss) = mapM fullMapper lhss >>= return . LHSConcat

traverseNestedLHSs :: Mapper LHS -> Mapper LHS
traverseNestedLHSs = unmonad traverseNestedLHSsM
collectNestedLHSsM :: Monad m => CollectorM m LHS -> CollectorM m LHS
collectNestedLHSsM = collectify traverseNestedLHSsM

traverseDeclsM :: Monad m => MapperM m Decl -> MapperM m ModuleItem
traverseDeclsM mapper item = do
    item' <- miMapperA item
    traverseStmtsM miMapperB item'
    where
        miMapperA (MIDecl decl) =
            mapper decl >>= return . MIDecl
        miMapperA (MIPackageItem (Function l t x decls s)) = do
            decls' <- mapM mapper decls
            return $ MIPackageItem $ Function l t x decls' s
        miMapperA (MIPackageItem (Task l x decls s)) = do
            decls' <- mapM mapper decls
            return $ MIPackageItem $ Task l x decls' s
        miMapperA other = return other
        miMapperB (Block name decls stmts) = do
            decls' <- mapM mapper decls
            return $ Block name decls' stmts
        miMapperB other = return other

traverseDecls :: Mapper Decl -> Mapper ModuleItem
traverseDecls = unmonad traverseDeclsM
collectDeclsM :: Monad m => CollectorM m Decl -> CollectorM m ModuleItem
collectDeclsM = collectify traverseDeclsM

traverseTypesM :: Monad m => MapperM m Type -> MapperM m ModuleItem
traverseTypesM mapper item =
    miMapper item >>=
    traverseDeclsM declMapper >>=
    traverseExprsM (traverseNestedExprsM exprMapper)
    where
        fullMapper t = tm t >>= mapper
        tm (Reg      r) = return $ Reg      r
        tm (Wire     r) = return $ Wire     r
        tm (Logic    r) = return $ Logic    r
        tm (Alias x  r) = return $ Alias  x r
        tm (Implicit r) = return $ Implicit r
        tm (IntegerT  ) = return $ IntegerT
        tm (InterfaceT x my r) = return $ InterfaceT x my r
        tm (Enum Nothing vals r) =
            return $ Enum Nothing vals r
        tm (Enum (Just t) vals r) = do
            t' <- fullMapper t
            return $ Enum (Just t') vals r
        tm (Struct p fields r) = do
            types <- mapM fullMapper $ map fst fields
            let idents = map snd fields
            return $ Struct p (zip types idents) r
        exprMapper (Cast t e) =
            fullMapper t >>= \t' -> return $ Cast t' e
        exprMapper other = return other
        declMapper (Parameter  t x    e) =
            fullMapper t >>= \t' -> return $ Parameter  t' x   e
        declMapper (Localparam t x    e) =
            fullMapper t >>= \t' -> return $ Localparam t' x   e
        declMapper (Variable d t x a me) =
            fullMapper t >>= \t' -> return $ Variable d t' x a me
        miMapper (MIPackageItem (Function l t x d s)) =
            fullMapper t >>= \t' -> return $ MIPackageItem $ Function l t' x d s
        miMapper (MIPackageItem (other @ (Task _ _ _ _))) =
            return $ MIPackageItem other
        miMapper other = return other

traverseTypes :: Mapper Type -> Mapper ModuleItem
traverseTypes = unmonad traverseTypesM
collectTypesM :: Monad m => CollectorM m Type -> CollectorM m ModuleItem
collectTypesM = collectify traverseTypesM

traverseGenItemsM :: Monad m => MapperM m GenItem -> MapperM m ModuleItem
traverseGenItemsM mapper = moduleItemMapper
    where
        fullMapper = traverseNestedGenItemsM mapper
        moduleItemMapper (Generate genItems) =
            mapM fullMapper genItems >>= return . Generate
        moduleItemMapper other = return other

traverseGenItems :: Mapper GenItem -> Mapper ModuleItem
traverseGenItems = unmonad traverseGenItemsM
collectGenItemsM :: Monad m => CollectorM m GenItem -> CollectorM m ModuleItem
collectGenItemsM = collectify traverseGenItemsM

-- traverses all GenItems within a given GenItem, but doesn't inspect within
-- GenModuleItems
traverseNestedGenItemsM :: Monad m => MapperM m GenItem -> MapperM m GenItem
traverseNestedGenItemsM mapper = fullMapper
    where
        fullMapper genItem = gim genItem >>= mapper
        gim (GenBlock x subItems) = do
            subItems' <- mapM fullMapper subItems
            return $ GenBlock x (concatMap flattenBlocks subItems')
        gim (GenFor a b c d subItems) = do
            subItems' <- mapM fullMapper subItems
            return $ GenFor a b c d (concatMap flattenBlocks subItems')
        gim (GenIf e i1 i2) = do
            i1' <- fullMapper i1
            i2' <- fullMapper i2
            return $ GenIf e i1' i2'
        gim (GenCase e cases def) = do
            caseItems <- mapM (fullMapper . snd) cases
            let cases' = zip (map fst cases) caseItems
            def' <- maybeDo fullMapper def
            return $ GenCase e cases' def'
        gim (GenModuleItem moduleItem) =
            return $ GenModuleItem moduleItem
        gim (GenNull) = return GenNull
        flattenBlocks :: GenItem -> [GenItem]
        flattenBlocks (GenBlock Nothing items) = items
        flattenBlocks other = [other]

traverseAsgnsM :: Monad m => MapperM m (LHS, Expr) -> MapperM m ModuleItem
traverseAsgnsM mapper = moduleItemMapper
    where
        moduleItemMapper item = miMapperA item >>= miMapperB

        miMapperA (Assign lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Assign lhs' expr'
        miMapperA (Defparam lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Defparam lhs' expr'
        miMapperA other = return other

        miMapperB = traverseStmtsM stmtMapper
        stmtMapper (AsgnBlk op lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ AsgnBlk op lhs' expr'
        stmtMapper (Asgn       lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Asgn       lhs' expr'
        stmtMapper other = return other

traverseAsgns :: Mapper (LHS, Expr) -> Mapper ModuleItem
traverseAsgns = unmonad traverseAsgnsM
collectAsgnsM :: Monad m => CollectorM m (LHS, Expr) -> CollectorM m ModuleItem
collectAsgnsM = collectify traverseAsgnsM

traverseNestedModuleItemsM :: Monad m => MapperM m ModuleItem -> MapperM m ModuleItem
traverseNestedModuleItemsM mapper item = do
    Part Module "DNE" [] [item'] <-
        traverseModuleItemsM mapper (Part Module "DNE" [] [item])
    return item'

traverseNestedModuleItems :: Mapper ModuleItem -> Mapper ModuleItem
traverseNestedModuleItems = unmonad traverseNestedModuleItemsM
collectNestedModuleItemsM :: Monad m => CollectorM m ModuleItem -> CollectorM m ModuleItem
collectNestedModuleItemsM = collectify traverseNestedModuleItemsM

traverseNestedStmts :: Mapper Stmt -> Mapper Stmt
traverseNestedStmts = unmonad traverseNestedStmtsM

traverseNestedExprs :: Mapper Expr -> Mapper Expr
traverseNestedExprs = unmonad traverseNestedExprsM
collectNestedExprsM :: Monad m => CollectorM m Expr -> CollectorM m Expr
collectNestedExprsM = collectify traverseNestedExprsM
