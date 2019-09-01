{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Utilities for traversing AST transformations.
 -}

module Convert.Traverse
( MapperM
, Mapper
, CollectorM
, TFStrategy (..)
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
, traverseStmtsM'
, traverseStmts'
, collectStmtsM'
, traverseStmtLHSsM
, traverseStmtLHSs
, collectStmtLHSsM
, traverseExprsM
, traverseExprs
, collectExprsM
, traverseExprsM'
, traverseExprs'
, collectExprsM'
, traverseStmtExprsM
, traverseStmtExprs
, collectStmtExprsM
, traverseLHSsM
, traverseLHSs
, collectLHSsM
, traverseLHSsM'
, traverseLHSs'
, collectLHSsM'
, traverseDeclsM
, traverseDecls
, collectDeclsM
, traverseDeclsM'
, traverseDecls'
, collectDeclsM'
, traverseNestedTypesM
, traverseNestedTypes
, collectNestedTypesM
, traverseTypesM
, traverseTypes
, collectTypesM
, traverseGenItemsM
, traverseGenItems
, collectGenItemsM
, traverseAsgnsM
, traverseAsgns
, collectAsgnsM
, traverseAsgnsM'
, traverseAsgns'
, collectAsgnsM'
, traverseStmtAsgnsM
, traverseStmtAsgns
, collectStmtAsgnsM
, traverseNestedModuleItemsM
, traverseNestedModuleItems
, collectNestedModuleItemsM
, traverseNestedStmts
, collectNestedStmtsM
, traverseNestedExprsM
, traverseNestedExprs
, collectNestedExprsM
, traverseNestedLHSsM
, traverseNestedLHSs
, collectNestedLHSsM
, traverseScopesM
, scopedConversion
, stately
, traverseFiles
) where

import Control.Monad.State
import Control.Monad.Writer
import Language.SystemVerilog.AST

type MapperM m t = t -> m t
type Mapper t = t -> t
type CollectorM m t = t -> m ()

data TFStrategy
    = IncludeTFs
    | ExcludeTFs
    deriving Eq

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
traverseModuleItemsM mapper (Part extern kw lifetime name ports items) = do
    items' <- mapM fullMapper items
    let items'' = concatMap breakGenerate items'
    return $ Part extern kw lifetime name ports items''
    where
        fullMapper (Generate [GenBlock Nothing genItems]) =
            mapM fullGenItemMapper genItems >>= mapper . Generate
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
        breakGenerate :: ModuleItem -> [ModuleItem]
        breakGenerate (Generate genItems) =
            if all isGenModuleItem genItems
                then map (\(GenModuleItem item) -> item) genItems
                else [Generate genItems]
            where
                isGenModuleItem :: GenItem -> Bool
                isGenModuleItem (GenModuleItem _) = True
                isGenModuleItem _ = False
        breakGenerate other = [other]
traverseModuleItemsM mapper (PackageItem packageItem) = do
    let item = MIPackageItem packageItem
    converted <-
        traverseModuleItemsM mapper (Part False Module Nothing "DNE" [] [item])
    let item' = case converted of
            Part False Module Nothing "DNE" [] [newItem] -> newItem
            _ -> error $ "redirected PackageItem traverse failed: "
                    ++ show converted
    return $ case item' of
        MIPackageItem packageItem' -> PackageItem packageItem'
        other -> error $ "encountered bad package module item: " ++ show other
traverseModuleItemsM mapper (Package lifetime name packageItems) = do
    let items = map MIPackageItem packageItems
    converted <-
        traverseModuleItemsM mapper (Part False Module Nothing "DNE" [] items)
    let items' = case converted of
            Part False Module Nothing "DNE" [] newItems -> newItems
            _ -> error $ "redirected Package traverse failed: "
                    ++ show converted
    return $ Package lifetime name $ map (\(MIPackageItem item) -> item) items'
traverseModuleItemsM _ (Directive str) = return $ Directive str

traverseModuleItems :: Mapper ModuleItem -> Mapper Description
traverseModuleItems = unmonad traverseModuleItemsM
collectModuleItemsM :: Monad m => CollectorM m ModuleItem -> CollectorM m Description
collectModuleItemsM = collectify traverseModuleItemsM

traverseStmtsM' :: Monad m => TFStrategy -> MapperM m Stmt -> MapperM m ModuleItem
traverseStmtsM' strat mapper = moduleItemMapper
    where
        moduleItemMapper (AlwaysC kw stmt) =
            fullMapper stmt >>= return . AlwaysC kw
        moduleItemMapper (MIPackageItem (Function lifetime ret name decls stmts)) = do
            stmts' <-
                if strat == IncludeTFs
                    then mapM fullMapper stmts
                    else return stmts
            return $ MIPackageItem $ Function lifetime ret name decls stmts'
        moduleItemMapper (MIPackageItem (Task lifetime name decls stmts)) = do
            stmts' <-
                if strat == IncludeTFs
                    then mapM fullMapper stmts
                    else return stmts
            return $ MIPackageItem $ Task lifetime name decls stmts'
        moduleItemMapper (Initial stmt) =
            fullMapper stmt >>= return . Initial
        moduleItemMapper other = return $ other
        fullMapper = traverseNestedStmtsM mapper

traverseStmts' :: TFStrategy -> Mapper Stmt -> Mapper ModuleItem
traverseStmts' strat = unmonad $ traverseStmtsM' strat
collectStmtsM' :: Monad m => TFStrategy -> CollectorM m Stmt -> CollectorM m ModuleItem
collectStmtsM' strat = collectify $ traverseStmtsM' strat

traverseStmtsM :: Monad m => MapperM m Stmt -> MapperM m ModuleItem
traverseStmtsM = traverseStmtsM' IncludeTFs
traverseStmts :: Mapper Stmt -> Mapper ModuleItem
traverseStmts = traverseStmts' IncludeTFs
collectStmtsM :: Monad m => CollectorM m Stmt -> CollectorM m ModuleItem
collectStmtsM = collectStmtsM' IncludeTFs

-- private utility for turning a thing which maps over a single lever of
-- statements into one that maps over the nested statements first, then the
-- higher levels up
traverseNestedStmtsM :: Monad m => MapperM m Stmt -> MapperM m Stmt
traverseNestedStmtsM mapper = fullMapper
    where
        fullMapper stmt = mapper stmt >>= traverseSinglyNestedStmtsM fullMapper

-- variant of the above which only traverses one level down
traverseSinglyNestedStmtsM :: Monad m => MapperM m Stmt -> MapperM m Stmt
traverseSinglyNestedStmtsM fullMapper = cs
    where
        cs (StmtAttr a stmt) = fullMapper stmt >>= return . StmtAttr a
        cs (Block Nothing [] []) = return Null
        cs (Block name decls stmts) =
            mapM fullMapper stmts >>= return . Block name decls
        cs (Case u kw expr cases def) = do
            caseStmts <- mapM fullMapper $ map snd cases
            let cases' = zip (map fst cases) caseStmts
            def' <- maybeDo fullMapper def
            return $ Case u kw expr cases' def'
        cs (AsgnBlk op lhs expr) = return $ AsgnBlk op lhs expr
        cs (Asgn    mt lhs expr) = return $ Asgn    mt lhs expr
        cs (For a b c stmt) = fullMapper stmt >>= return . For a b c
        cs (While   e stmt) = fullMapper stmt >>= return . While   e
        cs (RepeatL e stmt) = fullMapper stmt >>= return . RepeatL e
        cs (DoWhile e stmt) = fullMapper stmt >>= return . DoWhile e
        cs (Forever   stmt) = fullMapper stmt >>= return . Forever
        cs (If u e s1 s2) = do
            s1' <- fullMapper s1
            s2' <- fullMapper s2
            return $ If u e s1' s2'
        cs (Timing event stmt) = fullMapper stmt >>= return . Timing event
        cs (Return expr) = return $ Return expr
        cs (Subroutine ps f exprs) = return $ Subroutine ps f exprs
        cs (Trigger x) = return $ Trigger x
        cs (Assertion a) =
            traverseAssertionStmtsM fullMapper a >>= return . Assertion
        cs (Null) = return Null

traverseAssertionStmtsM :: Monad m => MapperM m Stmt -> MapperM m Assertion
traverseAssertionStmtsM mapper = assertionMapper
    where
        actionBlockMapper (ActionBlockIf stmt) =
            mapper stmt >>= return . ActionBlockIf
        actionBlockMapper (ActionBlockElse Nothing stmt) =
            mapper stmt >>= return . ActionBlockElse Nothing
        actionBlockMapper (ActionBlockElse (Just s1) s2) = do
            s1' <- mapper s1
            s2' <- mapper s2
            return $ ActionBlockElse (Just s1') s2'
        assertionMapper (Assert e ab) =
            actionBlockMapper ab >>= return . Assert e
        assertionMapper (Assume e ab) =
            actionBlockMapper ab >>= return . Assume e
        assertionMapper (Cover e stmt) =
            mapper stmt >>= return . Cover e

-- Note that this does not include the expressions without the statements of the
-- actions associated with the assertions.
traverseAssertionExprsM :: Monad m => MapperM m Expr -> MapperM m Assertion
traverseAssertionExprsM mapper = assertionMapper
    where
        seqExprMapper (SeqExpr e) =
            mapper e >>= return . SeqExpr
        seqExprMapper (SeqExprAnd        s1 s2) =
            ssMapper   SeqExprAnd        s1 s2
        seqExprMapper (SeqExprOr         s1 s2) =
            ssMapper   SeqExprOr         s1 s2
        seqExprMapper (SeqExprIntersect  s1 s2) =
            ssMapper   SeqExprIntersect  s1 s2
        seqExprMapper (SeqExprWithin     s1 s2) =
            ssMapper   SeqExprWithin     s1 s2
        seqExprMapper (SeqExprThroughout e s) = do
            e' <- mapper e
            s' <- seqExprMapper s
            return $ SeqExprThroughout e' s'
        seqExprMapper (SeqExprDelay ms e s) = do
            ms' <- case ms of
                Nothing -> return Nothing
                Just x -> seqExprMapper x >>= return . Just
            e' <- mapper e
            s' <- seqExprMapper s
            return $ SeqExprDelay ms' e' s'
        seqExprMapper (SeqExprFirstMatch s items) = do
            s' <- seqExprMapper s
            items' <- mapM seqMatchItemMapper items
            return $ SeqExprFirstMatch s' items'
        seqMatchItemMapper (Left (a, b, c)) = do
            c' <- mapper c
            return $ Left (a, b, c')
        seqMatchItemMapper (Right (x, (Args l p))) = do
            l' <- mapM maybeExprMapper l
            pes <- mapM maybeExprMapper $ map snd p
            let p' = zip (map fst p) pes
            return $ Right (x, Args l' p')
        maybeExprMapper Nothing = return Nothing
        maybeExprMapper (Just e) =
            mapper e >>= return . Just
        ppMapper constructor p1 p2 = do
            p1' <- propExprMapper p1
            p2' <- propExprMapper p2
            return $ constructor p1' p2'
        ssMapper constructor s1 s2 = do
            s1' <- seqExprMapper s1
            s2' <- seqExprMapper s2
            return $ constructor s1' s2'
        spMapper constructor se pe = do
            se' <- seqExprMapper se
            pe' <- propExprMapper pe
            return $ constructor se' pe'
        propExprMapper (PropExpr se) =
            seqExprMapper se >>= return . PropExpr
        propExprMapper (PropExprImpliesO se pe) =
            spMapper PropExprImpliesO se pe
        propExprMapper (PropExprImpliesNO se pe) =
            spMapper PropExprImpliesNO se pe
        propExprMapper (PropExprFollowsO se pe) =
            spMapper PropExprFollowsO se pe
        propExprMapper (PropExprFollowsNO se pe) =
            spMapper PropExprFollowsNO se pe
        propExprMapper (PropExprIff p1 p2) =
            ppMapper PropExprIff p1 p2
        propSpecMapper (PropertySpec ms me pe) = do
            me' <- case me of
                Nothing -> return Nothing
                Just e -> mapper e >>= return . Just
            pe' <- propExprMapper pe
            return $ PropertySpec ms me' pe'
        assertionExprMapper (Left e) =
            propSpecMapper e >>= return . Left
        assertionExprMapper (Right e) =
            mapper e >>= return . Right
        assertionMapper (Assert e ab) = do
            e' <- assertionExprMapper e
            return $ Assert e' ab
        assertionMapper (Assume e ab) = do
            e' <- assertionExprMapper e
            return $ Assume e' ab
        assertionMapper (Cover e stmt) = do
            e' <- assertionExprMapper e
            return $ Cover e' stmt

traverseStmtLHSsM :: Monad m => MapperM m LHS -> MapperM m Stmt
traverseStmtLHSsM mapper = stmtMapper
    where
        fullMapper = mapper
        stmtMapper (Timing (Event sense) stmt) = do
            sense' <- senseMapper sense
            return $ Timing (Event sense') stmt
        stmtMapper (Asgn (Just (Event sense)) lhs expr) = do
            lhs' <- fullMapper lhs
            sense' <- senseMapper sense
            return $ Asgn (Just $ Event sense') lhs' expr
        stmtMapper (AsgnBlk op lhs expr) = fullMapper lhs >>= \lhs' -> return $ AsgnBlk op lhs' expr
        stmtMapper (Asgn    mt lhs expr) = fullMapper lhs >>= \lhs' -> return $ Asgn    mt lhs' expr
        stmtMapper (For inits me incrs stmt) = do
            inits' <- mapM mapInit inits
            let (lhss, asgnOps, exprs) = unzip3 incrs
            lhss' <- mapM fullMapper lhss
            let incrs' = zip3 lhss' asgnOps exprs
            return $ For inits' me incrs' stmt
            where
                mapInit (Left decl) = return $ Left decl
                mapInit (Right (lhs, expr)) = do
                    lhs' <- fullMapper lhs
                    return $ Right (lhs', expr)
        stmtMapper (Assertion a) =
            assertionMapper a >>= return . Assertion
        stmtMapper other = return other
        senseMapper (Sense        lhs) = fullMapper lhs >>= return . Sense
        senseMapper (SensePosedge lhs) = fullMapper lhs >>= return . SensePosedge
        senseMapper (SenseNegedge lhs) = fullMapper lhs >>= return . SenseNegedge
        senseMapper (SenseOr    s1 s2) = do
            s1' <- senseMapper s1
            s2' <- senseMapper s2
            return $ SenseOr s1' s2'
        senseMapper (SenseStar       ) = return SenseStar
        assertionExprMapper (Left (PropertySpec (Just sense) me pe)) = do
            sense' <- senseMapper sense
            return $ Left $ PropertySpec (Just sense') me pe
        assertionExprMapper other = return $ other
        assertionMapper (Assert e ab) = do
            e' <- assertionExprMapper e
            return $ Assert e' ab
        assertionMapper (Assume e ab) = do
            e' <- assertionExprMapper e
            return $ Assume e' ab
        assertionMapper (Cover e stmt) = do
            e' <- assertionExprMapper e
            return $ Cover e' stmt

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
        em (String s) = return $ String s
        em (Number s) = return $ Number s
        em (Ident  i) = return $ Ident  i
        em (PSIdent x y) = return $ PSIdent x y
        em (Range e m (e1, e2)) = do
            e' <- exprMapper e
            e1' <- exprMapper e1
            e2' <- exprMapper e2
            return $ Range e' m (e1', e2')
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
        em (Stream o e l) = do
            e' <- exprMapper e
            l' <- mapM exprMapper l
            return $ Stream o e' l'
        em (Call    ps f (Args l p)) = do
            l' <- mapM maybeExprMapper l
            pes <- mapM maybeExprMapper $ map snd p
            let p' = zip (map fst p) pes
            return $ Call ps f (Args l' p')
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
        em (Cast (Left t) e) =
            exprMapper e >>= return . Cast (Left t)
        em (Cast (Right e1) e2) = do
            e1' <- exprMapper e1
            e2' <- exprMapper e2
            return $ Cast (Right e1') e2'
        em (Bits (Right e)) =
            exprMapper e >>= return . Bits . Right
        em (Bits (Left t)) = return $ Bits (Left t)
        em (Dot e x) =
            exprMapper e >>= \e' -> return $ Dot e' x
        em (Pattern l) = do
            let names = map fst l
            exprs <- mapM exprMapper $ map snd l
            return $ Pattern $ zip names exprs

exprMapperHelpers :: Monad m => MapperM m Expr ->
    (MapperM m Range, MapperM m (Maybe Expr), MapperM m Decl, MapperM m LHS, MapperM m Type)
exprMapperHelpers exprMapper =
    (rangeMapper, maybeExprMapper, declMapper, traverseNestedLHSsM lhsMapper, typeMapper)
    where

    rangeMapper (a, b) = do
        a' <- exprMapper a
        b' <- exprMapper b
        return (a', b')

    maybeExprMapper Nothing = return Nothing
    maybeExprMapper (Just e) =
        exprMapper e >>= return . Just

    typeMapper' t = do
        let (tf, rs) = typeRanges t
        rs' <- mapM rangeMapper rs
        return $ tf rs'
    typeMapper = traverseNestedTypesM typeMapper'

    declMapper (Parameter  t x e) = do
        t' <- typeMapper t
        e' <- exprMapper e
        return $ Parameter  t' x e'
    declMapper (Localparam t x e) = do
        t' <- typeMapper t
        e' <- exprMapper e
        return $ Localparam t' x e'
    declMapper (Variable d t x a me) = do
        t' <- typeMapper t
        a' <- mapM rangeMapper a
        me' <- maybeExprMapper me
        return $ Variable d t' x a' me'

    lhsMapper (LHSRange l m r) =
        rangeMapper r >>= return . LHSRange l m
    lhsMapper (LHSBit l e) =
        exprMapper e >>= return . LHSBit l
    lhsMapper other = return other

traverseExprsM' :: Monad m => TFStrategy -> MapperM m Expr -> MapperM m ModuleItem
traverseExprsM' strat exprMapper = moduleItemMapper
    where

    (rangeMapper, maybeExprMapper, declMapper, lhsMapper, typeMapper)
        = exprMapperHelpers exprMapper

    stmtMapper = traverseNestedStmtsM (traverseStmtExprsM exprMapper)

    portBindingMapper (p, me) =
        maybeExprMapper me >>= \me' -> return (p, me')

    moduleItemMapper (MIAttr attr mi) =
        -- note: we exclude expressions in attributes from conversion
        return $ MIAttr attr mi
    moduleItemMapper (MIPackageItem (Typedef t x)) = do
        t' <- typeMapper t
        return $ MIPackageItem $ Typedef t' x
    moduleItemMapper (MIPackageItem (Decl decl)) =
        declMapper decl >>= return . MIPackageItem . Decl
    moduleItemMapper (Defparam lhs expr) = do
        lhs' <- lhsMapper lhs
        expr' <- exprMapper expr
        return $ Defparam lhs' expr'
    moduleItemMapper (AlwaysC kw stmt) =
        stmtMapper stmt >>= return . AlwaysC kw
    moduleItemMapper (Initial stmt) =
        stmtMapper stmt >>= return . Initial
    moduleItemMapper (Assign delay lhs expr) = do
        delay' <- maybeExprMapper delay
        lhs' <- lhsMapper lhs
        expr' <- exprMapper expr
        return $ Assign delay' lhs' expr'
    moduleItemMapper (MIPackageItem (Function lifetime ret f decls stmts)) = do
        decls' <-
            if strat == IncludeTFs
                then mapM declMapper decls
                else return decls
        stmts' <-
            if strat == IncludeTFs
                then mapM stmtMapper stmts
                else return stmts
        return $ MIPackageItem $ Function lifetime ret f decls' stmts'
    moduleItemMapper (MIPackageItem (Task lifetime f decls stmts)) = do
        decls' <-
            if strat == IncludeTFs
                then mapM declMapper decls
                else return decls
        stmts' <-
            if strat == IncludeTFs
                then mapM stmtMapper stmts
                else return stmts
        return $ MIPackageItem $ Task lifetime f decls' stmts'
    moduleItemMapper (Instance m p x r l) = do
        p' <- mapM portBindingMapper p
        l' <- mapM portBindingMapper l
        r' <- mapM rangeMapper r
        return $ Instance m p' x r' l'
    moduleItemMapper (Modport x l) =
        mapM modportDeclMapper l >>= return . Modport x
    moduleItemMapper (NInputGate  kw x lhs exprs) = do
        exprs' <- mapM exprMapper exprs
        lhs' <- lhsMapper lhs
        return $ NInputGate kw x lhs' exprs'
    moduleItemMapper (NOutputGate kw x lhss expr) = do
        lhss' <- mapM lhsMapper lhss
        expr' <- exprMapper expr
        return $ NOutputGate kw x lhss' expr'
    moduleItemMapper (Genvar   x) = return $ Genvar   x
    moduleItemMapper (Generate items) = do
        items' <- mapM (traverseNestedGenItemsM genItemMapper) items
        return $ Generate items'
    moduleItemMapper (MIPackageItem (Comment c)) =
        return $ MIPackageItem $ Comment c
    moduleItemMapper (MIPackageItem (Import x y)) =
        return $ MIPackageItem $ Import x y
    moduleItemMapper (MIPackageItem (Export x)) =
        return $ MIPackageItem $ Export x
    moduleItemMapper (AssertionItem (mx, a)) = do
        a' <- traverseAssertionStmtsM stmtMapper a
        a'' <- traverseAssertionExprsM exprMapper a'
        return $ AssertionItem (mx, a'')

    genItemMapper (GenFor (n1, x1, e1) cc (x2, op2, e2) mn subItems) = do
        e1' <- exprMapper e1
        e2' <- exprMapper e2
        cc' <- exprMapper cc
        return $ GenFor (n1, x1, e1') cc' (x2, op2, e2') mn subItems
    genItemMapper (GenIf e i1 i2) = do
        e' <- exprMapper e
        return $ GenIf e' i1 i2
    genItemMapper (GenCase e cases def) = do
        e' <- exprMapper e
        caseExprs <- mapM (mapM exprMapper . fst) cases
        let cases' = zip caseExprs (map snd cases)
        return $ GenCase e' cases' def
    genItemMapper other = return other

    modportDeclMapper (dir, ident, Just e) = do
        e' <- exprMapper e
        return (dir, ident, Just e')
    modportDeclMapper other = return other

traverseExprs' :: TFStrategy -> Mapper Expr -> Mapper ModuleItem
traverseExprs' strat = unmonad $ traverseExprsM' strat
collectExprsM' :: Monad m => TFStrategy -> CollectorM m Expr -> CollectorM m ModuleItem
collectExprsM' strat = collectify $ traverseExprsM' strat

traverseExprsM :: Monad m => MapperM m Expr -> MapperM m ModuleItem
traverseExprsM = traverseExprsM' IncludeTFs
traverseExprs :: Mapper Expr -> Mapper ModuleItem
traverseExprs = traverseExprs' IncludeTFs
collectExprsM :: Monad m => CollectorM m Expr -> CollectorM m ModuleItem
collectExprsM = collectExprsM' IncludeTFs

traverseStmtExprsM :: Monad m => MapperM m Expr -> MapperM m Stmt
traverseStmtExprsM exprMapper = flatStmtMapper
    where

    (_, maybeExprMapper, declMapper, lhsMapper, _)
        = exprMapperHelpers exprMapper

    caseMapper (exprs, stmt) = do
        exprs' <- mapM exprMapper exprs
        return (exprs', stmt)
    stmtMapper = traverseNestedStmtsM flatStmtMapper
    flatStmtMapper (StmtAttr attr stmt) =
        -- note: we exclude expressions in attributes from conversion
        return $ StmtAttr attr stmt
    flatStmtMapper (Block name decls stmts) = do
        decls' <- mapM declMapper decls
        return $ Block name decls' stmts
    flatStmtMapper (Case u kw e cases def) = do
        e' <- exprMapper e
        cases' <- mapM caseMapper cases
        return $ Case u kw e' cases' def
    flatStmtMapper (AsgnBlk op lhs expr) = do
        lhs' <- lhsMapper lhs
        expr' <- exprMapper expr
        return $ AsgnBlk op lhs' expr'
    flatStmtMapper (Asgn    mt lhs expr) = do
        lhs' <- lhsMapper lhs
        expr' <- exprMapper expr
        return $ Asgn    mt lhs' expr'
    flatStmtMapper (For inits cc asgns stmt) = do
        inits' <- mapM initMapper inits
        cc' <- maybeExprMapper cc
        asgns' <- mapM asgnMapper asgns
        return $ For inits' cc' asgns' stmt
    flatStmtMapper (While   e stmt) =
        exprMapper e >>= \e' -> return $ While   e' stmt
    flatStmtMapper (RepeatL e stmt) =
        exprMapper e >>= \e' -> return $ RepeatL e' stmt
    flatStmtMapper (DoWhile e stmt) =
        exprMapper e >>= \e' -> return $ DoWhile e' stmt
    flatStmtMapper (Forever   stmt) = return $ Forever stmt
    flatStmtMapper (If u cc s1 s2) =
        exprMapper cc >>= \cc' -> return $ If u cc' s1 s2
    flatStmtMapper (Timing event stmt) = return $ Timing event stmt
    flatStmtMapper (Subroutine ps f (Args l p)) = do
        l' <- mapM maybeExprMapper l
        pes <- mapM maybeExprMapper $ map snd p
        let p' = zip (map fst p) pes
        return $ Subroutine ps f (Args l' p')
    flatStmtMapper (Return expr) =
        exprMapper expr >>= return . Return
    flatStmtMapper (Trigger x) = return $ Trigger x
    flatStmtMapper (Assertion a) = do
        a' <- traverseAssertionStmtsM stmtMapper a
        a'' <- traverseAssertionExprsM exprMapper a'
        return $ Assertion a''
    flatStmtMapper (Null) = return Null

    initMapper (Left decl) = declMapper decl >>= return . Left
    initMapper (Right (l, e)) = exprMapper e >>= \e' -> return $ Right (l, e')

    asgnMapper (l, op, e) = exprMapper e >>= \e' -> return $ (l, op, e')

traverseStmtExprs :: Mapper Expr -> Mapper Stmt
traverseStmtExprs = unmonad traverseStmtExprsM
collectStmtExprsM :: Monad m => CollectorM m Expr -> CollectorM m Stmt
collectStmtExprsM = collectify traverseStmtExprsM

traverseLHSsM' :: Monad m => TFStrategy -> MapperM m LHS -> MapperM m ModuleItem
traverseLHSsM' strat mapper item =
    traverseStmtsM' strat (traverseStmtLHSsM mapper) item >>= traverseModuleItemLHSsM
    where
        traverseModuleItemLHSsM (Assign delay lhs expr) = do
            lhs' <- mapper lhs
            return $ Assign delay lhs' expr
        traverseModuleItemLHSsM (Defparam lhs expr) = do
            lhs' <- mapper lhs
            return $ Defparam lhs' expr
        traverseModuleItemLHSsM (NOutputGate kw x lhss expr) = do
            lhss' <- mapM mapper lhss
            return $ NOutputGate kw x lhss' expr
        traverseModuleItemLHSsM (NInputGate  kw x lhs exprs) = do
            lhs' <- mapper lhs
            return $ NInputGate kw x lhs' exprs
        traverseModuleItemLHSsM (AssertionItem (mx, a)) = do
            converted <-
                traverseNestedStmtsM (traverseStmtLHSsM mapper) (Assertion a)
            return $ case converted of
                Assertion a' -> AssertionItem (mx, a')
                _ -> error $ "redirected AssertionItem traverse failed: "
                        ++ show converted
        traverseModuleItemLHSsM other = return other

traverseLHSs' :: TFStrategy -> Mapper LHS -> Mapper ModuleItem
traverseLHSs' strat = unmonad $ traverseLHSsM' strat
collectLHSsM' :: Monad m => TFStrategy -> CollectorM m LHS -> CollectorM m ModuleItem
collectLHSsM' strat = collectify $ traverseLHSsM' strat

traverseLHSsM :: Monad m => MapperM m LHS -> MapperM m ModuleItem
traverseLHSsM = traverseLHSsM' IncludeTFs
traverseLHSs :: Mapper LHS -> Mapper ModuleItem
traverseLHSs = traverseLHSs' IncludeTFs
collectLHSsM :: Monad m => CollectorM m LHS -> CollectorM m ModuleItem
collectLHSsM = collectLHSsM' IncludeTFs

traverseNestedLHSsM :: Monad m => MapperM m LHS -> MapperM m LHS
traverseNestedLHSsM mapper = fullMapper
    where
        fullMapper lhs = mapper lhs >>= tl
        tl (LHSIdent  x    ) = return $ LHSIdent x
        tl (LHSBit    l e  ) = fullMapper l >>= \l' -> return $ LHSBit    l' e
        tl (LHSRange  l m r) = fullMapper l >>= \l' -> return $ LHSRange  l' m r
        tl (LHSDot    l x  ) = fullMapper l >>= \l' -> return $ LHSDot    l' x
        tl (LHSConcat lhss ) = mapM fullMapper lhss >>= return . LHSConcat

traverseNestedLHSs :: Mapper LHS -> Mapper LHS
traverseNestedLHSs = unmonad traverseNestedLHSsM
collectNestedLHSsM :: Monad m => CollectorM m LHS -> CollectorM m LHS
collectNestedLHSsM = collectify traverseNestedLHSsM

traverseDeclsM' :: Monad m => TFStrategy -> MapperM m Decl -> MapperM m ModuleItem
traverseDeclsM' strat mapper item = do
    item' <- miMapper item
    traverseStmtsM' strat stmtMapper item'
    where
        miMapper (MIPackageItem (Decl decl)) =
            mapper decl >>= return . MIPackageItem . Decl
        miMapper (MIPackageItem (Function l t x decls stmts)) = do
            decls' <-
                if strat == IncludeTFs
                    then mapM mapper decls
                    else return decls
            return $ MIPackageItem $ Function l t x decls' stmts
        miMapper (MIPackageItem (Task l x decls stmts)) = do
            decls' <-
                if strat == IncludeTFs
                    then mapM mapper decls
                    else return decls
            return $ MIPackageItem $ Task l x decls' stmts
        miMapper other = return other
        stmtMapper (Block name decls stmts) = do
            decls' <- mapM mapper decls
            return $ Block name decls' stmts
        stmtMapper other = return other

traverseDecls' :: TFStrategy -> Mapper Decl -> Mapper ModuleItem
traverseDecls' strat = unmonad $ traverseDeclsM' strat
collectDeclsM' :: Monad m => TFStrategy -> CollectorM m Decl -> CollectorM m ModuleItem
collectDeclsM' strat = collectify $ traverseDeclsM' strat

traverseDeclsM :: Monad m => MapperM m Decl -> MapperM m ModuleItem
traverseDeclsM = traverseDeclsM' IncludeTFs
traverseDecls :: Mapper Decl -> Mapper ModuleItem
traverseDecls = traverseDecls' IncludeTFs
collectDeclsM :: Monad m => CollectorM m Decl -> CollectorM m ModuleItem
collectDeclsM = collectDeclsM' IncludeTFs

traverseNestedTypesM :: Monad m => MapperM m Type -> MapperM m Type
traverseNestedTypesM mapper = fullMapper
    where
        fullMapper t = tm t >>= mapper
        tm (Alias      ps xx    rs) = return $ Alias      ps xx    rs
        tm (Net           kw    rs) = return $ Net           kw    rs
        tm (Implicit         sg rs) = return $ Implicit         sg rs
        tm (IntegerVector kw sg rs) = return $ IntegerVector kw sg rs
        tm (IntegerAtom   kw sg   ) = return $ IntegerAtom   kw sg
        tm (NonInteger    kw      ) = return $ NonInteger    kw
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
        tm (Union p fields r) = do
            types <- mapM fullMapper $ map fst fields
            let idents = map snd fields
            return $ Union p (zip types idents) r

traverseNestedTypes :: Mapper Type -> Mapper Type
traverseNestedTypes = unmonad traverseNestedTypesM
collectNestedTypesM :: Monad m => CollectorM m Type -> CollectorM m Type
collectNestedTypesM = collectify traverseNestedTypesM

traverseTypesM :: Monad m => MapperM m Type -> MapperM m ModuleItem
traverseTypesM mapper item =
    miMapper item >>=
    traverseDeclsM declMapper >>=
    traverseExprsM (traverseNestedExprsM exprMapper)
    where
        fullMapper = traverseNestedTypesM mapper
        exprMapper (Cast (Left t) e) =
            fullMapper t >>= \t' -> return $ Cast (Left t') e
        exprMapper (Bits (Left t)) =
            fullMapper t >>= return . Bits . Left
        exprMapper other = return other
        declMapper (Parameter  t x    e) =
            fullMapper t >>= \t' -> return $ Parameter  t' x   e
        declMapper (Localparam t x    e) =
            fullMapper t >>= \t' -> return $ Localparam t' x   e
        declMapper (Variable d t x a me) =
            fullMapper t >>= \t' -> return $ Variable d t' x a me
        miMapper (MIPackageItem (Typedef t x)) =
            fullMapper t >>= \t' -> return $ MIPackageItem $ Typedef t' x
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
        fullMapper stmt =
            mapper stmt >>= traverseSinglyNestedGenItemsM fullMapper

traverseSinglyNestedGenItemsM :: Monad m => MapperM m GenItem -> MapperM m GenItem
traverseSinglyNestedGenItemsM fullMapper = gim
    where
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

traverseAsgnsM' :: Monad m => TFStrategy -> MapperM m (LHS, Expr) -> MapperM m ModuleItem
traverseAsgnsM' strat mapper = moduleItemMapper
    where
        moduleItemMapper item = miMapperA item >>= miMapperB

        miMapperA (Assign delay lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Assign delay lhs' expr'
        miMapperA (Defparam lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Defparam lhs' expr'
        miMapperA other = return other

        miMapperB = traverseStmtsM' strat stmtMapper
        stmtMapper = traverseStmtAsgnsM mapper

traverseAsgns' :: TFStrategy -> Mapper (LHS, Expr) -> Mapper ModuleItem
traverseAsgns' strat = unmonad $ traverseAsgnsM' strat
collectAsgnsM' :: Monad m => TFStrategy -> CollectorM m (LHS, Expr) -> CollectorM m ModuleItem
collectAsgnsM' strat = collectify $ traverseAsgnsM' strat

traverseAsgnsM :: Monad m => MapperM m (LHS, Expr) -> MapperM m ModuleItem
traverseAsgnsM = traverseAsgnsM' IncludeTFs
traverseAsgns :: Mapper (LHS, Expr) -> Mapper ModuleItem
traverseAsgns = traverseAsgns' IncludeTFs
collectAsgnsM :: Monad m => CollectorM m (LHS, Expr) -> CollectorM m ModuleItem
collectAsgnsM = collectAsgnsM' IncludeTFs

traverseStmtAsgnsM :: Monad m => MapperM m (LHS, Expr) -> MapperM m Stmt
traverseStmtAsgnsM mapper = stmtMapper
    where
        stmtMapper (AsgnBlk op lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ AsgnBlk op lhs' expr'
        stmtMapper (Asgn    mt lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Asgn    mt lhs' expr'
        stmtMapper other = return other

traverseStmtAsgns :: Mapper (LHS, Expr) -> Mapper Stmt
traverseStmtAsgns = unmonad traverseStmtAsgnsM
collectStmtAsgnsM :: Monad m => CollectorM m (LHS, Expr) -> CollectorM m Stmt
collectStmtAsgnsM = collectify traverseStmtAsgnsM

traverseNestedModuleItemsM :: Monad m => MapperM m ModuleItem -> MapperM m ModuleItem
traverseNestedModuleItemsM mapper item = do
    converted <-
        traverseModuleItemsM mapper (Part False Module Nothing "DNE" [] [item])
    let items' = case converted of
            Part False Module Nothing "DNE" [] newItems -> newItems
            _ -> error $ "redirected NestedModuleItems traverse failed: "
                    ++ show converted
    return $ case items' of
        [item'] -> item'
        _ -> Generate $ map GenModuleItem items'

traverseNestedModuleItems :: Mapper ModuleItem -> Mapper ModuleItem
traverseNestedModuleItems = unmonad traverseNestedModuleItemsM
collectNestedModuleItemsM :: Monad m => CollectorM m ModuleItem -> CollectorM m ModuleItem
collectNestedModuleItemsM = collectify traverseNestedModuleItemsM

traverseNestedStmts :: Mapper Stmt -> Mapper Stmt
traverseNestedStmts = unmonad traverseNestedStmtsM
collectNestedStmtsM :: Monad m => CollectorM m Stmt -> CollectorM m Stmt
collectNestedStmtsM = collectify traverseNestedStmtsM

traverseNestedExprs :: Mapper Expr -> Mapper Expr
traverseNestedExprs = unmonad traverseNestedExprsM
collectNestedExprsM :: Monad m => CollectorM m Expr -> CollectorM m Expr
collectNestedExprsM = collectify traverseNestedExprsM

-- Traverse all the declaration scopes within a ModuleItem. Note that Functions,
-- Tasks, Always and Initial blocks are all NOT passed through ModuleItem
-- mapper, and Decl ModuleItems are NOT passed through the Decl mapper. The
-- state is restored to its previous value after each scope is exited. Only the
-- Decl mapper may modify the state, as we maintain the invariant that all other
-- functions restore the state on exit. The Stmt mapper must not traverse
-- statements recursively, as we add a recursive wrapper here.
traverseScopesM
    :: (Eq s, Show s)
    => MapperM (State s) Decl
    -> MapperM (State s) ModuleItem
    -> MapperM (State s) Stmt
    -> MapperM (State s) ModuleItem
traverseScopesM declMapper moduleItemMapper stmtMapper =
    fullModuleItemMapper
    where

        nestedStmtMapper stmt =
            stmtMapper stmt >>= traverseSinglyNestedStmtsM fullStmtMapper
        fullStmtMapper (Block name decls stmts) = do
            prevState <- get
            decls' <- mapM declMapper decls
            block <- nestedStmtMapper $ Block name decls' stmts
            put prevState
            return block
        fullStmtMapper other = nestedStmtMapper other

        redirectModuleItem (MIPackageItem (Function ml t x decls stmts)) = do
            prevState <- get
            t' <- do
                res <- declMapper $ Variable Local t x [] Nothing
                case res of
                    Variable Local newType _ [] Nothing -> return newType
                    _ -> error $ "redirected func ret traverse failed: " ++ show res
            decls' <- mapM declMapper decls
            stmts' <- mapM fullStmtMapper stmts
            put prevState
            return $ MIPackageItem $ Function ml t' x decls' stmts'
        redirectModuleItem (MIPackageItem (Task     ml   x decls stmts)) = do
            prevState <- get
            decls' <- mapM declMapper decls
            stmts' <- mapM fullStmtMapper stmts
            put prevState
            return $ MIPackageItem $ Task     ml    x decls' stmts'
        redirectModuleItem (AlwaysC kw stmt) =
            fullStmtMapper stmt >>= return . AlwaysC kw
        redirectModuleItem (Initial stmt) =
            fullStmtMapper stmt >>= return . Initial
        redirectModuleItem item =
            moduleItemMapper item

        -- This previously checked the invariant that the module item mappers
        -- should not modify the state. Now we simply "enforce" it but resetting
        -- the state to its previous value. Comparing the state, as we did
        -- previously, incurs a noticeable performance hit.
        fullModuleItemMapper item = do
            prevState <- get
            item' <- redirectModuleItem item
            put prevState
            return item'

-- applies the given decl conversion across the description, and then performs a
-- scoped traversal for each ModuleItem in the description
scopedConversion
    :: (Eq s, Show s)
    => MapperM (State s) Decl
    -> MapperM (State s) ModuleItem
    -> MapperM (State s) Stmt
    -> s
    -> Description
    -> Description
scopedConversion traverseDeclM traverseModuleItemM traverseStmtM s description =
    evalState (initialTraverse description >>= scopedTraverse) s
    where
        initialTraverse = traverseModuleItemsM traverseMIPackageItemDecl
        scopedTraverse = traverseModuleItemsM $
            traverseScopesM traverseDeclM traverseModuleItemM traverseStmtM
        traverseMIPackageItemDecl (MIPackageItem (Decl decl)) =
            traverseDeclM decl >>= return . MIPackageItem . Decl
        traverseMIPackageItemDecl other = return other

-- convert a basic mapper with an initial argument to a stateful mapper
stately :: (Eq s, Show s) => (s -> Mapper a) -> MapperM (State s) a
stately mapper thing = do
    s <- get
    return $ mapper s thing

-- In many conversions, we want to resolve items locally first, and then fall
-- back to looking at other source files, if necessary. This helper captures
-- this behavior, allowing a conversion to fall back to arbitrary global
-- collected item, if one exists. While this isn't foolproof (we could
-- inadvertently resolve a name that doesn't exist in the given file), many
-- projects rely on their toolchain to locate their modules, interfaces,
-- packages, or typenames in other files. Global resolution of modules and
-- interfaces is more commonly expected than global resolution of typenames and
-- packages.
traverseFiles
    :: Monoid w
    => CollectorM (Writer w) AST
    -> (w -> Mapper AST)
    -> Mapper [AST]
traverseFiles fileCollectorM fileMapper files =
    map traverseFile files
    where
        globalNotes = execWriter $ mapM fileCollectorM files
        traverseFile file =
            fileMapper notes file
            where
                localNotes = execWriter $ fileCollectorM file
                notes = localNotes <> globalNotes
