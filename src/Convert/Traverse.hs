{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Utilities for traversing AST transformations.
 -}

module Convert.Traverse
( MapperM
, Mapper
, CollectorM
, unmonad
, collectify
, mapBothM
, breakGenerate
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
, traverseNodesM
, traverseNodes
, traverseStmtExprsM
, traverseStmtExprs
, collectStmtExprsM
, traverseLHSsM
, traverseLHSs
, collectLHSsM
, traverseDeclsM
, traverseDecls
, collectDeclsM
, traverseStmtDeclsM
, traverseStmtDecls
, collectStmtDeclsM
, traverseSinglyNestedTypesM
, traverseSinglyNestedTypes
, collectSinglyNestedTypesM
, traverseNestedTypesM
, traverseNestedTypes
, collectNestedTypesM
, traverseExprTypesM
, traverseExprTypes
, collectExprTypesM
, traverseTypeExprsM
, traverseTypeExprs
, collectTypeExprsM
, traverseGenItemExprsM
, traverseGenItemExprs
, collectGenItemExprsM
, traverseDeclNodesM
, traverseDeclNodes
, traverseDeclExprsM
, traverseDeclExprs
, collectDeclExprsM
, traverseDeclTypesM
, traverseDeclTypes
, collectDeclTypesM
, traverseTypesM
, traverseTypes
, collectTypesM
, traverseGenItemsM
, traverseGenItems
, collectGenItemsM
, traverseNestedGenItemsM
, traverseNestedGenItems
, traverseAsgnsM
, traverseAsgns
, collectAsgnsM
, traverseStmtAsgnsM
, traverseStmtAsgns
, collectStmtAsgnsM
, traverseNestedModuleItemsM
, traverseNestedModuleItems
, collectNestedModuleItemsM
, traverseNestedStmtsM
, traverseNestedStmts
, collectNestedStmtsM
, traverseNestedExprsM
, traverseNestedExprs
, collectNestedExprsM
, traverseSinglyNestedExprsM
, traverseSinglyNestedExprs
, collectSinglyNestedExprsM
, traverseLHSExprsM
, traverseLHSExprs
, collectLHSExprsM
, traverseNestedLHSsM
, traverseNestedLHSs
, collectNestedLHSsM
, traverseSinglyNestedLHSsM
, traverseSinglyNestedLHSs
, collectSinglyNestedLHSsM
, traverseFilesM
, traverseFiles
, traverseSinglyNestedGenItemsM
, traverseSinglyNestedStmtsM
, traverseSinglyNestedStmts
, collectSinglyNestedStmtsM
, traverseNetAsVarM
, traverseNetAsVar
, collectNetAsVarM
) where

import Data.Bitraversable (bimapM)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Writer.Strict
import Language.SystemVerilog.AST

type MapperM m t = t -> m t
type Mapper t = t -> t
type CollectorM m t = t -> m ()

unmonad :: (MapperM Identity a -> MapperM Identity b) -> Mapper a -> Mapper b
unmonad traverser mapper = runIdentity . traverser (return . mapper)

collectify :: Monad m => (MapperM m a -> MapperM m b) -> CollectorM m a -> CollectorM m b
collectify traverser collector =
    traverser mapper >=> \_ -> return ()
    where mapper x = collector x >>= \() -> return x

traverseDescriptionsM :: Monad m => MapperM m Description -> MapperM m AST
traverseDescriptionsM = mapM
traverseDescriptions :: Mapper Description -> Mapper AST
traverseDescriptions = map
collectDescriptionsM :: Monad m => CollectorM m Description -> CollectorM m AST
collectDescriptionsM = mapM_

breakGenerate :: ModuleItem -> [ModuleItem] -> [ModuleItem]
breakGenerate (Generate genItems) items =
    foldr breakGenerateStep items genItems
breakGenerate item items = item : items

breakGenerateStep :: GenItem -> [ModuleItem] -> [ModuleItem]
breakGenerateStep (GenModuleItem item) items = item : items
breakGenerateStep genItem (Generate genItems : items) =
    Generate (genItem : genItems) : items
breakGenerateStep genItem items = Generate [genItem] : items

traverseModuleItemsM :: Monad m => MapperM m ModuleItem -> MapperM m Description
traverseModuleItemsM mapper (Part attrs extern kw lifetime name ports items) = do
    items' <- mapM (traverseNestedModuleItemsM mapper) items
    let items'' = foldr breakGenerate [] items'
    return $ Part attrs extern kw lifetime name ports items''
    where
traverseModuleItemsM mapper (PackageItem packageItem) = do
    let item = MIPackageItem packageItem
    item' <- traverseNestedModuleItemsM mapper item
    return $ case item' of
        MIPackageItem packageItem' -> PackageItem packageItem'
        other -> error $ "encountered bad package module item: " ++ show other
traverseModuleItemsM mapper (Package lifetime name items) = do
    let itemsWrapped = map MIPackageItem items
    itemsWrapped' <- mapM (traverseNestedModuleItemsM mapper) itemsWrapped
    let items' = map (\(MIPackageItem item) -> item) $
                    foldr breakGenerate [] itemsWrapped'
    return $ Package lifetime name items'
traverseModuleItemsM mapper (Class lifetime name decls items) = do
    let declsWrapped = map (MIPackageItem . Decl) decls
    declsWrapped' <- mapM (traverseNestedModuleItemsM mapper) declsWrapped
    let decls' = map (\(MIPackageItem (Decl decl)) -> decl) $
                    foldr breakGenerate [] declsWrapped'
    items' <- fmap concat $ mapM indirect items
    return $ Class lifetime name decls' items'
    where
        indirect (qualifier, item) =
            fmap (map (unwrap qualifier) . flip breakGenerate []) $
            traverseNestedModuleItemsM mapper (MIPackageItem item)
        unwrap qualifier = \(MIPackageItem item) -> (qualifier, item)

traverseModuleItems :: Mapper ModuleItem -> Mapper Description
traverseModuleItems = unmonad traverseModuleItemsM
collectModuleItemsM :: Monad m => CollectorM m ModuleItem -> CollectorM m Description
collectModuleItemsM = collectify traverseModuleItemsM

traverseStmtsM :: Monad m => MapperM m Stmt -> MapperM m ModuleItem
traverseStmtsM mapper = moduleItemMapper
    where
        moduleItemMapper (AlwaysC kw stmt) =
            mapper stmt >>= return . AlwaysC kw
        moduleItemMapper (MIPackageItem (Function lifetime ret name decls stmts)) = do
            stmts' <- mapM mapper stmts
            return $ MIPackageItem $ Function lifetime ret name decls stmts'
        moduleItemMapper (MIPackageItem (Task lifetime name decls stmts)) = do
            stmts' <- mapM mapper stmts
            return $ MIPackageItem $ Task lifetime name decls stmts'
        moduleItemMapper (Initial stmt) =
            mapper stmt >>= return . Initial
        moduleItemMapper (Final stmt) =
            mapper stmt >>= return . Final
        moduleItemMapper other = return $ other

traverseStmts :: Mapper Stmt -> Mapper ModuleItem
traverseStmts = unmonad traverseStmtsM
collectStmtsM :: Monad m => CollectorM m Stmt -> CollectorM m ModuleItem
collectStmtsM = collectify traverseStmtsM

traverseNestedStmtsM :: Monad m => MapperM m Stmt -> MapperM m Stmt
traverseNestedStmtsM mapper = fullMapper
    where fullMapper = mapper >=> traverseSinglyNestedStmtsM fullMapper

traverseNestedStmts :: Mapper Stmt -> Mapper Stmt
traverseNestedStmts = unmonad traverseNestedStmtsM
collectNestedStmtsM :: Monad m => CollectorM m Stmt -> CollectorM m Stmt
collectNestedStmtsM = collectify traverseNestedStmtsM

traverseSinglyNestedStmtsM :: Monad m => MapperM m Stmt -> MapperM m Stmt
traverseSinglyNestedStmtsM fullMapper = cs
    where
        cs (StmtAttr a stmt) = fullMapper stmt >>= return . StmtAttr a
        cs (Block _ "" [] []) = return Null
        cs (Block _ "" [] [CommentStmt{}]) = return Null
        cs (Block _ "" [] [stmt]) = fullMapper stmt
        cs (Block _ "" [CommentDecl{}] []) = return Null
        cs (Block Seq name decls stmts) = do
            stmts' <- mapM fullMapper stmts
            return $ Block Seq name decls $ concatMap explode stmts'
            where
                explode :: Stmt -> [Stmt]
                explode (Block Seq "" [] ss) = ss
                explode other = [other]
        cs (Block kw name decls stmts) =
            mapM fullMapper stmts >>= return . Block kw name decls
        cs (Case u kw expr cases) = do
            caseStmts <- mapM fullMapper $ map snd cases
            let cases' = zip (map fst cases) caseStmts
            return $ Case u kw expr cases'
        cs (Asgn op mt lhs expr) = return $ Asgn op mt lhs expr
        cs (For a b c stmt) = fullMapper stmt >>= return . For a b c
        cs (While   e stmt) = fullMapper stmt >>= return . While   e
        cs (RepeatL e stmt) = fullMapper stmt >>= return . RepeatL e
        cs (DoWhile e stmt) = fullMapper stmt >>= return . DoWhile e
        cs (Forever   stmt) = fullMapper stmt >>= return . Forever
        cs (Foreach x vars stmt) = fullMapper stmt >>= return . Foreach x vars
        cs (If NoCheck (Number n) s1 s2) = do
            s1' <- fullMapper s1
            s2' <- fullMapper s2
            return $ case numberToInteger n of
                Nothing -> If NoCheck (Number n) s1' s2'
                Just 0 -> s2'
                Just _ -> s1'
        cs (If u e s1 s2) = do
            s1' <- fullMapper s1
            s2' <- fullMapper s2
            return $ If u e s1' s2'
        cs (Timing event stmt) = fullMapper stmt >>= return . Timing event
        cs (Return expr) = return $ Return expr
        cs (Subroutine expr exprs) = return $ Subroutine expr exprs
        cs (Trigger blocks x) = return $ Trigger blocks x
        cs stmt@Force{} = return stmt
        cs (Assertion a) =
            traverseAssertionStmtsM fullMapper a >>= return . Assertion
        cs (Continue) = return Continue
        cs (Break) = return Break
        cs (Null) = return Null
        cs (CommentStmt c) = return $ CommentStmt c

traverseSinglyNestedStmts :: Mapper Stmt -> Mapper Stmt
traverseSinglyNestedStmts = unmonad traverseSinglyNestedStmtsM
collectSinglyNestedStmtsM :: Monad m => CollectorM m Stmt -> CollectorM m Stmt
collectSinglyNestedStmtsM = collectify traverseSinglyNestedStmtsM

traverseAssertionStmtsM :: Monad m => MapperM m Stmt -> MapperM m Assertion
traverseAssertionStmtsM mapper = assertionMapper
    where
        actionBlockMapper (ActionBlock s1 s2) = do
            s1' <- mapper s1
            s2' <- mapper s2
            return $ ActionBlock s1' s2'
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
        seqExprMapper (SeqExprDelay ms r s) = do
            ms' <- case ms of
                Nothing -> return Nothing
                Just x -> seqExprMapper x >>= return . Just
            r' <- mapBothM mapper r
            s' <- seqExprMapper s
            return $ SeqExprDelay ms' r' s'
        seqExprMapper (SeqExprFirstMatch s items) = do
            s' <- seqExprMapper s
            items' <- mapM seqMatchItemMapper items
            return $ SeqExprFirstMatch s' items'
        seqMatchItemMapper (SeqMatchAsgn (a, b, c)) = do
            c' <- mapper c
            return $ SeqMatchAsgn (a, b, c')
        seqMatchItemMapper (SeqMatchCall x (Args l p)) = do
            l' <- mapM mapper l
            pes <- mapM mapper $ map snd p
            let p' = zip (map fst p) pes
            return $ SeqMatchCall x (Args l' p')
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
        propSpecMapper (PropertySpec mv e pe) = do
            mv' <- mapM (traverseEventExprsM mapper) mv
            e' <- mapper e
            pe' <- propExprMapper pe
            return $ PropertySpec mv' e' pe'
        assertionExprMapper (Concurrent e) =
            propSpecMapper e >>= return . Concurrent
        assertionExprMapper (Immediate d e) =
            mapper e >>= return . Immediate d
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
        stmtMapper (Asgn op mt lhs expr) =
            fullMapper lhs >>= \lhs' -> return $ Asgn op mt lhs' expr
        stmtMapper (For inits me incrs stmt) = do
            inits' <- mapM (bimapM fullMapper return) inits
            let (lhss, asgnOps, exprs) = unzip3 incrs
            lhss' <- mapM fullMapper lhss
            let incrs' = zip3 lhss' asgnOps exprs
            return $ For inits' me incrs' stmt
        stmtMapper (Force kw l e) =
            fullMapper l >>= \l' -> return $ Force kw l' e
        stmtMapper other = return other

traverseStmtLHSs :: Mapper LHS -> Mapper Stmt
traverseStmtLHSs = unmonad traverseStmtLHSsM
collectStmtLHSsM :: Monad m => CollectorM m LHS -> CollectorM m Stmt
collectStmtLHSsM = collectify traverseStmtLHSsM

traverseNestedExprsM :: Monad m => MapperM m Expr -> MapperM m Expr
traverseNestedExprsM mapper = exprMapper
    where exprMapper = mapper >=> traverseSinglyNestedExprsM exprMapper

traverseNestedExprs :: Mapper Expr -> Mapper Expr
traverseNestedExprs = unmonad traverseNestedExprsM
collectNestedExprsM :: Monad m => CollectorM m Expr -> CollectorM m Expr
collectNestedExprsM = collectify traverseNestedExprsM

traverseSinglyNestedExprsM :: Monad m => MapperM m Expr -> MapperM m Expr
traverseSinglyNestedExprsM exprMapper = em
    where
        typeOrExprMapper (Left t) = return $ Left t
        typeOrExprMapper (Right e) =
            exprMapper e >>= return . Right
        em (String s) = return $ String s
        em (Real   s) = return $ Real   s
        em (Number n) = return $ Number n
        em (Time   s) = return $ Time   s
        em (Ident  i) = return $ Ident  i
        em (PSIdent x y) = return $ PSIdent x y
        em (CSIdent x ps y) = do
            tes' <- mapM typeOrExprMapper $ map snd ps
            let ps' = zip (map fst ps) tes'
            return $ CSIdent x ps' y
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
        em (Call  e (Args l p)) = do
            e' <- exprMapper e
            l' <- mapM exprMapper l
            pes <- mapM exprMapper $ map snd p
            let p' = zip (map fst p) pes
            return $ Call e' (Args l' p')
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
        em (Cast tore e) = do
            tore' <- typeOrExprMapper tore
            e' <- exprMapper e
            return $ Cast tore' e'
        em (DimsFn f tore) =
            typeOrExprMapper tore >>= return . DimsFn f
        em (DimFn f tore e) = do
            tore' <- typeOrExprMapper tore
            e' <- exprMapper e
            return $ DimFn f tore' e'
        em (Dot e x) =
            exprMapper e >>= \e' -> return $ Dot e' x
        em (Pattern l) = do
            names <- mapM typeOrExprMapper $ map fst l
            exprs <- mapM exprMapper $ map snd l
            return $ Pattern $ zip names exprs
        em (Inside e l) = do
            e' <- exprMapper e
            l' <- mapM exprMapper l
            return $ Inside e' l'
        em (MinTypMax e1 e2 e3) = do
            e1' <- exprMapper e1
            e2' <- exprMapper e2
            e3' <- exprMapper e3
            return $ MinTypMax e1' e2' e3'
        em (ExprAsgn e1 e2) = do
            e1' <- exprMapper e1
            e2' <- exprMapper e2
            return $ ExprAsgn e1' e2'
        em (Nil) = return Nil

traverseSinglyNestedExprs :: Mapper Expr -> Mapper Expr
traverseSinglyNestedExprs = unmonad traverseSinglyNestedExprsM
collectSinglyNestedExprsM :: Monad m => CollectorM m Expr -> CollectorM m Expr
collectSinglyNestedExprsM = collectify traverseSinglyNestedExprsM

traverseLHSExprsM :: Monad m => MapperM m Expr -> MapperM m LHS
traverseLHSExprsM exprMapper =
    lhsMapper
    where
        lhsMapper (LHSRange l m r) =
            mapBothM exprMapper r >>= return . LHSRange l m
        lhsMapper (LHSBit l e) =
            exprMapper e >>= return . LHSBit l
        lhsMapper (LHSStream o e ls) = do
            e' <- exprMapper e
            return $ LHSStream o e' ls
        lhsMapper other = return other

traverseLHSExprs :: Mapper Expr -> Mapper LHS
traverseLHSExprs = unmonad traverseLHSExprsM
collectLHSExprsM :: Monad m => CollectorM m Expr -> CollectorM m LHS
collectLHSExprsM = collectify traverseLHSExprsM

mapBothM :: Monad m => MapperM m t -> MapperM m (t, t)
mapBothM mapper = bimapM mapper mapper

traverseExprsM :: Monad m => MapperM m Expr -> MapperM m ModuleItem
traverseExprsM exprMapper =
    traverseNodesM exprMapper declMapper typeMapper lhsMapper stmtMapper
    where
    declMapper = traverseDeclNodesM typeMapper exprMapper
    typeMapper = traverseNestedTypesM (traverseTypeExprsM exprMapper)
    lhsMapper = traverseNestedLHSsM (traverseLHSExprsM exprMapper)
    stmtMapper = traverseNestedStmtsM (traverseStmtExprsM exprMapper)

traverseExprs :: Mapper Expr -> Mapper ModuleItem
traverseExprs = unmonad traverseExprsM
collectExprsM :: Monad m => CollectorM m Expr -> CollectorM m ModuleItem
collectExprsM = collectify traverseExprsM

traverseNodesM
    :: Monad m
    => MapperM m Expr
    -> MapperM m Decl
    -> MapperM m Type
    -> MapperM m LHS
    -> MapperM m Stmt
    -> MapperM m ModuleItem
traverseNodesM exprMapper declMapper typeMapper lhsMapper stmtMapper =
    moduleItemMapper
    where

    portBindingMapper (p, e) =
        exprMapper e >>= \e' -> return (p, e')

    paramBindingMapper (p, Left t) =
        typeMapper t >>= \t' -> return (p, Left t')
    paramBindingMapper (p, Right e) =
        exprMapper e >>= \e' -> return (p, Right e')

    moduleItemMapper (MIAttr attr mi) =
        -- note: we exclude expressions in attributes from conversion
        return $ MIAttr attr mi
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
    moduleItemMapper (Final stmt) =
        stmtMapper stmt >>= return . Final
    moduleItemMapper (Assign opt lhs expr) = do
        opt' <- case opt of
            AssignOptionNone -> return $ AssignOptionNone
            AssignOptionDrive s0 s1 -> return $ AssignOptionDrive s0 s1
            AssignOptionDelay delay ->
                exprMapper delay >>= return . AssignOptionDelay
        lhs' <- lhsMapper lhs
        expr' <- exprMapper expr
        return $ Assign opt' lhs' expr'
    moduleItemMapper (MIPackageItem (Function lifetime ret f decls stmts)) = do
        ret' <- typeMapper ret
        decls' <- mapM declMapper decls
        stmts' <- mapM stmtMapper stmts
        return $ MIPackageItem $ Function lifetime ret' f decls' stmts'
    moduleItemMapper (MIPackageItem (Task lifetime f decls stmts)) = do
        decls' <- mapM declMapper decls
        stmts' <- mapM stmtMapper stmts
        return $ MIPackageItem $ Task lifetime f decls' stmts'
    moduleItemMapper (Instance m p x rs l) = do
        p' <- mapM paramBindingMapper p
        l' <- mapM portBindingMapper l
        rs' <- mapM (mapBothM exprMapper) rs
        return $ Instance m p' x rs' l'
    moduleItemMapper (Modport x l) =
        mapM modportDeclMapper l >>= return . Modport x
    moduleItemMapper (NInputGate  kw d x lhs exprs) = do
        d' <- exprMapper d
        exprs' <- mapM exprMapper exprs
        lhs' <- lhsMapper lhs
        return $ NInputGate kw d' x lhs' exprs'
    moduleItemMapper (NOutputGate kw d x lhss expr) = do
        d' <- exprMapper d
        lhss' <- mapM lhsMapper lhss
        expr' <- exprMapper expr
        return $ NOutputGate kw d' x lhss' expr'
    moduleItemMapper (Genvar   x) = return $ Genvar   x
    moduleItemMapper (Generate items) = do
        items' <- mapM (traverseNestedGenItemsM genItemMapper) items
        return $ Generate items'
    moduleItemMapper (MIPackageItem (Directive c)) =
        return $ MIPackageItem $ Directive c
    moduleItemMapper (MIPackageItem (Import x y)) =
        return $ MIPackageItem $ Import x y
    moduleItemMapper (MIPackageItem (Export x y)) =
        return $ MIPackageItem $ Export x y
    moduleItemMapper (MIPackageItem item@DPIImport{}) = do
        let DPIImport spec prop alias typ name decls = item
        typ' <- typeMapper typ
        decls' <- mapM declMapper decls
        let item' = DPIImport spec prop alias typ' name decls'
        return $ MIPackageItem item'
    moduleItemMapper (MIPackageItem (DPIExport spec alias kw name)) =
        return $ MIPackageItem $ DPIExport spec alias kw name
    moduleItemMapper (AssertionItem (mx, a)) = do
        a' <- traverseAssertionStmtsM stmtMapper a
        a'' <- traverseAssertionExprsM exprMapper a'
        return $ AssertionItem (mx, a'')
    moduleItemMapper (ElabTask severity (Args pnArgs kwArgs)) = do
        pnArgs' <- mapM exprMapper pnArgs
        kwArgs' <- fmap (zip kwNames) $ mapM exprMapper kwExprs
        return $ ElabTask severity $ Args pnArgs' kwArgs'
        where (kwNames, kwExprs) = unzip kwArgs

    genItemMapper = traverseGenItemExprsM exprMapper

    modportDeclMapper (dir, ident, e) = do
        e' <- exprMapper e
        return (dir, ident, e')

traverseNodes
    :: Mapper Expr
    -> Mapper Decl
    -> Mapper Type
    -> Mapper LHS
    -> Mapper Stmt
    -> Mapper ModuleItem
traverseNodes exprMapper declMapper typeMapper lhsMapper stmtMapper =
    runIdentity . traverseNodesM
        (return . exprMapper)
        (return . declMapper)
        (return . typeMapper)
        (return . lhsMapper )
        (return . stmtMapper)

traverseStmtExprsM :: Monad m => MapperM m Expr -> MapperM m Stmt
traverseStmtExprsM exprMapper = flatStmtMapper
    where

    declMapper = traverseDeclExprsM exprMapper
    lhsMapper = traverseNestedLHSsM (traverseLHSExprsM exprMapper)

    caseMapper (exprs, stmt) = do
        exprs' <- mapM exprMapper exprs
        return (exprs', stmt)
    flatStmtMapper (StmtAttr attr stmt) =
        -- note: we exclude expressions in attributes from conversion
        return $ StmtAttr attr stmt
    flatStmtMapper (Block kw name decls stmts) = do
        decls' <- mapM declMapper decls
        return $ Block kw name decls' stmts
    flatStmtMapper (Case u kw e cases) = do
        e' <- exprMapper e
        cases' <- mapM caseMapper cases
        return $ Case u kw e' cases'
    flatStmtMapper (Asgn op mt lhs expr) = do
        lhs' <- lhsMapper lhs
        expr' <- exprMapper expr
        return $ Asgn op mt lhs' expr'
    flatStmtMapper (For inits cc asgns stmt) = do
        inits' <- mapM (bimapM return exprMapper) inits
        cc' <- exprMapper cc
        asgns' <- mapM asgnMapper asgns
        return $ For inits' cc' asgns' stmt
    flatStmtMapper (While   e stmt) =
        exprMapper e >>= \e' -> return $ While   e' stmt
    flatStmtMapper (RepeatL e stmt) =
        exprMapper e >>= \e' -> return $ RepeatL e' stmt
    flatStmtMapper (DoWhile e stmt) =
        exprMapper e >>= \e' -> return $ DoWhile e' stmt
    flatStmtMapper (Forever   stmt) = return $ Forever stmt
    flatStmtMapper (Foreach x vars stmt) = return $ Foreach x vars stmt
    flatStmtMapper (If u cc s1 s2) =
        exprMapper cc >>= \cc' -> return $ If u cc' s1 s2
    flatStmtMapper (Timing timing stmt) =
        timingMapper timing >>= \timing' -> return $ Timing timing' stmt
    flatStmtMapper (Subroutine e (Args l p)) = do
        e' <- exprMapper e
        l' <- mapM exprMapper l
        pes <- mapM exprMapper $ map snd p
        let p' = zip (map fst p) pes
        return $ Subroutine e' (Args l' p')
    flatStmtMapper (Return expr) =
        exprMapper expr >>= return . Return
    flatStmtMapper (Trigger blocks x) = return $ Trigger blocks x
    flatStmtMapper (Force kw l e) = do
        l' <- lhsMapper l
        e' <- exprMapper e
        return $ Force kw l' e'
    flatStmtMapper (Assertion a) =
        traverseAssertionExprsM exprMapper a >>= return . Assertion
    flatStmtMapper (Continue) = return Continue
    flatStmtMapper (Break) = return Break
    flatStmtMapper (Null) = return Null
    flatStmtMapper (CommentStmt c) = return $ CommentStmt c

    asgnMapper (l, op, e) = exprMapper e >>= \e' -> return $ (l, op, e')

    timingMapper (Event e) = eventMapper e >>= return . Event
    timingMapper (Delay e) = exprMapper e >>= return . Delay
    timingMapper (Cycle e) = exprMapper e >>= return . Cycle

    eventMapper EventStar = return EventStar
    eventMapper (EventExpr e) =
        traverseEventExprsM exprMapper e >>= return . EventExpr

traverseEventExprsM :: Monad m => MapperM m Expr -> MapperM m EventExpr
traverseEventExprsM mapper (EventExprEdge edge expr) =
    mapper expr >>= return . EventExprEdge edge
traverseEventExprsM mapper (EventExprOr e1 e2) = do
    e1' <- traverseEventExprsM mapper e1
    e2' <- traverseEventExprsM mapper e2
    return $ EventExprOr e1' e2'

traverseStmtExprs :: Mapper Expr -> Mapper Stmt
traverseStmtExprs = unmonad traverseStmtExprsM
collectStmtExprsM :: Monad m => CollectorM m Expr -> CollectorM m Stmt
collectStmtExprsM = collectify traverseStmtExprsM

traverseLHSsM :: Monad m => MapperM m LHS -> MapperM m ModuleItem
traverseLHSsM mapper =
    traverseStmtsM (traverseNestedStmtsM $ traverseStmtLHSsM mapper)
        >=> traverseModuleItemLHSsM
    where
        traverseModuleItemLHSsM (Assign delay lhs expr) = do
            lhs' <- mapper lhs
            return $ Assign delay lhs' expr
        traverseModuleItemLHSsM (Defparam lhs expr) = do
            lhs' <- mapper lhs
            return $ Defparam lhs' expr
        traverseModuleItemLHSsM (NOutputGate kw d x lhss expr) = do
            lhss' <- mapM mapper lhss
            return $ NOutputGate kw d x lhss' expr
        traverseModuleItemLHSsM (NInputGate  kw d x lhs exprs) = do
            lhs' <- mapper lhs
            return $ NInputGate kw d x lhs' exprs
        traverseModuleItemLHSsM (AssertionItem (mx, a)) = do
            converted <-
                traverseNestedStmtsM (traverseStmtLHSsM mapper) (Assertion a)
            return $ case converted of
                Assertion a' -> AssertionItem (mx, a')
                _ -> error $ "redirected AssertionItem traverse failed: "
                        ++ show converted
        traverseModuleItemLHSsM (Generate items) = do
            items' <- mapM (traverseNestedGenItemsM traverGenItemLHSsM) items
            return $ Generate items'
        traverseModuleItemLHSsM other = return other
        traverGenItemLHSsM (GenFor (x1, e1) cc (x2, op2, e2) subItem) = do
            wrapped_x1' <- mapper $ LHSIdent x1
            wrapped_x2' <- mapper $ LHSIdent x2
            let LHSIdent x1' = wrapped_x1'
            let LHSIdent x2' = wrapped_x2'
            return $ GenFor (x1', e1) cc (x2', op2, e2) subItem
        traverGenItemLHSsM other = return other

traverseLHSs :: Mapper LHS -> Mapper ModuleItem
traverseLHSs = unmonad traverseLHSsM
collectLHSsM :: Monad m => CollectorM m LHS -> CollectorM m ModuleItem
collectLHSsM = collectify traverseLHSsM

traverseNestedLHSsM :: Monad m => MapperM m LHS -> MapperM m LHS
traverseNestedLHSsM mapper = fullMapper
    where fullMapper = mapper >=> traverseSinglyNestedLHSsM fullMapper

traverseNestedLHSs :: Mapper LHS -> Mapper LHS
traverseNestedLHSs = unmonad traverseNestedLHSsM
collectNestedLHSsM :: Monad m => CollectorM m LHS -> CollectorM m LHS
collectNestedLHSsM = collectify traverseNestedLHSsM

traverseSinglyNestedLHSsM :: Monad m => MapperM m LHS -> MapperM m LHS
traverseSinglyNestedLHSsM mapper = tl
    where
        tl (LHSIdent  x       ) = return $ LHSIdent x
        tl (LHSBit    l e     ) = mapper l >>= \l' -> return $ LHSBit    l' e
        tl (LHSRange  l m r   ) = mapper l >>= \l' -> return $ LHSRange  l' m r
        tl (LHSDot    l x     ) = mapper l >>= \l' -> return $ LHSDot    l' x
        tl (LHSConcat     lhss) = mapM mapper lhss >>= return . LHSConcat
        tl (LHSStream o e lhss) = mapM mapper lhss >>= return . LHSStream o e

traverseSinglyNestedLHSs :: Mapper LHS -> Mapper LHS
traverseSinglyNestedLHSs = unmonad traverseSinglyNestedLHSsM
collectSinglyNestedLHSsM :: Monad m => CollectorM m LHS -> CollectorM m LHS
collectSinglyNestedLHSsM = collectify traverseSinglyNestedLHSsM

traverseDeclsM :: Monad m => MapperM m Decl -> MapperM m ModuleItem
traverseDeclsM mapper = miMapper
    where
        miMapper (MIPackageItem (Decl decl)) =
            mapper decl >>= return . MIPackageItem . Decl
        miMapper other = return other

traverseDecls :: Mapper Decl -> Mapper ModuleItem
traverseDecls = unmonad traverseDeclsM
collectDeclsM :: Monad m => CollectorM m Decl -> CollectorM m ModuleItem
collectDeclsM = collectify traverseDeclsM

traverseStmtDeclsM :: Monad m => MapperM m Decl -> MapperM m Stmt
traverseStmtDeclsM mapper = stmtMapper
    where
        stmtMapper (Block kw name decls stmts) = do
            decls' <- mapM mapper decls
            return $ Block kw name decls' stmts
        stmtMapper other = return other

traverseStmtDecls :: Mapper Decl -> Mapper Stmt
traverseStmtDecls = unmonad traverseStmtDeclsM
collectStmtDeclsM :: Monad m => CollectorM m Decl -> CollectorM m Stmt
collectStmtDeclsM = collectify traverseStmtDeclsM

traverseSinglyNestedTypesM :: Monad m => MapperM m Type -> MapperM m Type
traverseSinglyNestedTypesM mapper = tm
    where
        typeOrExprMapper (Left t) = mapper t >>= return . Left
        typeOrExprMapper (Right e) = return $ Right e
        tm (Alias         xx    rs) = return $ Alias         xx    rs
        tm (PSAlias ps    xx    rs) = return $ PSAlias ps    xx    rs
        tm (CSAlias ps pm xx    rs) = do
            vals' <- mapM typeOrExprMapper $ map snd pm
            let pm' = zip (map fst pm) vals'
            return $ CSAlias ps pm' xx rs
        tm (Implicit         sg rs) = return $ Implicit         sg rs
        tm (IntegerVector kw sg rs) = return $ IntegerVector kw sg rs
        tm (IntegerAtom   kw sg   ) = return $ IntegerAtom   kw sg
        tm (NonInteger    kw      ) = return $ NonInteger    kw
        tm (TypeOf        expr    ) = return $ TypeOf        expr
        tm (TypedefRef    expr    ) = return $ TypedefRef    expr
        tm (InterfaceT x y r) = return $ InterfaceT x y r
        tm (Enum t vals r) = do
            t' <- mapper t
            return $ Enum t' vals r
        tm (Struct p fields r) = do
            types <- mapM mapper $ map fst fields
            let idents = map snd fields
            return $ Struct p (zip types idents) r
        tm (Union p fields r) = do
            types <- mapM mapper $ map fst fields
            let idents = map snd fields
            return $ Union p (zip types idents) r
        tm (UnpackedType t r) = do
            t' <- mapper t
            return $ UnpackedType t' r
        tm Void = return Void

traverseSinglyNestedTypes :: Mapper Type -> Mapper Type
traverseSinglyNestedTypes = unmonad traverseSinglyNestedTypesM
collectSinglyNestedTypesM :: Monad m => CollectorM m Type -> CollectorM m Type
collectSinglyNestedTypesM = collectify traverseSinglyNestedTypesM

traverseNestedTypesM :: Monad m => MapperM m Type -> MapperM m Type
traverseNestedTypesM mapper = fullMapper
    where fullMapper = mapper >=> traverseSinglyNestedTypesM fullMapper

traverseNestedTypes :: Mapper Type -> Mapper Type
traverseNestedTypes = unmonad traverseNestedTypesM
collectNestedTypesM :: Monad m => CollectorM m Type -> CollectorM m Type
collectNestedTypesM = collectify traverseNestedTypesM

traverseExprTypesM :: Monad m => MapperM m Type -> MapperM m Expr
traverseExprTypesM mapper = exprMapper
    where
        typeOrExprMapper (Right e) = return $ Right e
        typeOrExprMapper (Left t) =
            mapper t >>= return . Left
        exprMapper (Cast tore e) =
            typeOrExprMapper tore >>= return . flip Cast e
        exprMapper (DimsFn f tore) =
            typeOrExprMapper tore >>= return . DimsFn f
        exprMapper (DimFn f tore e) = do
            tore' <- typeOrExprMapper tore
            return $ DimFn f tore' e
        exprMapper (Pattern l) = do
            names <- mapM typeOrExprMapper $ map fst l
            let exprs = map snd l
            return $ Pattern $ zip names exprs
        exprMapper other = return other

traverseExprTypes :: Mapper Type -> Mapper Expr
traverseExprTypes = unmonad traverseExprTypesM
collectExprTypesM :: Monad m => CollectorM m Type -> CollectorM m Expr
collectExprTypesM = collectify traverseExprTypesM

traverseTypeExprsM :: Monad m => MapperM m Expr -> MapperM m Type
traverseTypeExprsM exprMapper =
    typeMapper
    where
        typeOrExprMapper (Left t) = return $ Left t
        typeOrExprMapper (Right e) = exprMapper e >>= return . Right
        typeMapper (TypeOf expr) =
            exprMapper expr >>= return . TypeOf
        -- TypedefRef root is a reference to a port, but the "field" here is
        -- really a typename; this indirection circumvents the interface
        -- expression resolution check and ensures the underlying modport is
        -- appropriately resolved to the corresponding interface instance
        typeMapper (TypedefRef expr) = do
            let Dot inn typ = expr
            let wrap = Dot inn "*"
            wrap' <- exprMapper wrap
            let Dot inn' "*" = wrap'
            return $ TypedefRef $ Dot inn' typ
        typeMapper (CSAlias ps pm xx rs) = do
            vals' <- mapM typeOrExprMapper $ map snd pm
            let pm' = zip (map fst pm) vals'
            rs' <- mapM (mapBothM exprMapper) rs
            return $ CSAlias ps pm' xx rs'
        typeMapper (Enum t enumItems rs) = do
            enumItems' <- mapM enumItemMapper enumItems
            rs' <- mapM (mapBothM exprMapper) rs
            return $ Enum t enumItems' rs'
            where enumItemMapper (x, e) = exprMapper e >>= \e' -> return (x, e')
        typeMapper t = do
            let (tf, rs) = typeRanges t
            rs' <- mapM (mapBothM exprMapper) rs
            return $ tf rs'

traverseTypeExprs :: Mapper Expr -> Mapper Type
traverseTypeExprs = unmonad traverseTypeExprsM
collectTypeExprsM :: Monad m => CollectorM m Expr -> CollectorM m Type
collectTypeExprsM = collectify traverseTypeExprsM

traverseGenItemExprsM :: Monad m => MapperM m Expr -> MapperM m GenItem
traverseGenItemExprsM exprMapper =
    genItemMapper
    where
        genItemMapper (GenFor (x1, e1) cc (x2, op2, e2) subItem) = do
            e1' <- exprMapper e1
            e2' <- exprMapper e2
            cc' <- exprMapper cc
            return $ GenFor (x1, e1') cc' (x2, op2, e2') subItem
        genItemMapper (GenIf e i1 i2) = do
            e' <- exprMapper e
            return $ GenIf e' i1 i2
        genItemMapper (GenCase e cases) = do
            e' <- exprMapper e
            caseExprs <- mapM (mapM exprMapper . fst) cases
            let cases' = zip caseExprs (map snd cases)
            return $ GenCase e' cases'
        genItemMapper other = return other

traverseGenItemExprs :: Mapper Expr -> Mapper GenItem
traverseGenItemExprs = unmonad traverseGenItemExprsM
collectGenItemExprsM :: Monad m => CollectorM m Expr -> CollectorM m GenItem
collectGenItemExprsM = collectify traverseGenItemExprsM

traverseDeclNodesM
    :: Monad m => MapperM m Type -> MapperM m Expr -> MapperM m Decl
traverseDeclNodesM typeMapper exprMapper =
    declMapper
    where
        declMapper (Param s t x e) = do
            t' <- typeMapper t
            e' <- exprMapper e
            return $ Param s t' x e'
        declMapper (ParamType s x t) = do
            t' <- typeMapper t
            return $ ParamType s x t'
        declMapper (Variable d t x a e) = do
            t' <- typeMapper t
            a' <- mapM (mapBothM exprMapper) a
            e' <- exprMapper e
            return $ Variable d t' x a' e'
        declMapper (Net d n s t x a e) = do
            t' <- typeMapper t
            a' <- mapM (mapBothM exprMapper) a
            e' <- exprMapper e
            return $ Net d n s t' x a' e'
        declMapper (CommentDecl c) =
            return $ CommentDecl c

traverseDeclNodes :: Mapper Type -> Mapper Expr -> Mapper Decl
traverseDeclNodes typeMapper exprMapper =
    runIdentity . traverseDeclNodesM
        (return . typeMapper)
        (return . exprMapper)

traverseDeclExprsM :: Monad m => MapperM m Expr -> MapperM m Decl
traverseDeclExprsM exprMapper = traverseDeclNodesM typeMapper exprMapper
    where typeMapper = traverseNestedTypesM (traverseTypeExprsM exprMapper)

traverseDeclExprs :: Mapper Expr -> Mapper Decl
traverseDeclExprs = unmonad traverseDeclExprsM
collectDeclExprsM :: Monad m => CollectorM m Expr -> CollectorM m Decl
collectDeclExprsM = collectify traverseDeclExprsM

traverseDeclTypesM :: Monad m => MapperM m Type -> MapperM m Decl
traverseDeclTypesM typeMapper = traverseDeclNodesM typeMapper exprMapper
    where exprMapper = traverseNestedExprsM (traverseExprTypesM typeMapper)

traverseDeclTypes :: Mapper Type -> Mapper Decl
traverseDeclTypes = unmonad traverseDeclTypesM
collectDeclTypesM :: Monad m => CollectorM m Type -> CollectorM m Decl
collectDeclTypesM = collectify traverseDeclTypesM

traverseTypesM :: Monad m => MapperM m Type -> MapperM m ModuleItem
traverseTypesM typeMapper =
    traverseNodesM exprMapper declMapper typeMapper lhsMapper stmtMapper
    where
        exprMapper = traverseNestedExprsM (traverseExprTypesM typeMapper)
        lhsMapper = traverseNestedLHSsM (traverseLHSExprsM exprMapper)
        stmtMapper = traverseNestedStmtsM $
            traverseStmtDeclsM declMapper >=> traverseStmtExprsM exprMapper
        declMapper = traverseDeclNodesM typeMapper exprMapper

traverseTypes :: Mapper Type -> Mapper ModuleItem
traverseTypes = unmonad traverseTypesM
collectTypesM :: Monad m => CollectorM m Type -> CollectorM m ModuleItem
collectTypesM = collectify traverseTypesM

traverseGenItemsM :: Monad m => MapperM m GenItem -> MapperM m ModuleItem
traverseGenItemsM mapper = moduleItemMapper
    where
        moduleItemMapper (Generate genItems) =
            mapM mapper genItems >>= return . Generate
        moduleItemMapper other = return other

traverseGenItems :: Mapper GenItem -> Mapper ModuleItem
traverseGenItems = unmonad traverseGenItemsM
collectGenItemsM :: Monad m => CollectorM m GenItem -> CollectorM m ModuleItem
collectGenItemsM = collectify traverseGenItemsM

-- traverses all GenItems within a given GenItem, but doesn't inspect within
-- GenModuleItems
traverseNestedGenItemsM :: Monad m => MapperM m GenItem -> MapperM m GenItem
traverseNestedGenItemsM mapper = fullMapper
    where fullMapper = mapper >=> traverseSinglyNestedGenItemsM fullMapper

traverseNestedGenItems :: Mapper GenItem -> Mapper GenItem
traverseNestedGenItems = unmonad traverseNestedGenItemsM

flattenGenBlocks :: GenItem -> [GenItem]
flattenGenBlocks (GenModuleItem (Generate items)) = items
flattenGenBlocks (GenFor _ _ _ GenNull) = []
flattenGenBlocks GenNull = []
flattenGenBlocks other = [other]

traverseSinglyNestedGenItemsM :: Monad m => MapperM m GenItem -> MapperM m GenItem
traverseSinglyNestedGenItemsM fullMapper = gim
    where
        gim (GenBlock x subItems) = do
            subItems' <- mapM fullMapper subItems
            return $ GenBlock x (concatMap flattenGenBlocks subItems')
        gim (GenFor a b c subItem) = do
            subItem' <- fullMapper subItem
            return $ GenFor a b c subItem'
        gim (GenIf e i1 i2) = do
            i1' <- fullMapper i1
            i2' <- fullMapper i2
            return $ GenIf e i1' i2'
        gim (GenCase e cases) = do
            caseItems <- mapM (fullMapper . snd) cases
            let cases' = zip (map fst cases) caseItems
            return $ GenCase e cases'
        gim (GenModuleItem moduleItem) =
            return $ GenModuleItem moduleItem
        gim (GenNull) = return GenNull

traverseAsgnsM :: Monad m => MapperM m (LHS, Expr) -> MapperM m ModuleItem
traverseAsgnsM mapper = moduleItemMapper
    where
        moduleItemMapper = miMapperA >=> miMapperB

        miMapperA (Assign delay lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Assign delay lhs' expr'
        miMapperA (Defparam lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Defparam lhs' expr'
        miMapperA other = return other

        miMapperB = traverseStmtsM $ traverseNestedStmtsM stmtMapper
        stmtMapper = traverseStmtAsgnsM mapper

traverseAsgns :: Mapper (LHS, Expr) -> Mapper ModuleItem
traverseAsgns = unmonad traverseAsgnsM
collectAsgnsM :: Monad m => CollectorM m (LHS, Expr) -> CollectorM m ModuleItem
collectAsgnsM = collectify traverseAsgnsM

traverseStmtAsgnsM :: Monad m => MapperM m (LHS, Expr) -> MapperM m Stmt
traverseStmtAsgnsM mapper = stmtMapper
    where
        stmtMapper (Asgn op mt lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Asgn op mt lhs' expr'
        stmtMapper (Force kw lhs expr) | expr /= Nil = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Force kw lhs' expr'
        stmtMapper other = return other

traverseStmtAsgns :: Mapper (LHS, Expr) -> Mapper Stmt
traverseStmtAsgns = unmonad traverseStmtAsgnsM
collectStmtAsgnsM :: Monad m => CollectorM m (LHS, Expr) -> CollectorM m Stmt
collectStmtAsgnsM = collectify traverseStmtAsgnsM

traverseNestedModuleItemsM :: Monad m => MapperM m ModuleItem -> MapperM m ModuleItem
traverseNestedModuleItemsM mapper = fullMapper
    where
        fullMapper (Generate genItems) = do
            let genItems' = concatMap flattenGenBlocks genItems
            mapM fullGenItemMapper genItems' >>= mapper . Generate
        fullMapper (MIAttr attr mi) =
            fullMapper mi >>= mapper . MIAttr attr
        fullMapper (Initial Null) = return $ Generate []
        fullMapper other = mapper other
        fullGenItemMapper = traverseNestedGenItemsM genItemMapper
        genItemMapper (GenModuleItem moduleItem) =
            fullMapper moduleItem >>= return . GenModuleItem
        genItemMapper (GenIf _ GenNull GenNull) = return GenNull
        genItemMapper (GenIf (Number n) s1 s2) = do
            case numberToInteger n of
                Nothing -> return $ GenIf (Number n) s1 s2
                Just 0 -> genItemMapper s2
                Just _ -> genItemMapper s1
        genItemMapper (GenBlock _ []) = return GenNull
        genItemMapper other = return other

traverseNestedModuleItems :: Mapper ModuleItem -> Mapper ModuleItem
traverseNestedModuleItems = unmonad traverseNestedModuleItemsM
collectNestedModuleItemsM :: Monad m => CollectorM m ModuleItem -> CollectorM m ModuleItem
collectNestedModuleItemsM = collectify traverseNestedModuleItemsM

-- In many conversions, we want to resolve items locally first, and then fall
-- back to looking at other source files, if necessary. This helper captures
-- this behavior, allowing a conversion to fall back to arbitrary global
-- collected item, if one exists. While this isn't foolproof (we could
-- inadvertently resolve a name that doesn't exist in the given file), many
-- projects rely on their toolchain to locate their modules, interfaces,
-- packages, or typenames in other files. Global resolution of modules and
-- interfaces is more commonly expected than global resolution of typenames and
-- packages.
traverseFilesM
    :: (Monoid w, Monad m)
    => CollectorM (Writer w) AST
    -> (w -> MapperM m AST)
    -> MapperM m [AST]
traverseFilesM fileCollectorM fileMapperM files =
    mapM traverseFileM files
    where
        globalNotes = execWriter $ mapM fileCollectorM files
        traverseFileM file =
            fileMapperM notes file
            where
                localNotes = execWriter $ fileCollectorM file
                notes = localNotes <> globalNotes
traverseFiles
    :: Monoid w
    => CollectorM (Writer w) AST
    -> (w -> Mapper AST)
    -> Mapper [AST]
traverseFiles fileCollectorM fileMapper files =
    runIdentity (traverseFilesM fileCollectorM fileMapperM  files)
    where fileMapperM = (\w -> return . fileMapper w)

traverseNetAsVarM :: Monad m => MapperM m Decl -> MapperM m Decl
traverseNetAsVarM func net = do
    let Net d n s t x a e = net
    let var = Variable d t x a e
    var' <- func var
    let Variable d' t' x' a' e' = var'
    let net' = Net d' n s t' x' a' e'
    return net'

traverseNetAsVar :: Mapper Decl -> Mapper Decl
traverseNetAsVar = unmonad traverseNetAsVarM
collectNetAsVarM :: Monad m => CollectorM m Decl -> CollectorM m Decl
collectNetAsVarM = collectify traverseNetAsVarM
