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
traverseModuleItemsM mapper (Part extern kw lifetime name ports items) =
    mapM fullMapper items >>= return . Part extern kw lifetime name ports
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
    Part False Module Nothing "DNE" [] [item'] <-
        traverseModuleItemsM mapper (Part False Module Nothing "DNE" [] [item])
    return $ case item' of
        MIPackageItem packageItem' -> PackageItem packageItem'
        other -> error $ "encountered bad package module item: " ++ show other
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
        fullMapper stmt = mapper stmt >>= cs
        cs (StmtAttr a stmt) = fullMapper stmt >>= return . StmtAttr a
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
        cs (Subroutine f exprs) = return $ Subroutine f exprs
        cs (Trigger x) = return $ Trigger x
        cs (Null) = return Null

traverseStmtLHSsM :: Monad m => MapperM m LHS -> MapperM m Stmt
traverseStmtLHSsM mapper = traverseNestedStmtsM stmtMapper
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
        em (String s) = return $ String s
        em (Number s) = return $ Number s
        em (Ident  i) = return $ Ident  i
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
        em (Call       f (Args l p)) = do
            l' <- mapM maybeExprMapper l
            pes <- mapM maybeExprMapper $ map snd p
            let p' = zip (map fst p) pes
            return $ Call f (Args l' p')
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


traverseExprsM' :: Monad m => TFStrategy -> MapperM m Expr -> MapperM m ModuleItem
traverseExprsM' strat mapper = moduleItemMapper
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
    flatStmtMapper (AsgnBlk op lhs expr) =
        exprMapper expr >>= return . AsgnBlk op lhs
    flatStmtMapper (Asgn    mt lhs expr) =
        exprMapper expr >>= return . Asgn    mt lhs
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
    flatStmtMapper (Subroutine f (Args l p)) = do
        l' <- mapM maybeExprMapper l
        pes <- mapM maybeExprMapper $ map snd p
        let p' = zip (map fst p) pes
        return $ Subroutine f (Args l' p')
    flatStmtMapper (Return expr) =
        exprMapper expr >>= return . Return
    flatStmtMapper (Trigger x) = return $ Trigger x
    flatStmtMapper (Null) = return Null

    initMapper (Left decl) = declMapper decl >>= return . Left
    initMapper (Right (l, e)) = exprMapper e >>= \e' -> return $ Right (l, e')

    asgnMapper (l, op, e) = exprMapper e >>= \e' -> return $ (l, op, e')

    portBindingMapper (p, me) =
        maybeExprMapper me >>= \me' -> return (p, me')

    moduleItemMapper (MIAttr attr mi) =
        -- note: we exclude expressions in attributes from conversion
        return $ MIAttr attr mi
    moduleItemMapper (MIDecl decl) =
        declMapper decl >>= return . MIDecl
    moduleItemMapper (Defparam lhs expr) =
        exprMapper expr >>= return . Defparam lhs
    moduleItemMapper (AlwaysC kw stmt) =
        stmtMapper stmt >>= return . AlwaysC kw
    moduleItemMapper (Initial stmt) =
        stmtMapper stmt >>= return . Initial
    moduleItemMapper (Assign delay lhs expr) = do
        delay' <- maybeExprMapper delay
        expr' <- exprMapper expr
        return $ Assign delay' lhs expr'
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
        return $ NInputGate kw x lhs exprs'
    moduleItemMapper (NOutputGate kw x lhss expr) =
        exprMapper expr >>= return . NOutputGate kw x lhss
    moduleItemMapper (Genvar   x) = return $ Genvar   x
    moduleItemMapper (Generate items) = do
        items' <- mapM (traverseNestedGenItemsM genItemMapper) items
        return $ Generate items'
    moduleItemMapper (MIPackageItem (Typedef t x)) =
        return $ MIPackageItem $ Typedef t x
    moduleItemMapper (MIPackageItem (Comment c)) =
        return $ MIPackageItem $ Comment c

    genItemMapper (GenFor (x1, e1) cc (x2, op2, e2) mn subItems) = do
        e1' <- exprMapper e1
        e2' <- exprMapper e2
        cc' <- exprMapper cc
        return $ GenFor (x1, e1') cc' (x2, op2, e2') mn subItems
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

traverseDeclsM' :: Monad m => TFStrategy -> MapperM m Decl -> MapperM m ModuleItem
traverseDeclsM' strat mapper item = do
    item' <- miMapperA item
    traverseStmtsM' strat miMapperB item'
    where
        miMapperA (MIDecl decl) =
            mapper decl >>= return . MIDecl
        miMapperA (MIPackageItem (Function l t x decls s)) = do
            decls' <-
                if strat == IncludeTFs
                    then mapM mapper decls
                    else return decls
            return $ MIPackageItem $ Function l t x decls' s
        miMapperA (MIPackageItem (Task l x decls s)) = do
            decls' <-
                if strat == IncludeTFs
                    then mapM mapper decls
                    else return decls
            return $ MIPackageItem $ Task l x decls' s
        miMapperA other = return other
        miMapperB (Block name decls stmts) = do
            decls' <- mapM mapper decls
            return $ Block name decls' stmts
        miMapperB other = return other

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

traverseTypesM :: Monad m => MapperM m Type -> MapperM m ModuleItem
traverseTypesM mapper item =
    miMapper item >>=
    traverseDeclsM declMapper >>=
    traverseExprsM (traverseNestedExprsM exprMapper)
    where
        fullMapper t = tm t >>= mapper
        tm (Alias         xx    rs) = return $ Alias         xx    rs
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
        stmtMapper (AsgnBlk op lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ AsgnBlk op lhs' expr'
        stmtMapper (Asgn    mt lhs expr) = do
            (lhs', expr') <- mapper (lhs, expr)
            return $ Asgn    mt lhs' expr'
        stmtMapper other = return other

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

traverseNestedModuleItemsM :: Monad m => MapperM m ModuleItem -> MapperM m ModuleItem
traverseNestedModuleItemsM mapper item = do
    Part False Module Nothing "DNE" [] [item'] <-
        traverseModuleItemsM mapper (Part False Module Nothing "DNE" [] [item])
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
