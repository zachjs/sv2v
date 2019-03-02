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
) where

import Data.Maybe (fromJust)
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
traverseModuleItemsM mapper (Module name ports items) =
    mapM fullMapper items >>= return . Module name ports
    where
        fullMapper (Generate genItems) =
            mapM genItemMapper genItems >>= mapper . Generate
        fullMapper other = mapper other
        -- maps all ModuleItems within the given GenItem
        genItemMapper (GenBlock x subItems) =
            mapM genItemMapper subItems >>= return . GenBlock x
        genItemMapper (GenFor a b c d subItems) =
            mapM genItemMapper subItems >>= return . GenFor a b c d
        genItemMapper (GenIf e i1 i2) = do
            i1' <- genItemMapper i1
            i2' <- genItemMapper i2
            return $ GenIf e i1' i2'
        genItemMapper (GenNull) = return GenNull
        genItemMapper (GenModuleItem moduleItem) = do
            moduleItem' <- fullMapper moduleItem
            return $ case moduleItem' of
                Generate subItems -> GenBlock Nothing subItems
                _ -> GenModuleItem moduleItem'
        genItemMapper (GenCase e cases def) = do
            caseItems <- mapM (genItemMapper . snd) cases
            let cases' = zip (map fst cases) caseItems
            def' <- maybeDo genItemMapper def
            return $ GenCase e cases' def'
traverseModuleItemsM _ orig = return orig

traverseModuleItems :: Mapper ModuleItem -> Mapper Description
traverseModuleItems = unmonad traverseModuleItemsM
collectModuleItemsM :: Monad m => CollectorM m ModuleItem -> CollectorM m Description
collectModuleItemsM = collectify traverseModuleItemsM

traverseStmtsM :: Monad m => MapperM m Stmt -> MapperM m ModuleItem
traverseStmtsM mapper = moduleItemMapper
    where
        moduleItemMapper (AlwaysC kw stmt) =
            fullMapper stmt >>= return . AlwaysC kw
        moduleItemMapper (Function ret name decls stmt) =
            fullMapper stmt >>= return . Function ret name decls
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
        cs (Block decls stmts) = mapM fullMapper stmts >>= return . Block decls
        cs (Case kw expr cases def) = do
            caseStmts <- mapM fullMapper $ map snd cases
            let cases' = zip (map fst cases) caseStmts
            def' <- maybeDo fullMapper def
            return $ Case kw expr cases' def'
        cs (AsgnBlk lhs expr) = return $ AsgnBlk lhs expr
        cs (Asgn    lhs expr) = return $ Asgn    lhs expr
        cs (For a b c stmt) = fullMapper stmt >>= return . For a b c
        cs (If e s1 s2) = do
            s1' <- fullMapper s1
            s2' <- fullMapper s2
            return $ If e s1' s2'
        cs (Timing sense stmt) = fullMapper stmt >>= return . Timing sense
        cs (Null) = return Null

traverseStmtLHSsM :: Monad m => MapperM m LHS -> MapperM m Stmt
traverseStmtLHSsM mapper = traverseNestedStmtsM stmtMapper
    where
        stmtMapper (AsgnBlk lhs expr) = mapper lhs >>= \lhs' -> return $ AsgnBlk lhs' expr
        stmtMapper (Asgn    lhs expr) = mapper lhs >>= \lhs' -> return $ Asgn    lhs' expr
        stmtMapper other = return other

traverseStmtLHSs :: Mapper LHS -> Mapper Stmt
traverseStmtLHSs = unmonad traverseStmtLHSsM
collectStmtLHSsM :: Monad m => CollectorM m LHS -> CollectorM m Stmt
collectStmtLHSsM = collectify traverseStmtLHSsM

traverseNestedExprsM :: Monad m => MapperM m Expr -> MapperM m Expr
traverseNestedExprsM mapper = exprMapper
    where
        exprMapper e = mapper e >>= em
        em (String     s) = return $ String    s
        em (Number     s) = return $ Number    s
        em (ConstBool  b) = return $ ConstBool b
        em (Ident      i) = return $ Ident     i
        em (IdentRange i (e1, e2)) = do
            e1' <- exprMapper e1
            e2' <- exprMapper e2
            return $ IdentRange i (e1', e2')
        em (IdentBit   i e) =
            exprMapper e >>= return . IdentBit i
        em (Repeat     e l) = do
            e' <- exprMapper e
            l' <- mapM exprMapper l
            return $ Repeat e' l'
        em (Concat     l) =
            mapM exprMapper l >>= return . Concat
        em (Call       f l) =
            mapM exprMapper l >>= return . Call f
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
        em (Bit        e n) =
            exprMapper e >>= \e' -> return $ Bit e' n
        em (Cast       t e) =
            exprMapper e >>= return . Cast t
        em (StructAccess e x) =
            exprMapper e >>= \e' -> return $ StructAccess e' x
        em (StructPattern l) = do
            let names = map fst l
            exprs <- mapM exprMapper $ map snd l
            return $ StructPattern $ zip names exprs


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

    exprMapper = traverseNestedExprsM mapper

    caseMapper (exprs, stmt) = do
        exprs' <- mapM exprMapper exprs
        return (exprs', stmt)
    stmtMapper = traverseNestedStmtsM flatStmtMapper
    flatStmtMapper (Block header stmts) = do
        if header == Nothing
            then return $ Block Nothing stmts
            else do
                let Just (name, decls) = header
                decls' <- mapM declMapper decls
                return $ Block (Just (name, decls')) stmts
    flatStmtMapper (Case kw e cases def) = do
        e' <- exprMapper e
        cases' <- mapM caseMapper cases
        return $ Case kw e' cases' def
    flatStmtMapper (AsgnBlk lhs expr) =
        exprMapper expr >>= return . AsgnBlk lhs
    flatStmtMapper (Asgn    lhs expr) =
        exprMapper expr >>= return . Asgn    lhs
    flatStmtMapper (For (x1, e1) cc (x2, e2) stmt) = do
        e1' <- exprMapper e1
        e2' <- exprMapper e2
        cc' <- exprMapper cc
        return $ For (x1, e1') cc' (x2, e2') stmt
    flatStmtMapper (If cc s1 s2) =
        exprMapper cc >>= \cc' -> return $ If cc' s1 s2
    flatStmtMapper (Timing sense stmt) = return $ Timing sense stmt
    flatStmtMapper (Null) = return Null

    portBindingMapper (p, me) =
        maybeExprMapper me >>= \me' -> return (p, me')

    moduleItemMapper (MIDecl decl) =
        declMapper decl >>= return . MIDecl
    moduleItemMapper (Assign lhs expr) =
        exprMapper expr >>= return . Assign lhs
    moduleItemMapper (AlwaysC kw stmt) =
        stmtMapper stmt >>= return . AlwaysC kw
    moduleItemMapper (Function ret f decls stmt) = do
        decls' <- mapM declMapper decls
        stmt' <- stmtMapper stmt
        return $ Function ret f decls' stmt'
    moduleItemMapper (Instance m params x ml) = do
        if ml == Nothing
            then return $ Instance m params x Nothing
            else do
                l <- mapM portBindingMapper (fromJust ml)
                return $ Instance m params x (Just l)
    moduleItemMapper (Comment  x) = return $ Comment  x
    moduleItemMapper (Genvar   x) = return $ Genvar   x
    moduleItemMapper (Generate x) = return $ Generate x

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
        traverseModuleItemLHSsM other = return other

traverseLHSs :: Mapper LHS -> Mapper ModuleItem
traverseLHSs = unmonad traverseLHSsM
collectLHSsM :: Monad m => CollectorM m LHS -> CollectorM m ModuleItem
collectLHSsM = collectify traverseLHSsM

traverseDeclsM :: Monad m => MapperM m Decl -> MapperM m ModuleItem
traverseDeclsM mapper item = do
    item' <- miMapperA item
    traverseStmtsM miMapperB item'
    where
        miMapperA (MIDecl decl) =
            mapper decl >>= return . MIDecl
        miMapperA (Function t x decls s) = do
            decls' <- mapM mapper decls
            return $ Function t x decls' s
        miMapperA other = return other
        miMapperB (Block (Just (name, decls)) stmts) = do
            decls' <- mapM mapper decls
            return $ Block (Just (name, decls')) stmts
        miMapperB other = return other

traverseDecls :: Mapper Decl -> Mapper ModuleItem
traverseDecls = unmonad traverseDeclsM
collectDeclsM :: Monad m => CollectorM m Decl -> CollectorM m ModuleItem
collectDeclsM = collectify traverseDeclsM

traverseTypesM :: Monad m => MapperM m Type -> MapperM m ModuleItem
traverseTypesM mapper item =
    traverseDeclsM declMapper item >>= traverseExprsM exprMapper
    where
        exprMapper (Cast t e) = do
            t' <- mapper t
            -- TODO HACK: If the cast type is no longer "simple", we just drop
            -- the case altogether. This probably doesn't work great in all
            -- cases.
            return $ if elem ' ' (show t')
                then e
                else Cast t' e
        exprMapper other = return other
        declMapper (Parameter  t x    e) =
            mapper t >>= \t' -> return $ Parameter  t' x   e
        declMapper (Localparam t x    e) =
            mapper t >>= \t' -> return $ Localparam t' x   e
        declMapper (Variable d t x a me) =
            mapper t >>= \t' -> return $ Variable d t' x a me

traverseTypes :: Mapper Type -> Mapper ModuleItem
traverseTypes = unmonad traverseTypesM
collectTypesM :: Monad m => CollectorM m Type -> CollectorM m ModuleItem
collectTypesM = collectify traverseTypesM
