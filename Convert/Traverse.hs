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
