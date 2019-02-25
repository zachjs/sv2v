{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Utilities for traversing AST transformations.
 -}

module Convert.Traverse
( MapperM
, Mapper
, unmonad
, traverseDescriptionsM
, traverseDescriptions
, traverseModuleItemsM
, traverseModuleItems
, traverseStmtsM
, traverseStmts
) where

import Control.Monad.State
import Data.Maybe
import Language.SystemVerilog.AST

type MapperM s t = t -> (State s) t
type Mapper t = t -> t

unmonad :: (MapperM () a -> MapperM () b) -> Mapper a -> Mapper b
unmonad traverser mapper thing =
    evalState (traverser (return . mapper) thing) ()

traverseDescriptionsM :: MapperM s Description -> MapperM s AST
traverseDescriptionsM mapper descriptions =
    mapM mapper descriptions

traverseDescriptions :: Mapper Description -> Mapper AST
traverseDescriptions = unmonad traverseDescriptionsM

traverseModuleItemsM :: MapperM s ModuleItem -> MapperM s Description
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
        genItemMapper (GenModuleItem moduleItem) =
            fullMapper moduleItem >>= return . GenModuleItem
        genItemMapper (GenCase e cases def) = do
            caseItems <- mapM (genItemMapper . snd) cases
            let cases' = zip (map fst cases) caseItems
            def' <- if def == Nothing
                    then return Nothing
                    else genItemMapper (fromJust def) >>= \x -> return $ Just x
            return $ GenCase e cases' def'
traverseModuleItemsM _ orig = return orig

traverseModuleItems :: Mapper ModuleItem -> Mapper Description
traverseModuleItems = unmonad traverseModuleItemsM

traverseStmtsM :: MapperM s Stmt -> MapperM s ModuleItem
traverseStmtsM mapper = moduleItemMapper
    where
        moduleItemMapper (AlwaysC kw stmt) =
            fullMapper stmt >>= return . AlwaysC kw
        moduleItemMapper (Function ret name decls stmt) =
            fullMapper stmt >>= return . Function ret name decls
        moduleItemMapper other = return $ other
        fullMapper stmt = mapper stmt >>= cs
        cs (Block decls stmts) = mapM fullMapper stmts >>= return . Block decls
        cs (Case kw expr cases def) = do
            caseStmts <- mapM fullMapper $ map snd cases
            let cases' = zip (map fst cases) caseStmts
            def' <- if def == Nothing
                    then return Nothing
                    else fullMapper (fromJust def) >>= \x -> return $ Just x
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

traverseStmts :: Mapper Stmt -> Mapper ModuleItem
traverseStmts = unmonad traverseStmtsM
