{-# LANGUAGE FlexibleInstances #-}
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for any unpacked array which must be packed because it is: A) a
 - port; B) is bound to a port; C) is assigned a value in a single assignment;
 - or D) is assigned to an unpacked array which itself must be packed. The
 - conversion allows for an array to be partially packed if all flat usages of
 - the array explicitly specify some of the unpacked dimensions.
 -}

module Convert.UnpackedArray (convert) where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type Location = [Identifier]
type Locations = Map.Map Location Int
type ST = ScoperT Decl (State Locations)

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ (Part _ _ Module _ _ ports _)) =
    evalState (operation description) Map.empty
    where
        operation = partScoperT
            (traverseDeclM ports) traverseModuleItemM noop traverseStmtM >=>
            partScoperT rewriteDeclM noop noop noop
        noop = return
convertDescription other = other

-- tracks multi-dimensional unpacked array declarations
traverseDeclM :: [Identifier] -> Decl -> ST Decl
traverseDeclM _ (decl @ (Variable _ _ _ [] _)) = return decl
traverseDeclM ports (decl @ (Variable _ _ x _ e)) = do
    insertElem x decl
    if elem x ports || e /= Nil
        then flatUsageM x
        else return ()
    return decl
traverseDeclM ports decl @ Net{} =
    traverseNetAsVarM (traverseDeclM ports) decl
traverseDeclM _ other = return other

-- pack decls marked for packing
rewriteDeclM :: Decl -> ST Decl
rewriteDeclM (decl @ (Variable _ _ _ [] _)) = return decl
rewriteDeclM (decl @ (Variable d t x a e)) = do
    insertElem x decl
    details <- lookupElemM x
    let Just (accesses, _, _) = details
    let location = map accessName accesses
    usedAsPacked <- lift $ gets $ Map.lookup location
    case usedAsPacked of
        Just depth -> do
            let (tf, rs) = typeRanges t
            let (unpacked, packed) = splitAt depth a
            let t' = tf $ packed ++ rs
            return $ Variable d t' x unpacked e
        Nothing -> return decl
rewriteDeclM decl @ Net{} = traverseNetAsVarM rewriteDeclM decl
rewriteDeclM other = return other

traverseModuleItemM :: ModuleItem -> ST ModuleItem
traverseModuleItemM =
    traverseModuleItemM'
    >=> traverseLHSsM  traverseLHSM
    >=> traverseExprsM traverseExprM
    >=> traverseAsgnsM traverseAsgnM

traverseModuleItemM' :: ModuleItem -> ST ModuleItem
traverseModuleItemM' (Instance a b c d bindings) = do
    bindings' <- mapM collectBinding bindings
    return $ Instance a b c d bindings'
    where
        collectBinding :: PortBinding -> ST PortBinding
        collectBinding (y, x) = do
            flatUsageM x
            return (y, x)
traverseModuleItemM' other = return other

traverseStmtM :: Stmt -> ST Stmt
traverseStmtM =
    traverseStmtLHSsM  traverseLHSM  >=>
    traverseStmtExprsM traverseExprM >=>
    traverseStmtAsgnsM traverseAsgnM

traverseExprM :: Expr -> ST Expr
traverseExprM (Range x mode i) =
    flatUsageM x >> return (Range x mode i)
traverseExprM other = return other

traverseLHSM :: LHS -> ST LHS
traverseLHSM x = flatUsageM x >> return x

traverseAsgnM :: (LHS, Expr) -> ST (LHS, Expr)
traverseAsgnM (x, Mux cond y z) = do
    flatUsageM x
    flatUsageM y
    flatUsageM z
    return (x, Mux cond y z)
traverseAsgnM (x, y) = do
    flatUsageM x
    flatUsageM y
    return (x, y)

class ScopeKey t => Key t where
    unbit :: t -> (t, Int)

instance Key Expr where
    unbit (Bit e _) = (e', n + 1)
        where (e', n) = unbit e
    unbit (Range e _ _) = (e', n)
        where (e', n) = unbit e
    unbit e = (e, 0)

instance Key LHS where
    unbit (LHSBit e _) = (e', n + 1)
        where (e', n) = unbit e
    unbit (LHSRange e _ _) = (e', n)
        where (e', n) = unbit e
    unbit e = (e, 0)

instance Key Identifier where
    unbit x = (x, 0)

flatUsageM :: Key k => k -> ST ()
flatUsageM k = do
    let (k', depth) = unbit k
    details <- lookupElemM k'
    case details of
        Just (accesses, _, _) -> do
            let location = map accessName accesses
            lift $ modify $ Map.insertWith min location depth
        Nothing -> return ()
