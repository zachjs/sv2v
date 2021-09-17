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
type ST = ScoperT () (State Locations)

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription description@(Part _ _ Module _ _ ports _) =
    evalScoper $ scopePart conScoper description
    where
        locations = execState
            (evalScoperT $ scopePart locScoper description) Map.empty
        locScoper = scopeModuleItem
            (traverseDeclM ports) traverseModuleItemM return traverseStmtM
        conScoper = scopeModuleItem
            (rewriteDeclM locations) return return return
convertDescription other = other

-- tracks multi-dimensional unpacked array declarations
traverseDeclM :: [Identifier] -> Decl -> ST Decl
traverseDeclM _ decl@(Variable _ _ _ [] e) =
    traverseExprArgsM e >> return decl
traverseDeclM ports decl@(Variable dir _ x _ e) = do
    insertElem x ()
    when (dir /= Local || elem x ports || e /= Nil) $
        flatUsageM x
    traverseExprArgsM e >> return decl
traverseDeclM ports decl@Net{} =
    traverseNetAsVarM (traverseDeclM ports) decl
traverseDeclM _ other = return other

-- pack decls marked for packing
rewriteDeclM :: Locations -> Decl -> Scoper () Decl
rewriteDeclM _ decl@(Variable _ _ _ [] _) = return decl
rewriteDeclM locations decl@(Variable d t x a e) = do
    accesses <- localAccessesM x
    let location = map accessName accesses
    case Map.lookup location locations of
        Just depth -> do
            let (tf, rs) = typeRanges t
            let (unpacked, packed) = splitAt depth a
            let t' = tf $ packed ++ rs
            return $ Variable d t' x unpacked e
        Nothing -> return decl
rewriteDeclM locations decl@Net{} =
    traverseNetAsVarM (rewriteDeclM locations) decl
rewriteDeclM _ other = return other

traverseModuleItemM :: ModuleItem -> ST ModuleItem
traverseModuleItemM item@(Instance _ _ _ _ bindings) =
    mapM_ (flatUsageM . snd) bindings >> return item
traverseModuleItemM item =
    traverseLHSsM traverseLHSM item
    >>= traverseExprsM traverseExprM
    >>= traverseAsgnsM traverseAsgnM

traverseStmtM :: Stmt -> ST Stmt
traverseStmtM =
    traverseStmtLHSsM  traverseLHSM  >=>
    traverseStmtExprsM traverseExprM >=>
    traverseStmtAsgnsM traverseAsgnM >=>
    traverseStmtArgsM

traverseStmtArgsM :: Stmt -> ST Stmt
traverseStmtArgsM stmt@(Subroutine (Ident ('$' : _)) _) =
    return stmt
traverseStmtArgsM stmt@(Subroutine _ (Args args [])) =
    mapM_ flatUsageM args >> return stmt
traverseStmtArgsM stmt = return stmt

traverseExprM :: Expr -> ST Expr
traverseExprM (Range x mode i) =
    flatUsageM x >> return (Range x mode i)
traverseExprM expr = traverseExprArgsM expr

traverseExprArgsM :: Expr -> ST Expr
traverseExprArgsM expr@(Call _ (Args args [])) =
    mapM_ (traverseExprArgsM >=> flatUsageM) args >> return expr
traverseExprArgsM expr =
    traverseSinglyNestedExprsM traverseExprArgsM expr

traverseLHSM :: LHS -> ST LHS
traverseLHSM x = flatUsageM x >> return x

traverseAsgnM :: (LHS, Expr) -> ST (LHS, Expr)
traverseAsgnM (x, y) = do
    flatUsageM x
    flatUsageM y
    return (x, y)

class ScopeKey t => Key t where
    unbit :: t -> (t, Int)
    split :: t -> Maybe (t, t)
    split = const Nothing

instance Key Expr where
    unbit (Bit e _) = (e', n + 1)
        where (e', n) = unbit e
    unbit (Range e _ _) = (e', n)
        where (e', n) = unbit e
    unbit e = (e, 0)
    split (Mux _ a b) = Just (a, b)
    split _ = Nothing

instance Key LHS where
    unbit (LHSBit e _) = (e', n + 1)
        where (e', n) = unbit e
    unbit (LHSRange e _ _) = (e', n)
        where (e', n) = unbit e
    unbit e = (e, 0)

instance Key Identifier where
    unbit x = (x, 0)

flatUsageM :: Key k => k -> ST ()
flatUsageM k | Just (a, b) <- split k =
    flatUsageM a >> flatUsageM b
flatUsageM k = do
    let (k', depth) = unbit k
    details <- lookupElemM k'
    case details of
        Just (accesses, _, ()) -> do
            let location = map accessName accesses
            lift $ modify $ Map.insertWith min location depth
        Nothing -> return ()
