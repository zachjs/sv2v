{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for any unpacked array which must be packed because it is: A) a
 - port; B) is bound to a port; C) is assigned a value in a single assignment;
 - or D) is assigned to an unpacked array which itself must be packed.
 -
 - The scoped nature of declarations makes this challenging. While scoping is
 - obeyed in general, if any of a set of *equivalent* declarations within a
 - module is packed, all of the declarations are packed. This is because we only
 - record the declaration that needs to be packed when a relevant usage is
 - encountered.
 -}

module Convert.UnpackedArray (convert) where

import Control.Monad.State.Strict
import qualified Data.Set as Set

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type Location = [Identifier]
type Locations = Set.Set Location
type ST = ScoperT Decl (State Locations)

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ (Part _ _ Module _ _ _ _)) =
    evalState (operation description) Set.empty
    where
        operation =
            partScoperT traverseDeclM traverseModuleItemM noop traverseStmtM >=>
            partScoperT rewriteDeclM noop noop noop
        noop = return
convertDescription other = other

-- tracks multi-dimensional unpacked array declarations
traverseDeclM :: Decl -> ST Decl
traverseDeclM (decl @ (Variable _ _ _ [] _)) = return decl
traverseDeclM (decl @ (Variable dir _ x _ e)) = do
    insertElem x decl
    if dir /= Local || e /= Nil
        then flatUsageM x
        else return ()
    return decl
traverseDeclM other = return other

-- pack decls marked for packing
rewriteDeclM :: Decl -> ST Decl
rewriteDeclM (decl @ (Variable _ _ _ [] _)) = return decl
rewriteDeclM (decl @ (Variable d t x a e)) = do
    insertElem x decl
    details <- lookupElemM x
    let Just (accesses, _, _) = details
    let location = map accessName accesses
    usedAsPacked <- lift $ gets $ Set.member location
    if usedAsPacked
        then do
            let (tf, rs) = typeRanges t
            let t' = tf $ a ++ rs
            return $ Variable d t' x [] e
        else return decl
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

flatUsageM :: ScopeKey e => e -> ST ()
flatUsageM x = do
    details <- lookupElemM x
    case details of
        Just (accesses, _, _) -> do
            let location = map accessName accesses
            lift $ modify $ Set.insert location
        Nothing -> return ()
