{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for any unpacked array which must be packed because it is: A) a
 - port; B) is bound to a port; C) is assigned a value in a single assignment;
 - or D) is assigned to an unpacked array which itself mus be packed.
 -
 - The scoped nature of declarations makes this challenging. While scoping is
 - obeyed in general, any of a set of *equivalent* declarations within a module
 - is packed, all of the declarations are packed. This is because we only record
 - the declaration that needs to be packed when a relevant usage is encountered.
 -}

module Convert.UnpackedArray (convert) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type DeclMap = Map.Map Identifier Decl
type DeclSet = Set.Set Decl

type ST = StateT DeclMap (Writer DeclSet)

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription description =
    traverseModuleItems (traverseDecls $ packDecl declsToPack) description'
    where
        (description', declsToPack) = runWriter $
            scopedConversionM traverseDeclM traverseModuleItemM traverseStmtM
            Map.empty description

-- collects and converts multi-dimensional packed-array declarations
traverseDeclM :: Decl -> ST Decl
traverseDeclM (orig @ (Variable dir _ x _ me)) = do
    modify $ Map.insert x orig
    () <- if dir /= Local || me /= Nothing
        then lift $ tell $ Set.singleton orig
        else return ()
    return orig
traverseDeclM other = return other

-- pack the given decls marked for packing
packDecl :: DeclSet -> Decl -> Decl
packDecl decls (orig @ (Variable d t x a me)) = do
    if Set.member orig decls
        then do
            let (tf, rs) = typeRanges t
            let t' = tf $ a ++ rs
            Variable d t' x [] me
        else orig
packDecl _ other = other


traverseModuleItemM :: ModuleItem -> ST ModuleItem
traverseModuleItemM item =
    traverseModuleItemM' item
    >>= traverseLHSsM  traverseLHSM
    >>= traverseExprsM traverseExprM
    >>= traverseAsgnsM traverseAsgnM

traverseModuleItemM' :: ModuleItem -> ST ModuleItem
traverseModuleItemM' (Instance a b c d bindings) = do
    bindings' <- mapM collectBinding bindings
    return $ Instance a b c d bindings'
    where
        collectBinding :: PortBinding -> ST PortBinding
        collectBinding (y, Just (Ident x)) = do
            flatUsageM x
            return (y, Just (Ident x))
        collectBinding other = return other
traverseModuleItemM' other = return other

traverseStmtM :: Stmt -> ST Stmt
traverseStmtM stmt =
    traverseStmtLHSsM  traverseLHSM  stmt >>=
    traverseStmtExprsM traverseExprM >>=
    traverseStmtAsgnsM traverseAsgnM

traverseExprM :: Expr -> ST Expr
traverseExprM (Range (Ident x) mode i) = do
    flatUsageM x
    return $ Range (Ident x) mode i
traverseExprM other = return other

traverseLHSM :: LHS -> ST LHS
traverseLHSM (LHSIdent x) = do
    flatUsageM x
    return $ LHSIdent x
traverseLHSM other = return other

traverseAsgnM :: (LHS, Expr) -> ST (LHS, Expr)
traverseAsgnM (LHSIdent x, Ident y) = do
    flatUsageM x
    flatUsageM y
    return (LHSIdent x, Ident y)
traverseAsgnM other = return other

flatUsageM :: Identifier -> ST ()
flatUsageM x = do
    declMap <- get
    case Map.lookup x declMap of
        Just decl -> lift $ tell $ Set.singleton decl
        Nothing -> return ()
