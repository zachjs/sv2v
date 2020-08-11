{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for named function and task arguments
 -
 - This conversion takes the named arguments and moves them into their
 - corresponding position in the argument list, with names removed.
 -}

module Convert.KWArgs (convert) where

import Data.List (elemIndex, sortOn)
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type TFs = Map.Map Identifier [Identifier]

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription description =
    traverseModuleItems (convertModuleItem tfs) description
    where tfs = execWriter $ collectModuleItemsM collectTF description

convertModuleItem :: TFs -> ModuleItem -> ModuleItem
convertModuleItem tfs =
    (traverseExprs $ traverseNestedExprs $ convertExpr tfs) .
    (traverseStmts $ convertStmt tfs)

collectTF :: ModuleItem -> Writer TFs ()
collectTF (MIPackageItem (Function _ _ f decls _)) = collectTFDecls f decls
collectTF (MIPackageItem (Task     _   f decls _)) = collectTFDecls f decls
collectTF _ = return ()

collectTFDecls :: Identifier -> [Decl] -> Writer TFs ()
collectTFDecls name decls =
    tell $ Map.singleton name $ filter (not . null) $ map getInput decls
    where
        getInput :: Decl -> Identifier
        getInput (Variable Input _ ident _ _) = ident
        getInput _ = ""

convertExpr :: TFs -> Expr -> Expr
convertExpr tfs (Call expr args) =
    convertInvoke tfs Call expr args
convertExpr _ other = other

convertStmt :: TFs -> Stmt -> Stmt
convertStmt tfs (Subroutine expr args) =
    convertInvoke tfs Subroutine expr args
convertStmt _ other = other

convertInvoke :: TFs -> (Expr -> Args -> a) -> Expr -> Args -> a
convertInvoke tfs constructor (Ident func) (Args pnArgs (kwArgs @ (_ : _))) =
    case tfs Map.!? func of
        Nothing -> constructor (Ident func) (Args pnArgs kwArgs)
        Just ordered -> constructor (Ident func) (Args args [])
            where
                args = pnArgs ++ (map snd $ sortOn position kwArgs)
                position (x, _) = elemIndex x ordered
convertInvoke _ constructor expr args =
    constructor expr args
