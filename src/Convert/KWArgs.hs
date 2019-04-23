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
import Data.Maybe (mapMaybe)
import Control.Monad.Writer
import qualified Data.Map.Strict as Map

import Convert.Traverse
import Language.SystemVerilog.AST

type TFs = Map.Map Identifier [Identifier]

convert :: AST -> AST
convert = traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription description =
    traverseModuleItems
        (traverseExprs $ traverseNestedExprs $ convertExpr tfs)
        description
    where
        tfs = execWriter $ collectModuleItemsM collectTF description

collectTF :: ModuleItem -> Writer TFs ()
collectTF (MIPackageItem (Function _ _ f decls _)) = collectTFDecls f decls
collectTF (MIPackageItem (Task     _   f decls _)) = collectTFDecls f decls
collectTF _ = return ()

collectTFDecls :: Identifier -> [Decl] -> Writer TFs ()
collectTFDecls name decls =
    tell $ Map.singleton name $ mapMaybe getInput decls
    where
        getInput :: Decl -> Maybe Identifier
        getInput (Variable Input _ ident _ _) = Just ident
        getInput _ = Nothing

convertExpr :: TFs -> Expr -> Expr
convertExpr _ (orig @ (Call _ (Args _ []))) = orig
convertExpr tfs (Call func (Args pnArgs kwArgs)) =
    case tfs Map.!? func of
        Nothing -> Call func (Args pnArgs kwArgs)
        Just ordered -> Call func (Args args [])
            where
                args = pnArgs ++ (map snd $ sortOn position kwArgs)
                position (x, _) = elemIndex x ordered
convertExpr _ other = other
