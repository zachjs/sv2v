{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Verilog-2005 requires that all functions have at least one input port.
 - SystemVerilog allows functions to have no arguments. This conversion adds a
 - dummy argument to such functions.
 -}

module Convert.EmptyArgs (convert) where

import Control.Monad.Writer.Strict
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type Idents = Set.Set Identifier

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ Part{}) =
    traverseModuleItems
        (traverseExprs $ traverseNestedExprs $ convertExpr functions)
        description'
    where
        (description', functions) =
            runWriter $ traverseModuleItemsM traverseFunctionsM description
convertDescription other = other

traverseFunctionsM :: ModuleItem -> Writer Idents ModuleItem
traverseFunctionsM (MIPackageItem (Function ml t f decls stmts)) = do
    decls' <-
        if any isInput decls
            then return decls
            else do
                tell $ Set.singleton f
                return $ dummyDecl : decls
    return $ MIPackageItem $ Function ml t f decls' stmts
    where
        dummyType = IntegerVector TReg Unspecified []
        dummyDecl = Variable Input dummyType "_sv2v_unused" [] Nil
        isInput :: Decl -> Bool
        isInput (Variable Input _ _ _ _) = True
        isInput _ = False
traverseFunctionsM other = return other

convertExpr :: Idents -> Expr -> Expr
convertExpr functions (Call (Ident func) (Args [] [])) =
    Call (Ident func) (Args args [])
    where args = if Set.member func functions
            then [RawNum 0]
            else []
convertExpr _ other = other
