{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - SystemVerilog allows functions to be called without using their result. For
 - example, if `f` is a function, one may write `f();` or `void'(f());`, causing
 - any side effects of `f` to occur in each case. Verilog-2005 does not allow
 - functions to be called as though they were tasks in this way. This conversion
 - creates a dummy variable to store the result of the function.
 -}

module Convert.FuncRoutine (convert) where

import Control.Monad.Writer.Strict
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type Idents = Set.Set Identifier

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (description @ Part{}) =
    traverseModuleItems (traverseStmts $ convertStmt functions) description
    where functions = execWriter $
            collectModuleItemsM collectFunctionsM description
convertDescription other = other

collectFunctionsM :: ModuleItem -> Writer Idents ()
collectFunctionsM (MIPackageItem (Function _ _ f _ _)) =
    tell $ Set.singleton f
collectFunctionsM _ = return ()

convertStmt :: Idents -> Stmt -> Stmt
convertStmt functions (Subroutine (Ident func) args) =
    if Set.member func functions
        then Block Seq "" [decl] []
        else Subroutine (Ident func) args
    where
        t = TypeOf e
        e = Call (Ident func) args
        decl = Variable Local t "sv2v_void" [] e
convertStmt _ other = other
