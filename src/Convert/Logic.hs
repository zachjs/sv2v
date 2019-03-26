{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `logic`
 -}

-- Regarding `logic` conversion: The SystemVerilog grammar has the concept of a
-- `data_declaration`, which seems to cover things much more generally. While
-- obviously `logic` can appear as module items or ports, they can also be
-- function arguments, for example.

-- It seems like logic only becomes reg if it is assigned to in an always block.

module Convert.Logic (convert) where

import Control.Monad.Writer
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type RegIdents = Set.Set String

convert :: AST -> AST
convert = traverseDescriptions convertDescription

convertDescription :: Description -> Description
convertDescription (orig @ (Part _ Module _ _ _ _)) =
    traverseModuleItems (traverseDecls convertDecl . convertModuleItem) orig
    where
        idents = execWriter (collectModuleItemsM regIdents orig)
        convertModuleItem :: ModuleItem -> ModuleItem
        convertModuleItem (MIDecl (Variable dir (IntegerVector TLogic sg mr) ident a me)) =
            MIDecl $ Variable dir (t mr) ident a me
            where
                t = if sg /= Unspecified || Set.member ident idents
                    then IntegerVector TReg sg
                    else Net TWire
        convertModuleItem other = other
        -- all other logics (i.e. inside of functions) become regs
        convertDecl :: Decl -> Decl
        convertDecl (Variable d (IntegerVector TLogic sg rs) x a me) =
            Variable d (IntegerVector TReg sg rs) x a me
        convertDecl other = other
convertDescription other = other

regIdents :: ModuleItem -> Writer RegIdents ()
regIdents (AlwaysC _ stmt) =
    collectStmtLHSsM (collectNestedLHSsM idents) $
    traverseNestedStmts removeTimings stmt
    where
        idents :: LHS -> Writer RegIdents ()
        idents (LHSIdent  vx  ) = tell $ Set.singleton vx
        idents _ = return () -- the collector recurses for us
        removeTimings :: Stmt -> Stmt
        removeTimings (Timing _ s) = s
        removeTimings other = other
regIdents _ = return ()
