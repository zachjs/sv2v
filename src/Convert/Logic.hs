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
convertDescription orig =
    traverseModuleItems convertModuleItem orig
    where
        idents = execWriter (collectModuleItemsM regIdents orig)
        convertModuleItem :: ModuleItem -> ModuleItem
        convertModuleItem (MIDecl (Variable dir (Logic mr) ident a me)) =
            MIDecl $ Variable dir (t mr) ident a me
            where t = if Set.member ident idents then Reg else Wire
        convertModuleItem other = other

regIdents :: ModuleItem -> Writer RegIdents ()
regIdents (AlwaysC _ stmt) = collectStmtLHSsM idents stmt
    where
        idents :: LHS -> Writer RegIdents ()
        idents (LHS       vx  ) = tell $ Set.singleton vx
        idents (LHSBit    vx _) = tell $ Set.singleton vx
        idents (LHSRange  vx _) = tell $ Set.singleton vx
        idents (LHSConcat lhss) = mapM idents lhss >>= \_ -> return ()
regIdents _ = return ()
