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

import qualified Data.Set as Set

import Language.SystemVerilog.AST

type RegIdents = Set.Set String

convert :: AST -> AST
convert descriptions = map convertDescription descriptions

convertDescription :: Description -> Description
convertDescription (Module name ports items) =
    Module name ports $ map (convertModuleItem idents) items
    where
        idents = Set.unions $ map getRegIdents items
convertDescription other = other

getStmtLHSs :: Stmt -> [LHS]
getStmtLHSs (Block _ stmts) = concat $ map getStmtLHSs stmts
getStmtLHSs (Case e cases (Just stmt)) = (getStmtLHSs stmt) ++ (getStmtLHSs $ Case e cases Nothing)
getStmtLHSs (Case _ cases Nothing) = concat $ map getStmtLHSs $ map snd cases
getStmtLHSs (BlockingAssignment    lhs _) = [lhs]
getStmtLHSs (NonBlockingAssignment lhs _) = [lhs]
getStmtLHSs (For _ _ _ stmt) = getStmtLHSs stmt
getStmtLHSs (If _ s1 s2) = (getStmtLHSs s1) ++ (getStmtLHSs s2)
getStmtLHSs (Timing _ s) = getStmtLHSs s
getStmtLHSs (Null) = []

getLHSIdents :: LHS -> [Identifier]
getLHSIdents (LHS       vx  ) = [vx]
getLHSIdents (LHSBit    vx _) = [vx]
getLHSIdents (LHSRange  vx _) = [vx]
getLHSIdents (LHSConcat lhss) = concat $ map getLHSIdents lhss

getRegIdents :: ModuleItem -> RegIdents
getRegIdents (AlwaysC _ stmt) =
    Set.fromList idents
    where
        lhss = getStmtLHSs stmt
        idents = concat $ map getLHSIdents lhss
getRegIdents _ = Set.empty

convertModuleItem :: RegIdents -> ModuleItem -> ModuleItem
convertModuleItem idents (LocalNet (Logic mr) ident val) =
    LocalNet (t mr) ident val
    where
        t = if Set.member ident idents then Reg else Wire
convertModuleItem _ other = other
