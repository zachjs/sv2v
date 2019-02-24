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

import Data.Maybe (fromJust)
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
getStmtLHSs (Case kw e cases (Just stmt)) = (getStmtLHSs stmt) ++ (getStmtLHSs $ Case kw e cases Nothing)
getStmtLHSs (Case _  _ cases Nothing) = concat $ map getStmtLHSs $ map snd cases
getStmtLHSs (AsgnBlk lhs _) = [lhs]
getStmtLHSs (Asgn    lhs _) = [lhs]
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
convertModuleItem idents (MIDecl (Variable dir (Logic mr) ident a me)) =
    MIDecl $ Variable dir (t mr) ident a me
    where
        t = if Set.member ident idents then Reg else Wire
convertModuleItem idents (Generate items) = Generate $ map (convertGenItem $ convertModuleItem idents) items
convertModuleItem _ other = other

convertGenItem :: (ModuleItem -> ModuleItem) -> GenItem -> GenItem
convertGenItem f item = convertGenItem' item
    where
        convertGenItem' :: GenItem -> GenItem
        convertGenItem' (GenBlock x items) = GenBlock x $ map convertGenItem' items
        convertGenItem' (GenFor a b c d items) = GenFor a b c d $ map convertGenItem' items
        convertGenItem' (GenIf e i1 i2) = GenIf e (convertGenItem' i1) (convertGenItem' i2)
        convertGenItem' (GenNull) = GenNull
        convertGenItem' (GenModuleItem moduleItem) = GenModuleItem $ f moduleItem
        convertGenItem' (GenCase e cases def) = GenCase e cases' def'
            where
                cases' = zip (map fst cases) (map (convertGenItem' . snd) cases)
                def' = if def == Nothing
                    then Nothing
                    else Just $ convertGenItem' $ fromJust def

