{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - SystemVerilog to Verilog conversion
 -}

-- XXX: Note that, for now, we're going to shove the first few conversions all
-- into this same file, more or less. As patterns start becoming clearer, we
-- should be making helpers to make traversing the AST much easier.

-- Regarding `logic` conversion: The SystemVerilog grammar has the concept of a
-- `data_declaration`, which seems to cover things much more generally. While
-- obviously `logic` can appear as module items or ports, they can also be
-- function arguments, for example.

module Convert (convert) where

--import Data.List
import Text.Printf
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Language.SystemVerilog.AST

data Possibility = AsWire | AsReg deriving (Eq, Ord)
type Possibilities = Set.Set Possibility
type TypeInfo = Map.Map String Possibilities
type ModulePorts = Map.Map String [Direction]

-- TODO: It now seems like logic only becomes reg if it is assigned to in an
-- always block. If this is true, it obviously could dramatically simplify this
-- initial transformation.

convert :: [Module] -> [Module]
convert modules = map (convertModule portsInfo) modules
    where
        portsInfo = Map.fromList $ map getPorts modules
        getPorts :: Module -> (Identifier, [Direction])
        getPorts (Module name ports items) =
            let portDirMap = Map.fromList $ mapMaybe getDirection items
            in (name, map (portDirMap Map.!) ports)
            where
                getDirection :: ModuleItem -> Maybe (Identifier, Direction)
                getDirection (PortDecl dir _ ident) = Just (ident, dir)
                getDirection _ = Nothing

convertModule :: ModulePorts -> Module -> Module
convertModule modules (Module name ports items) =
    Module name ports (map (convertModuleItem info) items)
    where
        info = foldr (Map.unionWith Set.intersection) Map.empty (map (getTypeInfoModuleItem modules) items)

getLHSIdentifiers :: LHS -> [Identifier]
getLHSIdentifiers (LHS       vx  ) = [vx]
getLHSIdentifiers (LHSBit    vx _) = [vx]
getLHSIdentifiers (LHSRange  vx _) = [vx]
getLHSIdentifiers (LHSConcat lhss) = concat $ map getLHSIdentifiers lhss

onlyAsWire :: Possibilities
onlyAsWire = Set.fromList [AsWire]
onlyAsReg  :: Possibilities
onlyAsReg  = Set.fromList [AsReg]
asEither   :: Possibilities
asEither   = Set.fromList [AsWire, AsReg]

getStmtLHSs :: Stmt -> [LHS]
getStmtLHSs (Block _ stmts) = concat $ map getStmtLHSs stmts
getStmtLHSs (Case e cases (Just stmt)) = (getStmtLHSs stmt) ++ (getStmtLHSs $ Case e cases Nothing)
getStmtLHSs (Case _ cases Nothing) = concat $ map getStmtLHSs $ map snd cases
getStmtLHSs (BlockingAssignment    lhs _) = [lhs]
getStmtLHSs (NonBlockingAssignment lhs _) = [lhs]
getStmtLHSs (For _ _ _ stmt) = getStmtLHSs stmt
getStmtLHSs (If _ s1 s2) = (getStmtLHSs s1) ++ (getStmtLHSs s2)
getStmtLHSs (Null) = []

getTypeInfoModuleItem :: ModulePorts -> ModuleItem -> TypeInfo
getTypeInfoModuleItem _ (Assign lhs _) =
    Map.fromList $ zip (getLHSIdentifiers lhs) (repeat onlyAsWire)
getTypeInfoModuleItem _ (Always _ stmt) =
    Map.fromList $ zip idents (repeat onlyAsReg)
    where
        lhss = getStmtLHSs stmt
        idents = concat $ map getLHSIdentifiers lhss
--getTypeInfoModuleItem modules (Instance name _ _ bindings) =
--    case Map.lookup name modules of
--        Nothing -> Map.empty
--        Just dirs ->
--
--            where
--                isDirect :: PortBinding -> Bool
--                isDirect x = snd x == Nothing
--                directs = map fst $ filter isDirect bindings
--getTypeInfoModuleItem _ (Function   (Maybe FuncRet) Identifier [(Bool, BlockItemDeclaration)] Stmt
--getTypeInfoModuleItem _ (Generate   [GenItem]
getTypeInfoModuleItem _ _ = Map.empty

convertModuleItem :: TypeInfo -> ModuleItem -> ModuleItem
convertModuleItem info (LocalNet (Logic mr) ident val) =
    LocalNet (t mr) ident val
    where
        t = case Map.lookup ident info of
            Nothing -> Wire
            Just possibilities ->
                if Set.member AsWire possibilities then Wire
                else if Set.member AsReg possibilities then Reg
                else error $ printf "item %s has not possibilities" ident
convertModuleItem _ other = other
