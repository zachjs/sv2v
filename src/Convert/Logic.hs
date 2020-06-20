{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion from `logic` to `wire` or `reg`
 -
 - We convert a module-level logic to a reg if it is assigned to in an always or
 - initial block. Other module-level logics become wires. All other logics
 - (i.e., in a function) become regs.
 -
 - Parameters and localparams with integer vector types become implicit.
 -
 - The struct conversion and Verilog-2005's lack of permissive net vs. variable
 - resolution leads to some interesting special cases for this conversion, as
 - parts of a struct may be used as a variable, while other parts may be used as
 - a net.
 -
 - 1) If a reg, or a portion thereof, is assigned by a continuous assignment
 - item, then that assignment is converted to a procedural assignment within an
 - added `always_comb` item.
 -
 - 2) If a reg, or a portion thereof, is bound to an output port, then that
 - binding is replaced by a temporary net declaration, and a procedural
 - assignment is added which updates the reg to the value of the new net.
 -}

module Convert.Logic (convert) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type Idents = Set.Set Identifier
type Ports = Map.Map Identifier [(Identifier, Direction)]

convert :: [AST] -> [AST]
convert =
    traverseFiles
        (collectDescriptionsM collectPortsM)
        (traverseDescriptions . convertDescription)
    where
        collectPortsM :: Description -> Writer Ports ()
        collectPortsM (orig @ (Part _ _ _ _ name portNames _)) =
            tell $ Map.singleton name ports
            where
                ports = zip portNames (map lookupDir portNames)
                dirs = execWriter $ collectModuleItemsM collectDeclDirsM orig
                lookupDir :: Identifier -> Direction
                lookupDir portName =
                    case lookup portName dirs of
                        Just dir -> dir
                        Nothing -> error $ "Could not find dir for port " ++
                                        portName ++ " in module " ++ name
        collectPortsM _ = return ()
        collectDeclDirsM :: ModuleItem -> Writer [(Identifier, Direction)] ()
        collectDeclDirsM (MIPackageItem (Decl (Variable dir t ident _ _))) =
            case (dir, t) of
                (_, InterfaceT{}) -> tell [(ident, Local)]
                (Local, _) -> return ()
                _ -> tell [(ident, dir)]
        collectDeclDirsM _ = return ()

convertDescription :: Ports -> Description -> Description
convertDescription ports orig =
    if shouldConvert
        then converted
        else orig
    where
        shouldConvert = case orig of
            Part _ _ Interface _ _ _ _ -> False
            Part _ _ Module _ _ _ _ -> True
            PackageItem _ -> True
            Package _ _ _ -> False

        origIdents = execWriter (collectModuleItemsM regIdents orig)
        fixed = traverseModuleItems fixModuleItem orig
        fixedIdents = execWriter (collectModuleItemsM regIdents fixed)
        conversion = traverseDecls convertDecl . convertModuleItem
        converted = traverseModuleItems conversion fixed

        fixModuleItem :: ModuleItem -> ModuleItem
        -- rewrite bad continuous assignments to use procedural assignments
        fixModuleItem (Assign AssignOptionNone lhs expr) =
            if Set.disjoint usedIdents origIdents
                then Assign AssignOptionNone lhs expr
                else AlwaysC AlwaysComb $ Asgn AsgnOpEq Nothing lhs expr
            where
                usedIdents = execWriter $ collectNestedLHSsM lhsIdents lhs
        -- rewrite port bindings to use temporary nets where necessary
        fixModuleItem (Instance moduleName params instanceName rs bindings) =
            if null newItems
                then Instance moduleName params instanceName rs bindings
                else Generate $ map GenModuleItem $
                    comment : newItems ++
                    [Instance moduleName params instanceName rs bindings']
            where
                comment = MIPackageItem $ Decl $ CommentDecl
                    "rewrote reg-to-output bindings"
                (bindings', newItemsList) =
                    unzip $ map (uncurry fixBinding) $ zip bindings [0..]
                newItems = concat newItemsList
                fixBinding :: PortBinding -> Int -> (PortBinding, [ModuleItem])
                fixBinding (portName, expr) portIdx =
                    if portDir /= Just Output || Set.disjoint usedIdents origIdents
                        then ((portName, expr), [])
                        else ((portName, tmpExpr), items)
                    where
                        portDir = lookupPortDir portName portIdx
                        usedIdents = execWriter $
                            collectNestedExprsM exprIdents expr
                        tmp = "sv2v_tmp_" ++ instanceName ++ "_" ++ portName
                        tmpExpr = Ident tmp
                        t = Net (NetType TWire) Unspecified
                                [(DimsFn FnBits $ Right expr, Number "1")]
                        items =
                            [ MIPackageItem $ Decl $ Variable Local t tmp [] Nil
                            , AlwaysC AlwaysComb $ Asgn AsgnOpEq Nothing lhs tmpExpr]
                        lhs = case exprToLHS expr of
                            Just l -> l
                            Nothing ->
                                error $ "bad non-lhs, non-net expr "
                                    ++ show expr ++ " connected to output port "
                                    ++ portName ++ " of " ++ instanceName
                lookupPortDir :: Identifier -> Int -> Maybe Direction
                lookupPortDir "" portIdx =
                    case Map.lookup moduleName ports of
                        Nothing -> Nothing
                        Just l -> if portIdx >= length l
                                    then Nothing
                                    else Just $ snd $ l !! portIdx
                lookupPortDir portName _ =
                    case Map.lookup moduleName ports of
                        Nothing -> Nothing
                        Just l -> lookup portName l
        fixModuleItem other = other

        -- rewrite variable declarations to have the correct type
        convertModuleItem (MIPackageItem (Decl (Variable dir (IntegerVector _ sg mr) ident a e))) =
            MIPackageItem $ Decl $ Variable dir' (t mr) ident a e
            where
                t = if Set.member ident fixedIdents
                    then IntegerVector TReg sg
                    else Net (NetType TWire) sg
                dir' =
                    if dir == Inout && Set.member ident fixedIdents
                    then Output
                    else dir
        convertModuleItem other = other
        -- all other logics (i.e. inside of functions) become regs
        convertDecl :: Decl -> Decl
        convertDecl (Param s (IntegerVector _ sg []) x e) =
            Param s (Implicit sg [(Number "0", Number "0")]) x e
        convertDecl (Param s (IntegerVector _ sg rs) x e) =
            Param s (Implicit sg rs) x e
        convertDecl (Variable d (IntegerVector TLogic sg rs) x a e) =
            Variable d (IntegerVector TReg sg rs) x a e
        convertDecl other = other

regIdents :: ModuleItem -> Writer Idents ()
regIdents (item @ AlwaysC{}) = regIdents' item
regIdents (item @ Initial{}) = regIdents' item
regIdents (item @ Final{})   = regIdents' item
regIdents _ = return ()

regIdents' :: ModuleItem -> Writer Idents ()
regIdents' item = do
    let write = traverseScopesM traverseDeclM return traverseStmtM item
    leftovers <- execStateT write Set.empty
    if Set.null leftovers
        then return ()
        else error $ "regIdents' got leftovers: " ++ show leftovers

traverseDeclM :: Monad m => Decl -> StateT Idents m Decl
traverseDeclM (decl @ (Variable _ _ x _ _)) =
    modify (Set.insert x) >> return decl
traverseDeclM decl = return decl

traverseStmtM :: Stmt -> StateT Idents (Writer Idents) Stmt
traverseStmtM (Timing _ stmt) = traverseStmtM stmt
traverseStmtM (Subroutine (Ident f) args) = do
    case args of
        Args [_, Ident x, _] [] ->
            -- assuming that no one will readmem into a local variable
            if f == "$readmemh" || f == "$readmemb"
                then lift $ tell $ Set.singleton x
                else return ()
        _ -> return ()
    return $ Subroutine (Ident f) args
traverseStmtM stmt = do
    -- only write down idents which aren't shadowed
    let regs = execWriter $ collectStmtLHSsM (collectNestedLHSsM lhsIdents) stmt
    locals <- get
    let globals = Set.difference regs locals
    lift $ tell globals
    return stmt

lhsIdents :: LHS -> Writer Idents ()
lhsIdents (LHSIdent x) = tell $ Set.singleton x
lhsIdents _ = return () -- the collector recurses for us

exprIdents :: Expr -> Writer Idents ()
exprIdents (Ident x) = tell $ Set.singleton x
exprIdents _ = return () -- the collector recurses for us
