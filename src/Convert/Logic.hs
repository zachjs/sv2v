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

import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Scoper
import Convert.Traverse
import Language.SystemVerilog.AST

type Ports = Map.Map Identifier [(Identifier, Direction)]
type Location = [Identifier]
type Locations = Set.Set Location
type ST = ScoperT Type (Writer Locations)

convert :: [AST] -> [AST]
convert =
    traverseFiles
        (collectDescriptionsM collectPortsM)
        (traverseDescriptions . convertDescription)
    where
        collectPortsM :: Description -> Writer Ports ()
        collectPortsM orig@(Part _ _ _ _ name portNames _) =
            tell $ Map.singleton name ports
            where
                ports = zip portNames (map lookupDir portNames)
                dirs = execWriter $ collectModuleItemsM collectDeclDirsM orig
                lookupDir :: Identifier -> Direction
                lookupDir portName =
                    case lookup portName dirs of
                        Just dir -> dir
                        Nothing -> Inout
        collectPortsM _ = return ()
        collectDeclDirsM :: ModuleItem -> Writer [(Identifier, Direction)] ()
        collectDeclDirsM (MIPackageItem (Decl (Variable dir _ ident _ _))) =
            when (dir /= Local) $ tell [(ident, dir)]
        collectDeclDirsM (MIPackageItem (Decl (Net dir _ _ _ ident _ _))) =
            when (dir /= Local) $ tell [(ident, dir)]
        collectDeclDirsM _ = return ()

convertDescription :: Ports -> Description -> Description
convertDescription ports description =
    evalScoper $ scopeModule conScoper description
    where
        locations = execWriter $ evalScoperT $ scopePart locScoper description
        -- write down which vars are procedurally assigned
        locScoper = scopeModuleItem traverseDeclM return return traverseStmtM
        -- rewrite reg continuous assignments and output port connections
        conScoper = scopeModuleItem
            (rewriteDeclM locations) (traverseModuleItemM ports) return return

traverseModuleItemM :: Ports -> ModuleItem -> Scoper Type ModuleItem
traverseModuleItemM ports = embedScopes $ traverseModuleItem ports

traverseModuleItem :: Ports -> Scopes Type -> ModuleItem -> ModuleItem
traverseModuleItem ports scopes =
    fixModuleItem
    where
        isReg :: LHS -> Bool
        isReg =
            or . execWriter . collectNestedLHSsM isReg'
            where
                isRegType :: Type -> Bool
                isRegType (IntegerVector TReg _ _) = True
                isRegType _ = False
                isReg' :: LHS -> Writer [Bool] ()
                isReg' lhs =
                    case lookupElem scopes lhs of
                        Just (_, _, t) -> tell [isRegType t]
                        _ -> tell [False]

        always_comb = AlwaysC Always . Timing (Event EventStar)

        fixModuleItem :: ModuleItem -> ModuleItem
        -- rewrite bad continuous assignments to use procedural assignments
        fixModuleItem (Assign AssignOptionNone lhs expr) =
            if not (isReg lhs)
                then Assign AssignOptionNone lhs expr
                else
                    Generate $ map GenModuleItem
                    [ MIPackageItem $ Decl decl
                    , Assign AssignOptionNone (LHSIdent x) expr
                    , always_comb $ Asgn AsgnOpEq Nothing lhs (Ident x)
                    ]
            where
                decl = Net Local TWire DefaultStrength t x [] Nil
                t = Implicit Unspecified [r]
                r = (DimsFn FnBits $ Right $ lhsToExpr lhs, RawNum 1)
                x = "sv2v_tmp_" ++ shortHash (lhs, expr)
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
                (bindings', newItemsList) = unzip $ map fixBinding bindings
                newItems = concat newItemsList
                fixBinding :: PortBinding -> (PortBinding, [ModuleItem])
                fixBinding (portName, expr) =
                    if not outputBound || not usesReg
                        then ((portName, expr), [])
                        else ((portName, tmpExpr), items)
                    where
                        outputBound = portDir == Just Output
                        usesReg = Just True == fmap isReg (exprToLHS expr)
                        portDir = maybeModulePorts >>= lookup portName
                        tmp = "sv2v_tmp_" ++ instanceName ++ "_" ++ portName
                        tmpExpr = Ident tmp
                        decl = Net Local TWire DefaultStrength t tmp [] Nil
                        t = Implicit Unspecified [r]
                        r = (DimsFn FnBits $ Right expr, RawNum 1)
                        items =
                            [ MIPackageItem $ Decl decl
                            , always_comb $ Asgn AsgnOpEq Nothing lhs tmpExpr]
                        Just lhs = exprToLHS expr
                maybeModulePorts = Map.lookup moduleName ports
        fixModuleItem other = other

traverseDeclM :: Decl -> ST Decl
traverseDeclM decl@(Variable _ t x _ _) =
    insertElem x t >> return decl
traverseDeclM decl@(Net _ _ _ t x _ _) =
    insertElem x t >> return decl
traverseDeclM decl = return decl

rewriteDeclM :: Locations -> Decl -> Scoper Type Decl
rewriteDeclM locations (Variable d (IntegerVector TLogic sg rs) x a e) = do
    accesses <- localAccessesM x
    let location = map accessName accesses
    let usedAsReg = Set.member location locations
    blockLogic <- withinProcedureM
    if blockLogic || usedAsReg || e /= Nil
        then do
            let d' = if d == Inout then Output else d
            let t' = IntegerVector TReg sg rs
            insertElem accesses t'
            return $ Variable d' t' x a e
        else do
            let t' = Implicit sg rs
            insertElem accesses t'
            return $ Net d TWire DefaultStrength t' x a e
rewriteDeclM _ decl@(Variable _ t x _ _) =
    insertElem x t >> return decl
rewriteDeclM _ (Net d n s (IntegerVector _ sg rs) x a e) =
    insertElem x t >> return (Net d n s t x a e)
    where t = Implicit sg rs
rewriteDeclM _ decl@(Net _ _ _ t x _ _) =
    insertElem x t >> return decl
rewriteDeclM _ (Param s (IntegerVector _ sg []) x e) =
    return $ Param s (Implicit sg [(zero, zero)]) x e
    where zero = RawNum 0
rewriteDeclM _ (Param s (IntegerVector _ sg rs) x e) =
    return $ Param s (Implicit sg rs) x e
rewriteDeclM _ decl = return decl

traverseStmtM :: Stmt -> ST Stmt
traverseStmtM (Asgn op Just{} lhs expr) =
    -- ignore the timing LHSs
    traverseStmtM $ Asgn op Nothing lhs expr
traverseStmtM stmt@(Subroutine (Ident f) (Args (_ : Ident x : _) [])) =
    when (f == "$readmemh" || f == "$readmemb") (collectLHSM $ LHSIdent x)
        >> return stmt
traverseStmtM stmt =
    collectStmtLHSsM (collectNestedLHSsM collectLHSM) stmt
        >> return stmt

collectLHSM :: LHS -> ST ()
collectLHSM lhs = do
    details <- lookupElemM lhs
    case details of
        Just (accesses, _, _) ->
            lift $ tell $ Set.singleton location
            where location = map accessName accesses
        Nothing -> return ()
