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

import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Convert.Traverse
import Language.SystemVerilog.AST

type Idents = Set.Set Identifier
type Ports = Map.Map (Identifier, Identifier) Direction

convert :: [AST] -> [AST]
convert =
    traverseFiles
        (collectDescriptionsM collectPortsM)
        (traverseDescriptions . convertDescription)
    where
        collectPortsM :: Description -> Writer Ports ()
        collectPortsM (orig @ (Part _ _ _ _ name portNames _)) =
            collectModuleItemsM collectPortDirsM orig
            where
                collectPortDirsM :: ModuleItem -> Writer Ports ()
                collectPortDirsM (MIPackageItem (Decl (Variable dir _ ident _ _))) =
                    if dir == Local then
                        return ()
                    else if elem ident portNames then
                        tell $ Map.singleton (name, ident) dir
                    else
                        error $ "encountered decl with a dir that isn't a port: "
                            ++ show (dir, ident)
                collectPortDirsM _ = return ()
        collectPortsM _ = return ()

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
        fixModuleItem (Assign Nothing lhs expr) =
            if Set.disjoint usedIdents origIdents
                then Assign Nothing lhs expr
                else AlwaysC AlwaysComb $ AsgnBlk AsgnOpEq lhs expr
            where
                usedIdents = execWriter $ collectNestedLHSsM lhsIdents lhs
        -- rewrite port bindings to use temporary nets where necessary
        fixModuleItem (Instance moduleName params instanceName rs bindings) =
            if null newItems
                then Instance moduleName params instanceName rs bindings
                else Generate $ map GenModuleItem $
                    (MIPackageItem $ Comment "rewrote reg-to-output bindings") :
                    newItems ++
                    [Instance moduleName params instanceName rs bindings']
            where
                (bindings', newItemsList) = unzip $ map fixBinding bindings
                newItems = concat newItemsList
                fixBinding :: PortBinding -> (PortBinding, [ModuleItem])
                fixBinding (portName, Just expr) =
                    if portDir /= Just Output || Set.disjoint usedIdents origIdents
                        then ((portName, Just expr), [])
                        else ((portName, Just tmpExpr), items)
                    where
                        portDir = Map.lookup (moduleName, portName) ports
                        usedIdents = execWriter $
                            collectNestedExprsM exprIdents expr
                        tmp = "sv2v_tmp_" ++ instanceName ++ "_" ++ portName
                        tmpExpr = Ident tmp
                        t = Net TWire Unspecified [(DimsFn FnBits $ Right expr, Number "1")]
                        items =
                            [ MIPackageItem $ Decl $ Variable Local t tmp [] Nothing
                            , AlwaysC AlwaysComb $ AsgnBlk AsgnOpEq lhs tmpExpr]
                        lhs = case exprToLHS expr of
                            Just l -> l
                            Nothing ->
                                error $ "bad non-lhs, non-net expr "
                                    ++ show expr ++ " connected to output port "
                                    ++ portName ++ " of " ++ instanceName
                fixBinding other = (other, [])
        fixModuleItem other = other

        -- rewrite variable declarations to have the correct type
        convertModuleItem (MIPackageItem (Decl (Variable dir (IntegerVector _ sg mr) ident a me))) =
            MIPackageItem $ Decl $ Variable dir (t mr) ident a me
            where
                t = if Set.member ident fixedIdents
                    then IntegerVector TReg sg
                    else Net TWire sg
        convertModuleItem other = other
        -- all other logics (i.e. inside of functions) become regs
        convertDecl :: Decl -> Decl
        convertDecl (Param s (IntegerVector _ sg rs) x e) =
            Param s (Implicit sg rs) x e
        convertDecl (Variable d (IntegerVector TLogic sg rs) x a me) =
            Variable d (IntegerVector TReg sg rs) x a me
        convertDecl other = other

regIdents :: ModuleItem -> Writer Idents ()
regIdents (AlwaysC _ stmt) =
    collectNestedStmtsM (collectStmtLHSsM (collectNestedLHSsM lhsIdents)) $
    traverseNestedStmts removeTimings stmt
    where
        removeTimings :: Stmt -> Stmt
        removeTimings (Timing _ s) = s
        removeTimings other = other
regIdents (Initial stmt) =
    regIdents $ AlwaysC Always stmt
regIdents (Final stmt) =
    regIdents $ AlwaysC Always stmt
regIdents _ = return ()

lhsIdents :: LHS -> Writer Idents ()
lhsIdents (LHSIdent x) = tell $ Set.singleton x
lhsIdents _ = return () -- the collector recurses for us

exprIdents :: Expr -> Writer Idents ()
exprIdents (Ident x) = tell $ Set.singleton x
exprIdents _ = return () -- the collector recurses for us
