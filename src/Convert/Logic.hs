{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion from `logic` to `wire` or `reg`
 -
 - We convert a module-level logic to a reg if it is assigned to in an always or
 - initial block. Other module-level logics become wires. All other logics
 - (i.e., in a function) become regs.
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

convert :: AST -> AST
convert ast =
    traverseDescriptions (convertDescription ports) ast
    where
        ports = execWriter $ collectDescriptionsM collectPortsM ast
        collectPortsM :: Description -> Writer Ports ()
        collectPortsM (orig @ (Part _ _ _ name portNames _)) =
            collectModuleItemsM collectPortDirsM orig
            where
                collectPortDirsM :: ModuleItem -> Writer Ports ()
                collectPortDirsM (MIDecl (Variable dir _ ident _ _)) =
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
        then traverseModuleItems conversion orig
        else orig
    where
        shouldConvert = case orig of
            Part _ Interface _ _ _ _ -> False
            Part _ Module _ _ _ _ -> True
            PackageItem _ -> True
            Package _ _ _ -> False
            Directive _ -> False
        conversion = traverseDecls convertDecl . convertModuleItem
        idents = execWriter (collectModuleItemsM regIdents orig)
        convertModuleItem :: ModuleItem -> ModuleItem
        -- rewrite bad continuous assignments to use procedural assignments
        convertModuleItem (Assign Nothing lhs expr) =
            if Set.disjoint usedIdents idents
                then Assign Nothing lhs expr
                else AlwaysC AlwaysComb $ AsgnBlk AsgnOpEq lhs expr
            where
                usedIdents = execWriter $ collectNestedLHSsM lhsIdents lhs
        -- rewrite port bindings to use temporary nets where necessary
        convertModuleItem (Instance moduleName params instanceName rs bindings) =
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
                    if portDir /= Just Output || Set.disjoint usedIdents idents
                        then ((portName, Just expr), [])
                        else ((portName, Just tmpExpr), items)
                    where
                        portDir = Map.lookup (moduleName, portName) ports
                        usedIdents = execWriter $
                            collectNestedExprsM exprIdents expr
                        tmp = "sv2v_tmp_" ++ instanceName ++ "_" ++ portName
                        tmpExpr = Ident tmp
                        t = Net TWire [(Bits $ Right expr, Number "1")]
                        items =
                            [ MIDecl $ Variable Local t tmp [] Nothing
                            , AlwaysC AlwaysComb $ AsgnBlk AsgnOpEq lhs tmpExpr]
                        lhs = case exprToLHS expr of
                            Just l -> l
                            Nothing ->
                                error $ "bad non-lhs, non-net expr "
                                    ++ show expr ++ " connected to output port "
                                    ++ portName ++ " of " ++ instanceName
                fixBinding other = (other, [])
        -- rewrite variable declarations to have the correct type
        convertModuleItem (MIDecl (Variable dir (IntegerVector TLogic sg mr) ident a me)) =
            MIDecl $ Variable dir (t mr) ident a me
            where
                t = if sg /= Unspecified || Set.member ident idents
                    then IntegerVector TReg sg
                    else Net TWire
        convertModuleItem other = other
        -- all other logics (i.e. inside of functions) become regs
        convertDecl :: Decl -> Decl
        convertDecl (Parameter  (IntegerVector TLogic sg rs) x e) =
            Parameter  (Implicit sg rs) x e
        convertDecl (Localparam (IntegerVector TLogic sg rs) x e) =
            Localparam (Implicit sg rs) x e
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
regIdents _ = return ()

lhsIdents :: LHS -> Writer Idents ()
lhsIdents (LHSIdent x) = tell $ Set.singleton x
lhsIdents _ = return () -- the collector recurses for us

exprIdents :: Expr -> Writer Idents ()
exprIdents (Ident x) = tell $ Set.singleton x
exprIdents _ = return () -- the collector recurses for us
