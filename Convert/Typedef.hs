{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for `typedef`
 -
 - Aliased types can (probably) appear in all item declarations, including
 - modules, blocks, and function parameters.
 -}

module Convert.Typedef (convert) where

import Data.Maybe
import qualified Data.Map as Map

import Language.SystemVerilog.AST

type Types = Map.Map Identifier Type

convert :: AST -> AST
convert descriptions =
    filter (not . isTypedef) $ map (convertDescription types) descriptions
    where
        types = Map.fromList $ mapMaybe getTypedef descriptions
        getTypedef :: Description -> Maybe (Identifier, Type)
        getTypedef (Typedef a b) = Just (b, a)
        getTypedef _ = Nothing

isTypedef :: Description -> Bool
isTypedef (Typedef _ _) = True
isTypedef _ = False

convertDescription :: Types -> Description -> Description
convertDescription types (Module name ports items) =
    Module name ports $ map (convertModuleItem types) items
convertDescription _ other = other

resolveType :: Types -> Type -> Type
resolveType _ (Reg      rs) = Reg      rs
resolveType _ (Wire     rs) = Wire     rs
resolveType _ (Logic    rs) = Logic    rs
resolveType _ (Implicit rs) = Implicit rs
resolveType _ (IntegerT   ) = IntegerT
resolveType _ (Enum Nothing vals rs) = Enum Nothing vals rs
resolveType types (Enum (Just t) vals rs) = Enum (Just $ resolveType types t) vals rs
resolveType types (Alias st rs1) =
    case resolveType types $ types Map.! st of
        (Reg      rs2) -> Reg      $ rs2 ++ rs1
        (Wire     rs2) -> Wire     $ rs2 ++ rs1
        (Logic    rs2) -> Logic    $ rs2 ++ rs1
        (Enum t v rs2) -> Enum t v $ rs2 ++ rs1
        (Implicit rs2) -> Implicit $ rs2 ++ rs1
        (IntegerT    ) -> error $ "resolveType encountered packed `integer` on " ++ st
        (Alias    _ _) -> error $ "resolveType invariant failed on " ++ st

convertDecl :: Types -> Decl -> Decl
convertDecl types decl =
    case decl of
        Parameter  t x    e -> Parameter  (rt t) x   (re e)
        Localparam t x    e -> Localparam (rt t) x   (re e)
        Variable d t x a me -> Variable d (rt t) x a me'
            where me' = if isJust me then Just (re $ fromJust me) else me
    where
        rt = resolveType types
        re = convertExpr types

convertModuleItem :: Types -> ModuleItem -> ModuleItem
convertModuleItem types (MIDecl decl) =
    MIDecl $ convertDecl types decl
convertModuleItem types (Function t x decls stmt) =
    Function (resolveType types t) x
        (map (convertDecl types) decls)
        (convertStmt types stmt)
convertModuleItem types (Assign lhs expr) =
    Assign lhs (convertExpr types expr)
convertModuleItem types (AlwaysC kw stmt) =
    AlwaysC kw (convertStmt types stmt)
convertModuleItem _ other = other

convertStmt :: Types -> Stmt -> Stmt
convertStmt types = rs
    where
        rd = convertDecl types
        re = convertExpr types
        rs :: Stmt -> Stmt
        rs (Block header stmts) =
            Block header' (map rs stmts)
            where header' = fmap (\(x, decls) -> (x, map rd decls)) header
        rs (Case kw e cases def) = Case kw (re e)
            (map convertCase cases) (fmap rs def)
            where
                convertCase (exprs, stmt) = (map re exprs, rs stmt)
        rs (AsgnBlk lhs expr) = AsgnBlk lhs (re expr)
        rs (Asgn    lhs expr) = Asgn    lhs (re expr)
        rs (For (x1, e1) e (x2, e2) stmt) =
            For (x1, re e1) (re e) (x2, re e2) (rs stmt)
        rs (If e s1 s2) = If (re e) (rs s1) (rs s2)
        rs (Timing sense stmt) = Timing sense (rs stmt)
        rs (Null) = Null

convertExpr :: Types -> Expr -> Expr
convertExpr types = re
    where
        re :: Expr -> Expr
        re (String     s) = String    s
        re (Number     s) = Number    s
        re (ConstBool  b) = ConstBool b
        re (Ident      i  ) = Ident      i
        re (IdentRange i r) = IdentRange i r
        re (IdentBit   i e) = IdentBit   i (re e)
        re (Repeat     e l) = Repeat (re e) (map re l)
        re (Concat     l  ) = Concat (map re l)
        re (Call       f l) = Call f (map re l)
        re (UniOp      o e) = UniOp o (re e)
        re (BinOp      o e1 e2) = BinOp o (re e1) (re e2)
        re (Mux        e1 e2 e3) = Mux (re e1) (re e2) (re e3)
        re (Bit        e n) = Bit (re e) n
        -- This is the reason we have to convert expressions in this module.
        re (Cast       t e) = Cast (resolveType types t) (re e)
