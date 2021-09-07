{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Conversion for checking and standardizing port declarations
 -
 - Non-ANSI style port declarations can be split into two separate declarations.
 - Section 23.2.2.1 of IEEE 1800-2017 defines rules for determining the
 - resulting details of such declarations. This conversion is part of the
 - initial phases to avoid requiring that downstream conversions handle these
 - unusual otherwise conflicting declarations.
 -
 - To avoid creating spurious conflicts for redeclared ports, this conversion is
 - also responsible for defaulting variable ports to `logic`.
 -}

module Convert.PortDecl (convert) where

import Data.List (intercalate, (\\))
import Data.Maybe (mapMaybe)

import Convert.Traverse
import Language.SystemVerilog.AST

convert :: [AST] -> [AST]
convert = map $ traverseDescriptions traverseDescription

traverseDescription :: Description -> Description
traverseDescription (Part attrs extern kw liftetime name ports items) =
    Part attrs extern kw liftetime name ports items'
    where items' = convertPorts name ports items
traverseDescription (PackageItem item) =
    PackageItem $ convertPackageItem item
traverseDescription other = other

convertPackageItem :: PackageItem -> PackageItem
convertPackageItem (Function l t x decls stmts) =
   Function l t x (convertTFDecls decls) stmts
convertPackageItem (Task l x decls stmts) =
   Task l x (convertTFDecls decls) stmts
convertPackageItem other = other

convertPorts :: Identifier -> [Identifier] -> [ModuleItem] -> [ModuleItem]
convertPorts name ports items
    | not (null name) && not (null extraPorts) =
        error $ "declared ports " ++ intercalate ", " extraPorts
            ++ " are not in the port list of " ++ name
    | otherwise =
        map traverseItem items
    where
        portDecls = mapMaybe (findDecl True ) items
        dataDecls = mapMaybe (findDecl False) items
        extraPorts = map fst portDecls \\ ports

        -- rewrite a declaration if necessary
        traverseItem :: ModuleItem -> ModuleItem
        traverseItem (MIPackageItem (Decl decl))
            | Variable d _ x _ e <- decl = rewrite decl (combineIdent x) d x e
            | Net  d _ _ _ x _ e <- decl = rewrite decl (combineIdent x) d x e
            | otherwise = MIPackageItem $ Decl decl
        traverseItem (MIPackageItem item) =
            MIPackageItem $ convertPackageItem item
        traverseItem other = other

        -- produce the combined declaration for a port, if it has one
        combineIdent :: Identifier -> Maybe Decl
        combineIdent x = do
            portDecl <- lookup x portDecls
            dataDecl <- lookup x dataDecls
            Just $ combineDecls portDecl dataDecl

-- wrapper for convertPorts enabling its application to task or function decls
convertTFDecls :: [Decl] -> [Decl]
convertTFDecls =
    map unwrap . convertPorts "" [] . map wrap
    where
        wrap :: Decl -> ModuleItem
        wrap = MIPackageItem . Decl
        unwrap :: ModuleItem -> Decl
        unwrap item = decl
            where MIPackageItem (Decl decl) = item

-- given helpfully extracted information, update the given declaration
rewrite :: Decl -> Maybe Decl -> Direction -> Identifier -> Expr -> ModuleItem
-- implicitly-typed output ports default to `logic` in SystemVerilog
rewrite (Variable Output (Implicit sg rs) x a e) Nothing _ _ _ =
    MIPackageItem $ Decl $ Variable Output (IntegerVector TLogic sg rs) x a e
-- not a relevant port declaration
rewrite decl Nothing _ _ _ =
    MIPackageItem $ Decl decl
-- turn the non-ANSI style port and data declarations into fully-specified ports
-- and optional continuous assignments, respectively
rewrite _ (Just combined) d x e
    | d /= Local =
        MIPackageItem $ Decl combined
    | e /= Nil =
        Assign AssignOptionNone (LHSIdent x) e
    | otherwise =
        MIPackageItem $ Decl $ CommentDecl $ "combined with " ++ x

-- combine the two declarations defining a non-ANSI style port
combineDecls :: Decl -> Decl -> Decl
combineDecls portDecl dataDecl
    | eP /= Nil =
        mismatch "invalid initialization at port declaration"
    | aP /= aD =
        mismatch "different unpacked dimensions"
    | rsP /= rsD =
        mismatch "different packed dimensions"
    | otherwise =
        base (tf rsD) ident aD Nil
    where

        -- signed if *either* declaration is marked signed
        sg = if sgP == Signed || sgD == Signed
            then Signed
            else Unspecified

        -- the port cannot have a variable or net type
        Implicit sgP rsP = case tP of
            Implicit{} -> tP
            _ -> mismatch "redeclaration"

        -- pull out the base type, signedness, and packed dimensions
        (tf, sgD, rsD) = case tD of
            Implicit        s r -> (IntegerVector TLogic sg, s, r )
            IntegerVector k s r -> (IntegerVector k      sg, s, r )
            IntegerAtom   k s   -> (\[] -> IntegerAtom k s , s, [])
            -- TODO: other basic types may be worth supporting here
            _ -> mismatch "non-ANSI port declaration with unsupported data type"

        -- extract the core components of each declaration
        Variable dir tP ident aP eP = portDecl
        (base, tD, aD) = case dataDecl of
            Variable Local t _ a _ -> (Variable dir, t, a)
            Net  Local n s t _ a _ -> (Net  dir n s, t, a)
            _ -> undefined -- not possible given findDecl

        -- helpful error message utility
        mismatch :: String -> a
        mismatch msg = error $ "declarations `" ++ p portDecl ++ "` and `"
            ++ p dataDecl ++ "` are incompatible due to " ++ msg
            where p = init . show

-- used to build independent lists of port and data declarations
findDecl :: Bool -> ModuleItem -> Maybe (Identifier, Decl)
findDecl isPort (MIPackageItem (Decl decl))
    | Variable d _ x _ _ <- decl, (d /= Local) == isPort = Just (x, decl)
    | Net  d _ _ _ x _ _ <- decl, (d /= Local) == isPort = Just (x, decl)
findDecl _ _ = Nothing
