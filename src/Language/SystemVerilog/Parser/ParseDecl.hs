{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Advanced parser for declarations, module instantiations, and some statements.
 -
 - This module exists because the SystemVerilog grammar is not LALR(1), and
 - Happy can only produce LALR(1) parsers. This module provides an interface for
 - parsing a list of "DeclTokens" into `Decl`s, `ModuleItem`s, or `Stmt`s. This
 - works through a series of functions which have use a greater lookahead for
 - resolving the conflicts.
 -
 - Consider the following two module declarations:
 -  module Test(one two, three [1:0], four);
 -  module Test(one two, three [1:0]  four);
 -
 - When `{one} two ,` is on the stack, it is impossible to know whether to A)
 - shift `three` to add to the current declaration list; or B) to reduce the
 - stack and begin a new port declaration; without looking ahead more than 1
 - token.
 -
 - While I previously had some success dealing with these conflicts with
 - increasingly convoluted grammars, this became more and more untenable as I
 - added support for more SystemVerilog constructs.
 -
 - Because declarations and statements are subject to the same kind of
 - conflicts, this module additionally provides an interface for parsing
 - DeclTokens as either declarations or the basic statements (either assignments
 - or task/function calls) with which they can conflict. The initialization
 - portion of a for loop also allows for declarations and assignments, and so a
 - similar interface is provided for this case.
 -
 - Parameter port lists allow the omission of the `parameter` and `localparam`
 - keywords, and so require special handling in the same vein as other
 - declarations. However, this custom parsing is not necessary for parameter
 - declarations outside of parameter port lists because the leading keyword is
 - otherwise required.
 -
 - This parser is very liberal, and so accepts some syntactically invalid files.
 - In the future, we may add some basic type-checking to complain about
 - malformed input files. However, we generally assume that users have tested
 - their code with a commercial simulator before running it through our tool.
 -}

module Language.SystemVerilog.Parser.ParseDecl
( DeclToken (..)
, parseDTsAsPortDecls
, parseDTsAsModuleItems
, parseDTsAsTFDecls
, parseDTsAsDecl
, parseDTsAsDeclOrStmt
, parseDTsAsDeclsOrAsgns
, parseDTsAsParams
) where

import Data.List (findIndex, partition, uncons)

import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser.Tokens (Position(..))

-- [PUBLIC]: combined (irregular) tokens for declarations
data DeclToken
    = DTComma    Position
    | DTAutoDim  Position
    | DTConst    Position
    | DTVar      Position
    | DTAsgn     Position AsgnOp (Maybe Timing) Expr
    | DTRange    Position PartSelectMode Range
    | DTIdent    Position Identifier
    | DTPSIdent  Position Identifier Identifier
    | DTCSIdent  Position Identifier [ParamBinding] Identifier
    | DTDir      Position Direction
    | DTType     Position (Signing -> [Range] -> Type)
    | DTNet      Position NetType Strength
    | DTParams   Position [ParamBinding]
    | DTPorts    Position [PortBinding]
    | DTBit      Position Expr
    | DTLHSBase  Position LHS
    | DTDot      Position Identifier
    | DTSigning  Position Signing
    | DTLifetime Position Lifetime
    | DTAttr     Position Attr
    | DTEnd      Position Char
    | DTParamKW  Position ParamScope
    | DTTypeDecl Position
    | DTTypeAsgn Position


-- [PUBLIC]: parser for module port declarations, including interface ports
-- Example: `input foo, bar, One inst`
parseDTsAsPortDecls :: [DeclToken] -> ([Identifier], [ModuleItem])
parseDTsAsPortDecls = parseDTsAsPortDecls' . dropTrailingComma

-- internal utility to drop a single trailing comma in port or parameter lists,
-- which we allow for compatibility with Yosys
dropTrailingComma :: [DeclToken] -> [DeclToken]
dropTrailingComma [] = []
dropTrailingComma [DTComma{}, end@DTEnd{}] = [end]
dropTrailingComma (tok : toks) = tok : dropTrailingComma toks

-- internal parseDTsAsPortDecls after the removal of an optional trailing comma
parseDTsAsPortDecls' :: [DeclToken] -> ([Identifier], [ModuleItem])
parseDTsAsPortDecls' pieces =
    if isSimpleList
        then (simpleIdents, [])
        else (portNames declarations, applyAttrs [] pieces declarations)
    where
        maybeSimpleIdents = parseDTsAsIdents pieces
        Just simpleIdents = maybeSimpleIdents
        isSimpleList = maybeSimpleIdents /= Nothing

        declarations = parseDTsAsDecls Input ModeDefault pieces'

        pieces' = filter (not . isAttr) pieces

        portNames :: [Decl] -> [Identifier]
        portNames = filter (not . null) . map portName
        portName :: Decl -> Identifier
        portName (Variable _ _ ident _ _) = ident
        portName (Net  _ _ _ _ ident _ _) = ident
        portName _ = ""

        applyAttrs :: [Attr] -> [DeclToken] -> [Decl] -> [ModuleItem]
        applyAttrs _ tokens (CommentDecl c : decls) =
            MIPackageItem (Decl $ CommentDecl c) : applyAttrs [] tokens decls
        applyAttrs attrs (DTAttr _ attr : tokens) decls =
            applyAttrs (attr : attrs) tokens decls
        applyAttrs attrs [] [decl] =
            [wrapDecl attrs decl]
        applyAttrs attrs (DTComma{} : tokens) (decl : decls) =
            wrapDecl attrs decl : applyAttrs attrs tokens decls
        applyAttrs attrs (_ : tokens) decls =
            applyAttrs attrs tokens decls
        applyAttrs _ [] _ = undefined

        wrapDecl :: [Attr] -> Decl -> ModuleItem
        wrapDecl attrs decl = foldr MIAttr (MIPackageItem $ Decl decl) attrs

-- internal utility for a simple list of port identifiers
parseDTsAsIdents :: [DeclToken] -> Maybe [Identifier]
parseDTsAsIdents [DTIdent _ x, DTEnd _ _] = Just [x]
parseDTsAsIdents [_, _] = Nothing
parseDTsAsIdents (DTIdent _ x : DTComma _ : rest) =
    fmap (x :) (parseDTsAsIdents rest)
parseDTsAsIdents _ = Nothing


-- [PUBLIC]: parser for single (semicolon-terminated) declarations (including
-- parameters) and module instantiations
parseDTsAsModuleItems :: [DeclToken] -> [ModuleItem]
parseDTsAsModuleItems tokens =
    if maybeElabTask /= Nothing then
        [elabTask]
    else if any isPorts tokens then
        parseDTsAsIntantiations tokens
    else
        map (MIPackageItem . Decl) $ parseDTsAsDecl tokens
    where
        Just elabTask = maybeElabTask
        maybeElabTask = asElabTask tokens

-- internal; attempt to parse an elaboration system task
asElabTask :: [DeclToken] -> Maybe ModuleItem
asElabTask tokens = do
    DTIdent _ x@('$' : _) <- return $ head tokens
    severity <- lookup x elabTasks
    Just $ ElabTask severity args
    where
        args =
            case tail tokens of
                [DTEnd{}] -> Args [] []
                [DTPorts _ ports, DTEnd{}] -> portsToArgs ports
                DTPorts{} : tok : _ -> parseError tok msg
                toks -> parseError (head toks) msg
        msg = "unexpected token after elaboration system task"

-- lookup table for elaboration system task severities
elabTasks :: [(String, Severity)]
elabTasks = map (\x -> (show x, x))
    [SeverityInfo, SeverityWarning, SeverityError, SeverityFatal]

-- internal; parser for module instantiations
parseDTsAsIntantiations :: [DeclToken] -> [ModuleItem]
parseDTsAsIntantiations (DTIdent _ name : DTParams _ params : tokens) =
    step tokens
    where
        step :: [DeclToken] -> [ModuleItem]
        step [] = []
        step toks = inst : step restToks
            where
                inst = Instance name params x rs p
                (x, rs, p) = parseDTsAsIntantiation instToks delimTok
                (instToks, delimTok : restToks) = break isCommaOrEnd toks
parseDTsAsIntantiations (DTIdent pos name : tokens) =
    parseDTsAsIntantiations $ DTIdent pos name : DTParams pos [] : tokens
parseDTsAsIntantiations tokens =
    parseError (head tokens)
        "expected module or interface name at beginning of instantiation list"

-- internal; parser for an individual instantiations
parseDTsAsIntantiation :: [DeclToken] -> DeclToken
    -> (Identifier, [Range], [PortBinding])
parseDTsAsIntantiation l0 delimTok =
    if null l0 then
        parseError delimTok $ "expected instantiation before " ++ delimStr
    else if not (isIdent nameTok) then
        parseError nameTok "expected instantiation name"
    else if null l1 then
        parseError delimTok $ "expected port connections before " ++ delimStr
    else if seq ranges not (isPorts portsTok) then
        parseError portsTok "expected port connections"
    else
        (name, ranges, ports)
    where
        delimChar = case delimTok of
                        DTEnd _ char -> char
                        _ -> ','
        delimStr = ['\'', delimChar, '\'']
        Just (nameTok, l1) = uncons l0
        rangeToks = init l1
        portsTok = last l1
        DTIdent _ name = nameTok
        DTPorts _ ports = portsTok
        ranges = map asRange rangeToks
        asRange :: DeclToken -> Range
        asRange (DTRange _ NonIndexed s) = s
        asRange (DTBit _ s) = (RawNum 0, BinOp Sub s (RawNum 1))
        asRange tok = parseError tok "expected instantiation dimensions"


-- [PUBLIC]: parser for comma-separated task/function port declarations
parseDTsAsTFDecls :: [DeclToken] -> [Decl]
parseDTsAsTFDecls = parseDTsAsDecls Input ModeDefault


-- [PUBLIC]; used for "single" declarations, i.e., declarations appearing
-- outside of a port list
parseDTsAsDecl :: [DeclToken] -> [Decl]
parseDTsAsDecl = parseDTsAsDecls Local ModeSingle


-- [PUBLIC]: parser for single block item declarations or assign or arg-less
-- subroutine call statements
parseDTsAsDeclOrStmt :: [DeclToken] -> ([Decl], [Stmt])
parseDTsAsDeclOrStmt tokens =
    if declLookahead tokens
        then (parseDTsAsDecl tokens, [])
        else ([], parseDTsAsStmt $ shiftIncOrDec tokens)

-- check if the necessary tokens for a complete declaration exist at the
-- beginning of the given token list
declLookahead :: [DeclToken] -> Bool
declLookahead l0 =
    length l0 /= length l6 && tripLookahead l6
    where
        (_, l1) = takeDir      l0 Local
        (_, l2) = takeLifetime l1
        (_, l3) = takeConst    l2
        (_, l4) = takeVarOrNet l3
        (_, l5) = takeType     l4
        (_, l6) = takeRanges   l5

-- internal; parser for leading statements in a procedural block
parseDTsAsStmt :: [DeclToken] -> [Stmt]
parseDTsAsStmt l0 =
    [traceStmt $ head l0, stmt]
    where
        (lhs, _) = takeLHS l0
        (expr, l1) = takeExpr l0
        stmt = case init l1 of
            [DTAsgn _ op mt e] -> Asgn op mt lhs e
            [DTPorts _ ports] -> Subroutine expr (portsToArgs ports)
            [] -> Subroutine expr (Args [] [])
            tok : _ -> parseError tok "unexpected statement token"

traceStmt :: DeclToken -> Stmt
traceStmt tok = CommentStmt $ "Trace: " ++ show (tokPos tok)

-- read the given tokens as the root of a subroutine invocation
takeExpr :: [DeclToken] -> (Expr, [DeclToken])
takeExpr (DTPSIdent _ p   x : toks) = (PSIdent p   x, toks)
takeExpr (DTCSIdent _ c p x : toks) = (CSIdent c p x, toks)
takeExpr toks = (lhsToExpr lhs, rest)
    where (lhs, rest) = takeLHS toks

-- converts port bindings to call args
portsToArgs :: [PortBinding] -> Args
portsToArgs bindings =
    Args pnArgs kwArgs
    where
        (pnBindings, kwBindings) = partition (null . fst) bindings
        pnArgs = map snd pnBindings
        kwArgs = kwBindings

-- [PUBLIC]: parser for comma-separated declarations or assignment lists; this
-- is only used for `for` loop initialization lists
parseDTsAsDeclsOrAsgns :: [DeclToken] -> Either [Decl] [(LHS, Expr)]
parseDTsAsDeclsOrAsgns tokens =
    if declLookahead tokens
        then Left $ parseDTsAsDecls Local ModeForLoop tokens
        else Right $ parseDTsAsAsgns $ shiftIncOrDec tokens

-- internal parser for basic assignment lists
parseDTsAsAsgns :: [DeclToken] -> [(LHS, Expr)]
parseDTsAsAsgns tokens =
    if not (isAsgn asgnTok) then
        parseError asgnTok "expected assignment operator"
    else if mt /= Nothing then
        unexpected "timing modifier"
    else (lhs, expr) : case head remaining of
        DTEnd{} -> []
        DTComma{} -> parseDTsAsAsgns $ tail remaining
        tok -> parseError tok "expected ',' or ';'"
    where
        (lhs, asgnTok : remaining) = takeLHS tokens
        DTAsgn _ op mt rhs = asgnTok
        expr = case op of
            AsgnOpEq -> rhs
            AsgnOpNonBlocking -> unexpected "non-blocking assignment"
            AsgnOp binop -> BinOp binop (lhsToExpr lhs) rhs

        unexpected surprise = parseError asgnTok $
            "unexpected " ++ surprise ++ " in for loop initialization"

shiftIncOrDec :: [DeclToken] -> [DeclToken]
shiftIncOrDec (tok@(DTAsgn _ AsgnOp{} _ _) : toks) =
    before ++ tok : delim : shiftIncOrDec after
    where (before, delim : after) = break isCommaOrEnd toks
shiftIncOrDec [] = []
shiftIncOrDec toks =
    before ++ delim : shiftIncOrDec after
    where (before, delim : after) = break isCommaOrEnd toks

takeLHS :: [DeclToken] -> (LHS, [DeclToken])
takeLHS tokens = takeLHSStep (takeLHSStart tok) toks
    where tok : toks = tokens

takeLHSStart :: DeclToken -> LHS
takeLHSStart (DTLHSBase _ lhs) = lhs
takeLHSStart (DTIdent _ x) = LHSIdent x
takeLHSStart tok = parseError tok "expected primary token or type"

takeLHSStep :: LHS -> [DeclToken] -> (LHS, [DeclToken])
takeLHSStep curr (DTBit   _ e   : toks) = takeLHSStep (LHSBit   curr e  ) toks
takeLHSStep curr (DTRange _ m r : toks) = takeLHSStep (LHSRange curr m r) toks
takeLHSStep curr (DTDot   _ x   : toks) = takeLHSStep (LHSDot   curr x  ) toks
takeLHSStep lhs toks = (lhs, toks)

type DeclBase = Identifier -> [Range] -> Expr -> Decl
type Triplet = (Identifier, [Range], Expr)

data Mode
    = ModeForLoop -- initialization always required
    | ModeSingle -- single declaration (not port list)
    | ModeDefault -- comma separated, multiple declarations
    deriving Eq

-- internal; entrypoint of the critical portion of our parser
parseDTsAsDecls :: Direction -> Mode -> [DeclToken] -> [Decl]
parseDTsAsDecls backupDir mode l0 =
    if l /= Nothing && l /= Just Automatic then
        parseError (head l1) "unexpected non-automatic lifetime"
    else if dir == Local && isImplicit t && not (isNet $ head l3) then
        parseError (head l0) "declaration missing type information"
    else if null l7 then
        decls
    else if mode == ModeSingle then
        parseError (head l7) "unexpected token in declaration"
    else
        decls ++ parseDTsAsDecls dir mode l7
    where
        initReason
            | hasDriveStrength (head l3) = "net with drive strength"
            | mode == ModeForLoop = "for loop"
            | con = "const"
            | otherwise = ""
        (dir, l1) = takeDir      l0 backupDir
        (l  , l2) = takeLifetime l1
        (con, l3) = takeConst    l2
        (von, l4) = takeVarOrNet l3
        (tf , l5) = takeType     l4
        (rs , l6) = takeRanges   l5
        (tps, l7) = takeTrips    l6 initReason
        base = von dir t
        t = tf rs
        decls =
            traceComment l0 :
            map (\(x, a, e) -> base x a e) tps

traceComment :: [DeclToken] -> Decl
traceComment = CommentDecl . ("Trace: " ++) . show . tokPos . head

hasDriveStrength :: DeclToken -> Bool
hasDriveStrength (DTNet _ _ DriveStrength{}) = True
hasDriveStrength _ = False

isImplicit :: Type -> Bool
isImplicit Implicit{} = True
isImplicit _ = False

takeTrips :: [DeclToken] -> String -> ([Triplet], [DeclToken])
takeTrips l0 initReason =
    (trip : trips, l5)
    where
        (x, l1) = takeIdent  l0
        (a, l2) = takeRanges l1
        (e, l3) = takeAsgn   l2 initReason
        l4 = takeCommaOrEnd  l3
        trip = (x, a, e)
        (trips, l5) =
            if tripLookahead l4
                then takeTrips l4 initReason
                else ([], l4)

tripLookahead :: [DeclToken] -> Bool
tripLookahead l0 =
    not (null l0) &&
    -- every triplet *must* begin with an identifier
    isIdent (head l0) &&
    -- expecting to see a comma or the ending token after the identifier and
    -- optional ranges and/or assignment
    isCommaOrEnd (head l3)
    where
        (_, l1) = takeIdent  l0
        (_, l2) = takeRanges l1
        (_, l3) = takeAsgn   l2 ""


-- [PUBLIC]: parser for parameter lists in headers of modules, interfaces, etc.
parseDTsAsParams :: [DeclToken] -> [Decl]
parseDTsAsParams = parseDTsAsParams' Parameter . dropTrailingComma

-- internal variant of the above, used recursively after the optional trailing
-- comma has been removed
parseDTsAsParams' :: ParamScope -> [DeclToken] -> [Decl]
parseDTsAsParams' _ [] = []
parseDTsAsParams' prevParamScope l0 =
    nextStep paramScope l2
    where
        nextStep = if isParamType
            then parseDTsAsParamType
            else parseDTsAsParam
        (paramScope , l1) = takeParamScope l0 prevParamScope
        (isParamType, l2) = takeParamType  l1

-- parse one or more regular parameter declarations at the beginning of the decl
-- token list before continuing on to subsequent declarations
parseDTsAsParam :: ParamScope -> [DeclToken] -> [Decl]
parseDTsAsParam paramScope l0 =
    traceComment l0 :
    map base tps ++
    parseDTsAsParams' paramScope l3
    where
        (tfO, l1) = takeType   l0
        (rsO, l2) = takeRanges l1
        (tps, l3) = takeTrips  l2 ""
        base :: Triplet -> Decl
        (tf, rs) = typeRanges $ tfO rsO -- for packing tolerance
        base (x, a, e) = Param paramScope (tf $ a ++ rs) x e

-- parse a single type parameter declaration at the beginning of the decl token
-- list before continuing on to subsequent declarations
parseDTsAsParamType :: ParamScope -> [DeclToken] -> [Decl]
parseDTsAsParamType paramScope l0 =
    traceComment l0 :
    ParamType paramScope x t :
    nextStep paramScope l3
    where
        nextStep = if typeAsgnLookahead l3
            then parseDTsAsParamType
            else parseDTsAsParams'
        (x, l1) = takeIdent    l0
        (t, l2) = takeTypeAsgn l1
        l3 = takeCommaOrEnd    l2

-- take the optional default value assignment for a type parameter declaration;
-- note that aliases are parsed as regular assignments to avoid conflicts, and
-- so are converted to types when encountered
takeTypeAsgn :: [DeclToken] -> (Type, [DeclToken])
takeTypeAsgn (DTTypeAsgn _ : l0) =
    (tf rs, l2)
    where
        (tf, l1) = takeType   l0
        (rs, l2) = takeRanges l1
takeTypeAsgn (DTAsgn pos AsgnOpEq Nothing expr : toks) =
    case exprToType expr of
        Nothing -> parseError pos "unexpected non-type parameter assignment"
        Just t -> (t, toks)
takeTypeAsgn toks = (UnknownType, toks)

-- check whether the front of the tokens could plausibly form a type parameter
-- declaration and optional assignment; type parameter declaration assignments
-- can only be "escaped" using an explicit non-type parameter/localparam keyword
typeAsgnLookahead :: [DeclToken] -> Bool
typeAsgnLookahead l0 =
    not (null l0) &&
    -- every type assignment *must* begin with an identifier
    isIdent (head l0) &&
    -- expecting to see a comma or the ending token after the identifier and
    -- optional assignment
    isCommaOrEnd (head l2)
    where
        (_, l1) = takeIdent    l0
        (_, l2) = takeTypeAsgn l1


takeDir :: [DeclToken] -> Direction -> (Direction, [DeclToken])
takeDir (DTDir _ dir : rest) _   = (dir, rest)
takeDir                rest  dir = (dir, rest)

takeLifetime :: [DeclToken] -> (Maybe Lifetime, [DeclToken])
takeLifetime (DTLifetime _ l : rest) = (Just  l, rest)
takeLifetime                   rest  = (Nothing, rest)

takeConst :: [DeclToken] -> (Bool, [DeclToken])
takeConst (DTConst{} : DTConst pos : _) =
    parseError pos "duplicate const modifier"
takeConst (DTConst pos : DTNet _ typ _ : _) =
    parseError pos $ show typ ++ " cannot be const"
takeConst (DTConst{} : tokens) = (True, tokens)
takeConst tokens = (False, tokens)

takeVarOrNet :: [DeclToken] -> (Direction -> Type -> DeclBase, [DeclToken])
takeVarOrNet (DTNet{} : DTVar pos : _) =
    parseError pos "unexpected var after net type"
takeVarOrNet (DTNet pos n s : tokens) =
    if n /= TTrireg && isChargeStrength s
        then parseError pos "only trireg can have a charge strength"
        else (\d -> Net d n s, tokens)
    where
        isChargeStrength :: Strength -> Bool
        isChargeStrength ChargeStrength{} = True
        isChargeStrength _ = False
takeVarOrNet tokens = (Variable, tokens)

takeType :: [DeclToken] -> ([Range] -> Type, [DeclToken])
takeType (DTIdent _ a  : DTDot _ b      : rest) = (InterfaceT a  b      , rest)
takeType (DTType  _ tf : DTSigning _ sg : rest) = (tf       sg          , rest)
takeType (DTType  _ tf                  : rest) = (tf       Unspecified , rest)
takeType (DTSigning _ sg                : rest) = (Implicit sg          , rest)
takeType (DTPSIdent _ ps    tn          : rest) = (PSAlias ps    tn     , rest)
takeType (DTCSIdent _ ps pm tn          : rest) = (CSAlias ps pm tn     , rest)
takeType (DTIdent pos tn                : rest) =
    if couldBeTypename
        then (Alias tn            ,                  rest)
        else (Implicit Unspecified, DTIdent pos tn : rest)
    where
        couldBeTypename =
            case (findIndex isIdent rest, findIndex isComma rest) of
                -- no identifiers left => no decl asgns
                (Nothing, _) -> False
                -- an identifier is left, and no more commas
                (_, Nothing) -> True
                -- if comma is first, then this ident is a declaration
                (Just a, Just b) -> a < b
takeType (DTVar{} : DTVar pos : _) =
    parseError pos "duplicate var modifier"
takeType (DTVar _ : rest) =
    case tf [] of
        Implicit sg [] -> (IntegerVector TLogic sg, rest')
        _ -> (tf, rest')
    where (tf, rest') = takeType rest
takeType rest = (Implicit Unspecified, rest)

takeRanges :: [DeclToken] -> ([Range], [DeclToken])
takeRanges tokens =
    case head tokens of
        DTRange _ NonIndexed r -> (r         : rs, rest)
        DTBit   _ s            -> (asRange s : rs, rest)
        DTAutoDim _            ->
            case head $ tail tokens of
                DTAsgn _ AsgnOpEq Nothing (Pattern l) -> autoDim l
                DTAsgn _ AsgnOpEq Nothing (Concat  l) -> autoDim l
                _ -> ([], tokens)
        _ -> ([], tokens)
    where
        (rs, rest) = takeRanges $ tail tokens
        asRange s = (RawNum 0, BinOp Sub s (RawNum 1))
        autoDim :: [a] -> ([Range], [DeclToken])
        autoDim l =
            ((lo, hi) : rs, rest)
            where
                n = length l
                lo = RawNum 0
                hi = RawNum $ fromIntegral $ n - 1

takeAsgn :: [DeclToken] -> String -> (Expr, [DeclToken])
takeAsgn (DTAsgn pos op mt e : rest) _ =
    if op == AsgnOpNonBlocking then
        unexpected "non-blocking assignment operator"
    else if op /= AsgnOpEq then
        unexpected "binary assignment operator"
    else if mt /= Nothing then
        unexpected "timing modifier"
    else
        (e, rest)
    where
        unexpected surprise =
            parseError pos $ "unexpected " ++ surprise ++ " in declaration"
takeAsgn rest "" = (Nil, rest)
takeAsgn toks initReason =
    parseError (head toks) $
        initReason ++ " declaration is missing initialization"

takeCommaOrEnd :: [DeclToken] -> [DeclToken]
takeCommaOrEnd tokens =
    if isCommaOrEnd tok
        then toks
        else parseError tok "expected comma or end of declarations"
    where tok : toks = tokens

takeIdent :: [DeclToken] -> (Identifier, [DeclToken])
takeIdent (DTIdent _ x : rest) = (x, rest)
takeIdent tokens = parseError (head tokens) "expected identifier"


takeParamScope :: [DeclToken] -> ParamScope -> (ParamScope, [DeclToken])
takeParamScope (DTParamKW _ psc : toks) _   = (psc, toks)
takeParamScope                    toks  psc = (psc, toks)

takeParamType :: [DeclToken] -> (Bool, [DeclToken])
takeParamType (DTTypeDecl _ : toks) = (True , toks)
takeParamType                 toks  = (False, toks)


isAttr :: DeclToken -> Bool
isAttr DTAttr{} = True
isAttr _ = False

isAsgn :: DeclToken -> Bool
isAsgn DTAsgn{} = True
isAsgn _ = False

isIdent :: DeclToken -> Bool
isIdent DTIdent{} = True
isIdent _ = False

isComma :: DeclToken -> Bool
isComma DTComma{} = True
isComma _ = False

isCommaOrEnd :: DeclToken -> Bool
isCommaOrEnd DTEnd{} = True
isCommaOrEnd tok = isComma tok

isPorts :: DeclToken -> Bool
isPorts DTPorts{} = True
isPorts _ = False

isNet :: DeclToken -> Bool
isNet DTNet{} = True
isNet _ = False

tokPos :: DeclToken -> Position
tokPos (DTComma    p) = p
tokPos (DTAutoDim  p) = p
tokPos (DTConst    p) = p
tokPos (DTVar      p) = p
tokPos (DTAsgn     p _ _ _) = p
tokPos (DTRange    p _ _) = p
tokPos (DTIdent    p _) = p
tokPos (DTPSIdent  p _ _) = p
tokPos (DTCSIdent  p _ _ _) = p
tokPos (DTDir      p _) = p
tokPos (DTType     p _) = p
tokPos (DTNet      p _ _) = p
tokPos (DTParams   p _) = p
tokPos (DTPorts    p _) = p
tokPos (DTBit      p _) = p
tokPos (DTLHSBase  p _) = p
tokPos (DTDot      p _) = p
tokPos (DTSigning  p _) = p
tokPos (DTLifetime p _) = p
tokPos (DTAttr     p _) = p
tokPos (DTEnd      p _) = p
tokPos (DTParamKW  p _) = p
tokPos (DTTypeDecl p) = p
tokPos (DTTypeAsgn p) = p

class Loc t where
    parseError :: t -> String -> a

instance Loc Position where
    parseError pos msg = error $ show pos ++ ": Parse error: " ++ msg

instance Loc DeclToken where
    parseError = parseError . tokPos
