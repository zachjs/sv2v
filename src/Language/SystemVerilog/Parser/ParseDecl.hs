{-# LANGUAGE PatternSynonyms #-}
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
 - This parser is very liberal, and so accepts some syntactically invalid files.
 - In the future, we may add some basic type-checking to complain about
 - malformed input files. However, we generally assume that users have tested
 - their code with a commercial simulator before running it through our tool.
 -}

module Language.SystemVerilog.Parser.ParseDecl
( DeclToken (..)
, parseDTsAsPortDecls
, parseDTsAsModuleItems
, parseDTsAsDecls
, parseDTsAsDeclOrStmt
, parseDTsAsDeclsOrAsgns
) where

import Data.List (findIndex, findIndices, partition, uncons)

import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser.Tokens (Position(..))

-- [PUBLIC]: combined (irregular) tokens for declarations
data DeclToken
    = DTComma    Position
    | DTAutoDim  Position
    | DTConst    Position
    | DTVar      Position
    | DTAsgn     Position AsgnOp (Maybe Timing) Expr
    | DTRange    Position (PartSelectMode, Range)
    | DTIdent    Position Identifier
    | DTPSIdent  Position Identifier Identifier
    | DTCSIdent  Position Identifier [ParamBinding] Identifier
    | DTDir      Position Direction
    | DTType     Position (Signing -> [Range] -> Type)
    | DTNet      Position NetType Strength
    | DTParams   Position [ParamBinding]
    | DTPorts    Position [PortBinding]
    | DTBit      Position Expr
    | DTConcat   Position [LHS]
    | DTStream   Position StreamOp Expr [LHS]
    | DTDot      Position Identifier
    | DTSigning  Position Signing
    | DTLifetime Position Lifetime
    | DTAttr     Position Attr
    deriving (Show, Eq)

-- entrypoints besides `parseDTsAsDeclOrStmt` use this to disallow `DTAsgn` with
-- a non-blocking operator, binary assignment operator, or a timing control
-- because we don't expect to see those assignment operators in declarations
forbidNonEqAsgn :: [DeclToken] -> a -> a
forbidNonEqAsgn [] = id
forbidNonEqAsgn (tok @ (DTAsgn _ op mt _) : toks) =
    if op /= AsgnOpEq then
        parseError tok $ "unexpected " ++ opKind
            ++ " assignment operator in declaration"
    else if mt /= Nothing then
        parseError tok "unexpected timing modifier in declaration"
    else
        forbidNonEqAsgn toks
    where opKind = if op == AsgnOpNonBlocking then "non-blocking" else "binary"
forbidNonEqAsgn (_ : toks) = forbidNonEqAsgn toks


-- [PUBLIC]: parser for module port declarations, including interface ports
-- Example: `input foo, bar, One inst`
parseDTsAsPortDecls :: [DeclToken] -> ([Identifier], [ModuleItem])
parseDTsAsPortDecls pieces =
    parseDTsAsPortDecls' $
    case last pieces of
        DTComma{} -> init pieces
        _ -> pieces

-- internal parseDTsAsPortDecls after the removal of an optional trailing comma
parseDTsAsPortDecls' :: [DeclToken] -> ([Identifier], [ModuleItem])
parseDTsAsPortDecls' pieces =
    forbidNonEqAsgn pieces `seq`
    if isSimpleList
        then (simpleIdents, [])
        else (portNames declarations, applyAttrs [] pieces declarations)
    where
        commaIdxs = findIndices isComma pieces
        identIdxs = findIndices isIdent pieces
        isSimpleList =
            all even identIdxs &&
            all odd commaIdxs &&
            odd (length pieces) &&
            length pieces == length commaIdxs + length identIdxs

        simpleIdents = map extractIdent $ filter isIdent pieces
        declarations = propagateDirections Input $ parseDTsAsDecls pieces'

        extractIdent = \(DTIdent _ x) -> x

        pieces' = filter (not . isDTAttr) pieces
        isDTAttr :: DeclToken -> Bool
        isDTAttr DTAttr{} = True
        isDTAttr _ = False

        propagateDirections :: Direction -> [Decl] -> [Decl]
        propagateDirections dir (decl @ (Variable _ InterfaceT{} _ _ _) : decls) =
            decl : propagateDirections dir decls
        propagateDirections lastDir (Variable currDir t x a e : decls) =
            decl : propagateDirections dir decls
            where
                decl = Variable dir t x a e
                dir = if currDir == Local then lastDir else currDir
        propagateDirections lastDir (Net currDir n s t x a e : decls) =
            decl : propagateDirections dir decls
            where
                decl = Net dir n s t x a e
                dir = if currDir == Local then lastDir else currDir
        propagateDirections dir (decl : decls) =
            decl : propagateDirections dir decls
        propagateDirections _ [] = []

        portNames :: [Decl] -> [Identifier]
        portNames = filter (not . null) . map portName
        portName :: Decl -> Identifier
        portName (Variable _ _ ident _ _) = ident
        portName (Net  _ _ _ _ ident _ _) = ident
        portName CommentDecl{} = ""
        portName decl =
            error $ "unexpected non-variable port declaration: " ++ (show decl)

        applyAttrs :: [Attr] -> [DeclToken] -> [Decl] -> [ModuleItem]
        applyAttrs _ [] [] = []
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
        applyAttrs _ [] _ = error "applyAttrs internal invariant failed"

        wrapDecl :: [Attr] -> Decl -> ModuleItem
        wrapDecl attrs decl = foldr MIAttr (MIPackageItem $ Decl decl) attrs


-- [PUBLIC]: parser for single (semicolon-terminated) declarations (including
-- parameters) and module instantiations
parseDTsAsModuleItems :: [DeclToken] -> [ModuleItem]
parseDTsAsModuleItems tokens =
    forbidNonEqAsgn tokens `seq`
    if isElabTask $ head tokens then
        asElabTask tokens
    else if any isPorts tokens then
        parseDTsAsIntantiations tokens
    else
        map (MIPackageItem . Decl) $ parseDTsAsDecl tokens
    where
        isElabTask :: DeclToken -> Bool
        isElabTask (DTIdent _ x) = elem x elabTasks
            where elabTasks = ["$fatal", "$error", "$warning", "$info"]
        isElabTask _ = False

-- internal; approximates the behavior of the elaboration system tasks
asElabTask :: [DeclToken] -> [ModuleItem]
asElabTask [DTIdent _ name, DTPorts _ args] =
    if name == "$info"
        then [] -- just drop them for simplicity
        else [Instance "ThisModuleDoesNotExist" [] name' [] args]
    where name' = "__sv2v_elab_" ++ tail name
asElabTask [DTIdent pos name] =
    asElabTask [DTIdent pos name, DTPorts pos []]
asElabTask tokens =
    error $ "could not parse elaboration system task: " ++ show tokens


-- internal; parser for module instantiations
parseDTsAsIntantiations :: [DeclToken] -> [ModuleItem]
parseDTsAsIntantiations (DTIdent _ name : DTParams _ params : tokens) =
    step tokens
    where
        step :: [DeclToken] -> [ModuleItem]
        step [] = parseError endTok "unexpected end of instantiation list"
        step toks = inst : rest
            where
                (delimTok, rest) =
                    if null restToks
                        then (endTok, [])
                        else (head restToks, step $ tail restToks)
                inst = Instance name params x rs p
                (x, rs, p) = parseDTsAsIntantiation instToks delimTok
                (instToks, restToks) = break isComma toks
        -- TODO: all public interfaces should take the ending token
        endTok = last tokens
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
        parseError delimTok "expected instantiation before delimiter"
    else if not (isIdent nameTok) then
        parseError nameTok "expected instantiation name"
    else if null l1 then
        parseError delimTok "expected port connections before delimiter"
    else if seq ranges not (isPorts portsTok) then
        parseError portsTok "expected port connections"
    else
        (name, ranges, ports)
    where
        Just (nameTok, l1) = uncons l0
        rangeToks = init l1
        portsTok = last l1
        DTIdent _ name = nameTok
        DTPorts _ ports = portsTok
        ranges = map asRange rangeToks
        asRange :: DeclToken -> Range
        asRange (DTRange _ (NonIndexed, s)) = s
        asRange (DTBit _ s) = (RawNum 0, BinOp Sub s (RawNum 1))
        asRange tok = parseError tok "expected instantiation dimensions"


-- [PUBLIC]: parser for generic, comma-separated declarations
parseDTsAsDecls :: [DeclToken] -> [Decl]
parseDTsAsDecls tokens =
    forbidNonEqAsgn tokens `seq`
    concat $ map finalize $ parseDTsAsComponents tokens


-- internal; used for "single" declarations, i.e., declarations appearing
-- outside of a port list
parseDTsAsDecl :: [DeclToken] -> [Decl]
parseDTsAsDecl tokens =
    if length components /= 1
        then parseError tok $ "unexpected comma-separated declarations"
        else finalize $ head components
    where
        components = parseDTsAsComponents tokens
        _ : (pos, _, _) : _ = components
        tok = DTComma pos


-- [PUBLIC]: parser for single block item declarations or assign or arg-less
-- subroutine call statements
parseDTsAsDeclOrStmt :: [DeclToken] -> ([Decl], [Stmt])
parseDTsAsDeclOrStmt (DTAsgn pos (AsgnOp op) mt e : tok : toks) =
    parseDTsAsDeclOrStmt $ (tok : toks) ++ [DTAsgn pos (AsgnOp op) mt e]
parseDTsAsDeclOrStmt tokens =
    if not hasLeadingDecl
        then ([], [traceStmt pos, stmt])
        else (parseDTsAsDecl tokens, [])
    where
        pos = tokPos $ last tokens
        stmt = case last tokens of
            DTAsgn  _ op mt e -> Asgn op mt lhs e
            DTPorts _ ports -> asSubroutine lhsToks (portsToArgs ports)
            _ -> asSubroutine tokens (Args [] [])
        lhsToks = init tokens
        lhs = case takeLHS lhsToks of
            Nothing -> error $ "could not parse as LHS: " ++ show lhsToks
            Just l -> l
        hasLeadingDecl = tokens /= l5 && tripLookahead l5
        (_, l1) = takeDir      tokens
        (_, l2) = takeLifetime l1
        (_, l3) = takeVarOrNet l2
        (_, l4) = takeType     l3
        (_, l5) = takeRanges   l4

traceStmt :: Position -> Stmt
traceStmt pos = CommentStmt $ "Trace: " ++ show pos

-- read the given tokens as the root of a subroutine invocation
asSubroutine :: [DeclToken] -> Args -> Stmt
asSubroutine [DTIdent   _     x] = Subroutine $ Ident       x
asSubroutine [DTPSIdent _ p   x] = Subroutine $ PSIdent p   x
asSubroutine [DTCSIdent _ c p x] = Subroutine $ CSIdent c p x
asSubroutine tokens =
    case takeLHS tokens of
        Just lhs -> Subroutine $ lhsToExpr lhs
        Nothing -> error $ "invalid block item decl or stmt: " ++ show tokens

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
    forbidNonEqAsgn tokens `seq`
    if hasLeadingAsgn || tripLookahead tokens
        then Right $ parseDTsAsAsgns tokens
        else Left $ map checkDecl $ parseDTsAsDecls tokens
    where
        hasLeadingAsgn =
            -- if there is an asgn token before the next comma
            case (findIndex isComma tokens, findIndex isAsgnToken tokens) of
                (Just a, Just b) -> a > b
                (Nothing, Just _) -> True
                _ -> False
        checkDecl :: Decl -> Decl
        checkDecl (decl @ (Variable _ _ _ _ Nil)) =
            error $ "for loop declaration missing initialization: "
                ++ init (show decl)
        checkDecl decl = decl

-- internal parser for basic assignment lists
parseDTsAsAsgns :: [DeclToken] -> [(LHS, Expr)]
parseDTsAsAsgns tokens =
    case l1 of
        [] -> [asgn]
        DTComma{} : remaining -> asgn : parseDTsAsAsgns remaining
        _ -> error $ "bad assignment tokens: " ++ show tokens
    where
        (lhsToks, l0) = break isDTAsgn tokens
        lhs = case takeLHS lhsToks of
            Nothing -> error $ "could not parse as LHS: " ++ show lhsToks
            Just l -> l
        DTAsgn _ AsgnOpEq Nothing expr : l1 = l0
        asgn = (lhs, expr)

        isDTAsgn :: DeclToken -> Bool
        isDTAsgn (DTAsgn _ _ Nothing _) = True
        isDTAsgn _ = False

isAsgnToken :: DeclToken -> Bool
isAsgnToken (DTBit{}   ) = True
isAsgnToken (DTConcat{}) = True
isAsgnToken (DTStream{}) = True
isAsgnToken (DTDot{}   ) = True
isAsgnToken (DTAsgn _ op _ _) = op /= AsgnOpEq
isAsgnToken _ = False

takeLHS :: [DeclToken] -> Maybe LHS
takeLHS [] = Nothing
takeLHS (t : ts) =
    foldl takeLHSStep (takeLHSStart t) ts

takeLHSStart :: DeclToken -> Maybe LHS
takeLHSStart (DTConcat _     lhss) = Just $ LHSConcat lhss
takeLHSStart (DTStream _ o e lhss) = Just $ LHSStream o e lhss
takeLHSStart (DTIdent  _ x       ) = Just $ LHSIdent x
takeLHSStart _ = Nothing

takeLHSStep :: Maybe LHS -> DeclToken -> Maybe LHS
takeLHSStep (Just curr) (DTBit   _  e   ) = Just $ LHSBit   curr e
takeLHSStep (Just curr) (DTRange _ (m,r)) = Just $ LHSRange curr m r
takeLHSStep (Just curr) (DTDot   _  x   ) = Just $ LHSDot curr x
takeLHSStep _ _ = Nothing


-- batches together separate declaration lists
type DeclBase = Identifier -> [Range] -> Expr -> Decl
type Triplet = (Identifier, [Range], Expr)
type Component = (Position, DeclBase, [Triplet])
finalize :: Component -> [Decl]
finalize (pos, base, trips) =
    CommentDecl ("Trace: " ++ show pos) :
    map (\(x, a, e) -> base x a e) trips


-- internal; entrypoint of the critical portion of our parser
parseDTsAsComponents :: [DeclToken] -> [Component]
parseDTsAsComponents [] = []
parseDTsAsComponents tokens =
    component : parseDTsAsComponents tokens'
    where (component, tokens') = parseDTsAsComponent tokens

parseDTsAsComponent :: [DeclToken] -> (Component, [DeclToken])
parseDTsAsComponent [] = error "parseDTsAsComponent unexpected end of tokens"
parseDTsAsComponent l0 =
    if l /= Nothing && l /= Just Automatic then
        error $ "unexpected non-automatic lifetime: " ++ show l0
    else if dir == Local && length l2 == length l5 then
        error $ "declaration(s) missing type information: "
            ++ show (position, tps)
    else
        (component, l6)
    where
        (dir, l1) = takeDir      l0
        (l  , l2) = takeLifetime l1
        (von, l3) = takeVarOrNet l2
        (tf , l4) = takeType     l3
        (rs , l5) = takeRanges   l4
        (tps, l6) = takeTrips    l5 True
        position = tokPos $ head l0
        base = von dir $ tf rs
        component = (position, base, tps)

takeTrips :: [DeclToken] -> Bool -> ([Triplet], [DeclToken])
takeTrips [] True = error "incomplete declaration"
takeTrips [] False = ([], [])
takeTrips l0 force =
    if not force && not (tripLookahead l0)
        then ([], l0)
        else (trip : trips, l5)
    where
        (x, l1) = takeIdent  l0
        (a, l2) = takeRanges l1
        (e, l3) = takeAsgn   l2
        (_, l4) = takeComma  l3
        trip = (x, a, e)
        (trips, l5) = takeTrips l4 False

tripLookahead :: [DeclToken] -> Bool
tripLookahead [] = False
tripLookahead l0 =
    -- every triplet *must* begin with an identifier
    if not (isIdent $ head l0) then
        False
    -- if the identifier is the last token, or if it assigned a value, then we
    -- know we must have a valid triplet ahead
    else if null l1 || asgn /= Nil then
        True
    -- if there is an ident followed by some number of ranges, and that's it,
    -- then there is a trailing declaration of an array ahead
    else if (not $ null l1) && (null l2) then
        True
    -- if there is a comma after the identifier (and optional ranges and
    -- assignment) that we're looking at, then we know this identifier is not a
    -- type name, as type names must be followed by a first identifier before a
    -- comma or the end of the list
    else
        (not $ null l3) && (isComma $ head l3)
    where
        (_   , l1) = takeIdent  l0
        (_   , l2) = takeRanges l1
        (asgn, l3) = takeAsgn   l2

takeDir :: [DeclToken] -> (Direction, [DeclToken])
takeDir (DTDir _ dir : rest) = (dir  , rest)
takeDir                rest  = (Local, rest)

takeLifetime :: [DeclToken] -> (Maybe Lifetime, [DeclToken])
takeLifetime (DTLifetime _ l : rest) = (Just  l, rest)
takeLifetime                   rest  = (Nothing, rest)

takeVarOrNet :: [DeclToken] -> (Direction -> Type -> DeclBase, [DeclToken])
takeVarOrNet (DTConst{} : tok @ DTConst{} : _) =
    parseError tok "duplicate const modifier"
takeVarOrNet (DTConst _ : tokens) = takeVarOrNet tokens
takeVarOrNet (DTNet _ n s : tokens) = (\d -> Net d n s, tokens)
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
takeType (DTVar{} : tok @ DTVar{} : _) =
    parseError tok "duplicate var modifier"
takeType (DTVar _ : rest) =
    case tf [] of
        Implicit sg [] -> (IntegerVector TLogic sg, rest')
        _ -> (tf, rest')
    where (tf, rest') = takeType rest
takeType rest = (Implicit Unspecified, rest)

takeRanges :: [DeclToken] -> ([Range], [DeclToken])
takeRanges [] = ([], [])
takeRanges (token : tokens) =
    case token of
        DTRange _ (NonIndexed, r) -> (r         : rs, rest          )
        DTBit   _ s               -> (asRange s : rs, rest          )
        DTAutoDim _               ->
            case rest of
                (DTAsgn _ AsgnOpEq Nothing (Pattern l) : _) -> autoDim l
                (DTAsgn _ AsgnOpEq Nothing (Concat  l) : _) -> autoDim l
                _ ->                 ([]            , token : tokens)
        _                         -> ([]            , token : tokens)
    where
        (rs, rest) = takeRanges tokens
        asRange s = (RawNum 0, BinOp Sub s (RawNum 1))
        autoDim :: [a] -> ([Range], [DeclToken])
        autoDim l =
            ((lo, hi) : rs, rest)
            where
                n = length l
                lo = RawNum 0
                hi = RawNum $ fromIntegral $ n - 1

-- Matching `AsgnOpEq` and `AsgnOpNonBlocking` here allows tripLookahead to work
-- both for standard declarations and in `parseDTsAsDeclOrStmt`, where we're
-- checking for an assignment statement. The other entry points disallow
-- `AsgnOpNonBlocking`, so this doesn't liberalize the parser.
takeAsgn :: [DeclToken] -> (Expr, [DeclToken])
takeAsgn (DTAsgn _ op Nothing e : rest) =
    if op == AsgnOpEq || op == AsgnOpNonBlocking
        then (e  , rest)
        else (Nil, rest)
takeAsgn rest = (Nil, rest)

takeComma :: [DeclToken] -> (Bool, [DeclToken])
takeComma [] = (False, [])
takeComma (DTComma{} : rest) = (True, rest)
takeComma toks = error $ "expected comma or end of decl, got: " ++ show toks

takeIdent :: [DeclToken] -> (Identifier, [DeclToken])
takeIdent (DTIdent _ x : rest) = (x, rest)
takeIdent tokens = error $ "takeIdent didn't find identifier: " ++ show tokens


isIdent :: DeclToken -> Bool
isIdent (DTIdent{}) = True
isIdent _ = False

isComma :: DeclToken -> Bool
isComma (DTComma{}) = True
isComma _ = False

isPorts :: DeclToken -> Bool
isPorts DTPorts{} = True
isPorts _ = False

tokPos :: DeclToken -> Position
tokPos (DTComma    p) = p
tokPos (DTAutoDim  p) = p
tokPos (DTConst    p) = p
tokPos (DTVar      p) = p
tokPos (DTAsgn     p _ _ _) = p
tokPos (DTRange    p _) = p
tokPos (DTIdent    p _) = p
tokPos (DTPSIdent  p _ _) = p
tokPos (DTCSIdent  p _ _ _) = p
tokPos (DTDir      p _) = p
tokPos (DTType     p _) = p
tokPos (DTNet      p _ _) = p
tokPos (DTParams   p _) = p
tokPos (DTPorts    p _) = p
tokPos (DTBit      p _) = p
tokPos (DTConcat   p _) = p
tokPos (DTStream   p _ _ _) = p
tokPos (DTDot      p _) = p
tokPos (DTSigning  p _) = p
tokPos (DTLifetime p _) = p
tokPos (DTAttr     p _) = p

parseError :: DeclToken -> String -> a
parseError tok msg = error $ show pos ++ ": Parse error: " ++ msg
    where pos = tokPos tok
