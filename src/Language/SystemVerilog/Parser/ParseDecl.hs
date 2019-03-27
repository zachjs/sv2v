{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 -
 - Advanced parser for declarations and module instantiations.
 -
 - This module exists because the SystemVerilog grammar has conflicts which
 - cannot be resolved by an LALR(1) parser. This module provides an interface
 - for parsing an list of "DeclTokens" into `Decl`s and/or `ModuleItem`s. This
 - works through a series of functions which have an greater lookahead for
 - resolving the conflicts.
 -
 - Consider the following two module declarations:
 -  module Test(one two, three [1:0], four);
 -  module Test(one two, three [1:0]  four);
 -
 - When `{one} two ,` is on the stack, it is impossible to know whether to A)
 - shift `three` to add to the current declaration list; or B) to reduce the
 - stack and begin a new port declaration; without looking ahead more than 1
 - token (even ignoring the fact that a range is itself multiple tokens).
 -
 - While I previous had some success dealing with conflicts in the parser with
 - increasingly convoluted grammars, this became more and more untenable as I
 - added support for more SystemVerilog constructs.
 -
 - Because of how liberal this parser is, the parser will accept some
 - syntactically invalid files. In the future, we may add some basic
 - type-checking to complain about malformed input files. However, we generally
 - assume that users have tested their code with commercial simulator before
 - running it through our tool.
 -}

module Language.SystemVerilog.Parser.ParseDecl
( DeclToken (..)
, parseDTsAsPortDecls
, parseDTsAsModuleItems
, parseDTsAsDecls
, parseDTsAsDecl
, parseDTsAsDeclOrAsgn
, parseDTsAsDeclsAndAsgns
) where

import Data.List (elemIndex, findIndex, findIndices)
import Data.Maybe (fromJust, mapMaybe)

import Language.SystemVerilog.AST

-- [PUBLIC]: combined (irregular) tokens for declarations
data DeclToken
    = DTComma
    | DTAsgn     AsgnOp Expr
    | DTAsgnNBlk (Maybe Timing) Expr
    | DTRange    Range
    | DTIdent    Identifier
    | DTDir      Direction
    | DTType     (Signing -> [Range] -> Type)
    | DTParams   [PortBinding]
    | DTInstance [PortBinding]
    | DTBit      Expr
    | DTConcat   [LHS]
    | DTDot      Identifier
    | DTSigning  Signing
    deriving (Show, Eq)


-- entrypoints besides `parseDTsAsDeclOrAsgn` use this to disallow `DTAsgnNBlk`
-- and `DTAsgn` with a binary assignment operator because we don't expect to see
-- those assignment oeprators in declarations
forbidNonEqAsgn :: [DeclToken] -> a -> a
forbidNonEqAsgn tokens =
    if any isNonEqAsgn tokens
        then error $ "decl tokens contain bad assignment operator: " ++ show tokens
        else id
    where
        isNonEqAsgn :: DeclToken -> Bool
        isNonEqAsgn (DTAsgnNBlk _ _) = True
        isNonEqAsgn (DTAsgn (AsgnOp _) _) = True
        isNonEqAsgn _ = False


-- [PUBLIC]: parser for module port declarations, including interface ports
-- Example: `input foo, bar, One inst`
parseDTsAsPortDecls :: [DeclToken] -> ([Identifier], [ModuleItem])
parseDTsAsPortDecls pieces =
    forbidNonEqAsgn pieces $
    if isSimpleList
        then (simpleIdents, [])
        else (portNames declarations, map MIDecl declarations)
    where
        commaIdxs = findIndices isComma pieces
        identIdxs = findIndices isIdent pieces
        isSimpleList =
            all even identIdxs &&
            all odd commaIdxs &&
            odd (length pieces) &&
            length pieces == length commaIdxs + length identIdxs

        simpleIdents = map extractIdent $ filter isIdent pieces
        declarations = parseDTsAsDecls pieces

        isComma :: DeclToken -> Bool
        isComma token = token == DTComma
        extractIdent = \(DTIdent x) -> x

        portNames :: [Decl] -> [Identifier]
        portNames items = mapMaybe portName items
        portName :: Decl -> Maybe Identifier
        portName (Variable _ _ ident _ _) = Just ident
        portName decl =
            error $ "unexpected non-variable port declaration: " ++ (show decl)


-- [PUBLIC]: parser for single (semicolon-terminated) declarations (including
-- parameters) and module instantiations
parseDTsAsModuleItems :: [DeclToken] -> [ModuleItem]
parseDTsAsModuleItems tokens =
    forbidNonEqAsgn tokens $
    if any isInstance tokens
        then parseDTsAsIntantiations tokens
        else map MIDecl $ parseDTsAsDecl tokens
    where
        isInstance :: DeclToken -> Bool
        isInstance (DTInstance _) = True
        isInstance _ = False


-- internal; parser for module instantiations
parseDTsAsIntantiations :: [DeclToken] -> [ModuleItem]
parseDTsAsIntantiations (DTIdent name : tokens) =
    if not (all isInstanceToken rest)
        then error $ "instantiations mixed with other items: " ++ (show rest)
        else step rest
    where
        step :: [DeclToken] -> [ModuleItem]
        step [] = error $ "unexpected end of instantiation list: " ++ (show tokens)
        step toks =
            Instance name params x mr p : follow
            where
                (inst, toks') = span (DTComma /=) toks
                (x, mr, p) = case inst of
                    [DTIdent a, DTRange s, DTInstance b] -> (a, Just s , b)
                    [DTIdent a,            DTInstance b] -> (a, Nothing, b)
                    _ -> error $ "unrecognized instantiation: " ++ show inst
                follow = x `seq` if null toks' then [] else step (tail toks')
        (params, rest) =
            case head tokens of
                DTParams ps -> (ps, tail tokens)
                _           -> ([],      tokens)
        isInstanceToken :: DeclToken -> Bool
        isInstanceToken (DTInstance _) = True
        isInstanceToken (DTRange _) = True
        isInstanceToken (DTIdent _) = True
        isInstanceToken DTComma = True
        isInstanceToken _ = False
parseDTsAsIntantiations tokens =
    error $
        "DeclTokens contain instantiations, but start with non-ident: "
        ++ (show tokens)


-- [PUBLIC]: parser for generic, comma-separated declarations
parseDTsAsDecls :: [DeclToken] -> [Decl]
parseDTsAsDecls tokens =
    forbidNonEqAsgn tokens $
    concat $ map finalize $ parseDTsAsComponents tokens


-- [PUBLIC]: used for "single" declarations, i.e., declarations appearing
-- outside of a port list
parseDTsAsDecl :: [DeclToken] -> [Decl]
parseDTsAsDecl tokens =
    forbidNonEqAsgn tokens $
    if length components /= 1
        then error $ "too many declarations: " ++ (show tokens)
        else finalize $ head components
    where components = parseDTsAsComponents tokens


-- [PUBLIC]: parser for single block item declarations or assign or arg-less
-- subroutine call statetments
parseDTsAsDeclOrAsgn :: [DeclToken] -> ([Decl], [Stmt])
parseDTsAsDeclOrAsgn [DTIdent f] = ([], [Subroutine f []])
parseDTsAsDeclOrAsgn tokens =
    if any isAsgnToken tokens || tripLookahead tokens
        then ([], [constructor lhs expr])
        else (parseDTsAsDecl tokens, [])
    where
        (constructor, expr) = case last tokens of
            DTAsgn     op e -> (AsgnBlk op, e)
            DTAsgnNBlk mt e -> (Asgn    mt, e)
            _ -> error $ "invalid block item decl or stmt: " ++ (show tokens)
        lhs = takeLHS $ init tokens

-- [PUBLIC]: parser for mixed comma-separated declaration and assignment lists;
-- the main use case is for `for` loop initialization lists
parseDTsAsDeclsAndAsgns :: [DeclToken] -> [Either Decl (LHS, Expr)]
parseDTsAsDeclsAndAsgns [] = []
parseDTsAsDeclsAndAsgns tokens =
    if hasLeadingAsgn || tripLookahead tokens
        then
            let (lhsToks, l0) = break isDTAsgn tokens
                lhs = takeLHS lhsToks
                DTAsgn AsgnOpEq expr : l1 = l0
                asgn = Right (lhs, expr)
            in case l1 of
                DTComma : remaining -> asgn : parseDTsAsDeclsAndAsgns remaining
                [] -> [asgn]
                _ -> error $ "bad decls and asgns tokens: " ++ show tokens
        else
            let (component, remaining) = parseDTsAsComponent tokens
                decls = finalize component
            in (map Left decls) ++ parseDTsAsDeclsAndAsgns remaining
    where
        hasLeadingAsgn =
            -- if there is an asgn token before the next comma
            case (elemIndex DTComma tokens, findIndex isAsgnToken tokens) of
                (Just a, Just b) -> a > b
                (Nothing, Just _) -> True
                _ -> False
        isDTAsgn :: DeclToken -> Bool
        isDTAsgn (DTAsgn _ _) = True
        isDTAsgn _ = False

isAsgnToken :: DeclToken -> Bool
isAsgnToken (DTBit             _) = True
isAsgnToken (DTConcat          _) = True
isAsgnToken (DTAsgnNBlk      _ _) = True
isAsgnToken (DTAsgn (AsgnOp _) _) = True
isAsgnToken _ = False

takeLHS :: [DeclToken] -> LHS
takeLHS tokens = fromJust $ foldl takeLHSStep Nothing tokens

takeLHSStep :: Maybe LHS -> DeclToken -> Maybe LHS
takeLHSStep (Nothing  ) (DTConcat lhss) = Just $ LHSConcat lhss
takeLHSStep (Nothing  ) (DTIdent  x   ) = Just $ LHSIdent x
takeLHSStep (Just curr) (DTBit    e   ) = Just $ LHSBit   curr e
takeLHSStep (Just curr) (DTRange  r   ) = Just $ LHSRange curr r
takeLHSStep (Just curr) (DTDot    x   ) = Just $ LHSDot curr x
takeLHSStep (maybeCurr) token =
    error $ "unexpected token in LHS: " ++ show (maybeCurr, token)


-- batches together seperate declaration lists
type Triplet = (Identifier, [Range], Maybe Expr)
type Component = (Direction, Type, [Triplet])
finalize :: Component -> [Decl]
finalize (dir, typ, trips) =
    map (\(x, a, me) -> Variable dir typ x a me) trips


-- internal; entrypoint of the critical portion of our parser
parseDTsAsComponents :: [DeclToken] -> [Component]
parseDTsAsComponents [] = []
parseDTsAsComponents tokens =
    component : parseDTsAsComponents tokens'
    where
        (component, tokens') = parseDTsAsComponent tokens

parseDTsAsComponent :: [DeclToken] -> (Component, [DeclToken])
parseDTsAsComponent [] = error "parseDTsAsComponent unexpected end of tokens"
parseDTsAsComponent l0 =
    (component, l4)
    where
        (dir, l1) = takeDir    l0
        (tf , l2) = takeType   l1
        (rs , l3) = takeRanges l2
        (tps, l4) = takeTrips  l3 True
        component = (dir, tf rs, tps)


takeTrips :: [DeclToken] -> Bool -> ([Triplet], [DeclToken])
takeTrips [] True = error "incomplete declaration"
takeTrips [] False = ([], [])
takeTrips l0 force =
    if not force && not (tripLookahead l0)
        then ([], l0)
        else (trip : trips, l5)
    where
        (x , l1) = takeIdent  l0
        (a , l2) = takeRanges l1
        (me, l3) = takeAsgn   l2
        (_ , l4) = takeComma  l3
        trip = (x, a, me)
        (trips, l5) = takeTrips l4 False

tripLookahead :: [DeclToken] -> Bool
tripLookahead [] = False
tripLookahead l0 =
    -- every triplet *must* begin with an identifier
    if not (isIdent $ head l0) then
        False
    -- if the identifier is the last token, or if it assigned a value, then we
    -- know we must have a valid triplet ahead
    else if null l1 || asgn /= Nothing then
        True
    -- if there is a comma after the identifier (and optional ranges and
    -- assignment) that we're looking at, then we know this identifier is not a
    -- type name, as type names must be followed by a first identifier before a
    -- comma or the end of the list
    else
        (not $ null l3) && (head l3 == DTComma)
    where
        (_   , l1) = takeIdent  l0
        (_   , l2) = takeRanges l1
        (asgn, l3) = takeAsgn   l2

takeDir :: [DeclToken] -> (Direction, [DeclToken])
takeDir (DTDir dir : rest) = (dir  , rest)
takeDir              rest  = (Local, rest)

takeType :: [DeclToken] -> ([Range] -> Type, [DeclToken])
takeType (DTIdent a  : DTDot b      : rest) = (InterfaceT a (Just b),                       rest)
takeType (DTType  tf : DTSigning sg : rest) = (tf       sg         ,                        rest)
takeType (DTType  tf                : rest) = (tf       Unspecified,                        rest)
takeType (DTSigning sg              : rest) = (Implicit sg         ,                        rest)
takeType (DTIdent tn : DTComma      : rest) = (Implicit Unspecified, DTIdent tn : DTComma : rest)
takeType (DTIdent tn                : [  ]) = (Implicit Unspecified, DTIdent tn           : [  ])
takeType (DTIdent tn                : rest) = (Alias tn            ,                        rest)
takeType                              rest  = (Implicit Unspecified,                        rest)

takeRanges :: [DeclToken] -> ([Range], [DeclToken])
takeRanges [] = ([], [])
takeRanges (token : tokens) =
    case token of
        DTRange r -> (r         : rs, rest          )
        DTBit   s -> (asRange s : rs, rest          )
        _         -> ([]            , token : tokens)
    where
        (rs, rest) = takeRanges tokens
        asRange s = (simplify $ BinOp Sub s (Number "1"), Number "0")

-- Matching DTAsgnNBlk here allows tripLookahead to work both for standard
-- declarations and in `parseDTsAsDeclOrAsgn`, where we're checking for an
-- assignment assignment statement. The other entry points disallow
-- `DTAsgnNBlk`, so this doesn't liberalize the parser.
takeAsgn :: [DeclToken] -> (Maybe Expr, [DeclToken])
takeAsgn (DTAsgn AsgnOpEq e : rest) = (Just e , rest)
takeAsgn (DTAsgnNBlk    _ e : rest) = (Just e , rest)
takeAsgn                 rest  = (Nothing, rest)

takeComma :: [DeclToken] -> (Bool, [DeclToken])
takeComma [] = (False, [])
takeComma (DTComma : rest) = (True, rest)
takeComma _ = error "take comma encountered neither comma nor end of tokens"

takeIdent :: [DeclToken] -> (Identifier, [DeclToken])
takeIdent (DTIdent x : rest) = (x, rest)
takeIdent tokens = error $ "takeIdent didn't find identifier: " ++ show tokens


isIdent :: DeclToken -> Bool
isIdent (DTIdent _) = True
isIdent _ = False
