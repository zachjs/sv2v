{
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Original Lexer Author: Tom Hawkins <tomahawkins@gmail.com>
 -
 - Combined source lexing and preprocessing
 -
 - These procedures are combined so that we can simultaneously process macros in
 - a sane way (something analogous to character-by-character) and have our
 - lexemes properly tagged with source file positions.
 -
 - The scariest piece of this module is the use of `unsafePerformIO`. We want to
 - be able to search for and read files whenever we see an include directive.
 - Trying to thread the IO Monad through alex's interface would be very
 - convoluted. The operations performed are not effectful, and are type safe.
 -}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- The above pragma gets rid of annoying warning caused by alex 3.2.4. This has
-- been fixed on their development branch, so this can be removed once they roll
-- a new release. (no new release as of 3/29/2018)
module Language.SystemVerilog.Parser.Lex (lexFile) where

import System.FilePath (dropFileName)
import System.Directory (findFile)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map
import Data.List (findIndex, isPrefixOf)

import Language.SystemVerilog.Parser.Tokens
}

%wrapper "monadUserState"

-- Numbers

$nonZeroDecimalDigit = [1-9]
$decimalDigit = [0-9]
@xDigit       = [xX]
@zDigit       = [zZ\?]
@binaryDigit  = @xDigit | @zDigit | [0-1]
@octalDigit   = @xDigit | @zDigit | [0-7]
@hexDigit     = @xDigit | @zDigit | [0-9a-fA-F]

@decimalBase = "'" [sS]? [dD]
@binaryBase  = "'" [sS]? [bB]
@octalBase   = "'" [sS]? [oO]
@hexBase     = "'" [sS]? [hH]

@binaryValue = @binaryDigit ("_" | @binaryDigit)*
@octalValue  = @octalDigit  ("_" | @octalDigit)*
@hexValue    = @hexDigit    ("_" | @hexDigit)*

@unsignedNumber = $decimalDigit ("_" | $decimalDigit)*

@sign = [\-\+]
@fixedPointNumber    = @unsignedNumber "." @unsignedNumber
@floatingPointNumber = @unsignedNumber ("." @unsignedNumber)? [eE] @sign? @unsignedNumber

@size = @unsignedNumber " "?

@decimalNumber = @size? @decimalBase " "? @unsignedNumber
@binaryNumber  = @size? @binaryBase  " "? @binaryValue
@octalNumber   = @size? @octalBase   " "? @octalValue
@hexNumber     = @size? @hexBase     " "? @hexValue
@realNumber    = @fixedPointNumber | @floatingPointNumber

@unbasedUnsizedLiteral = "'" ( 0 | 1 | x | X | z | Z )

@number
    = @unsignedNumber
    | @decimalNumber
    | @octalNumber
    | @binaryNumber
    | @hexNumber
    | @unbasedUnsizedLiteral
    | @realNumber

-- Strings

@string = \" (\\\"|[^\"\r\n])* \"

-- Identifiers

@escapedIdentifier = "\" ($printable # $white)+ $white
@simpleIdentifier  = [a-zA-Z_] [a-zA-Z0-9_\$]*
@systemIdentifier = "$" [a-zA-Z0-9_\$]+

-- Comments

@commentBlock = "/*"
@commentLine = "//"

-- Directives

@directive = "`" @simpleIdentifier

-- Whitespace

@newline = \n
@escapedNewline = \\\n
@whitespace = ($white # \n) | @escapedNewline

tokens :-

    "always"           { tok KW_always       }
    "always_comb"      { tok KW_always_comb  }
    "always_ff"        { tok KW_always_ff    }
    "always_latch"     { tok KW_always_latch }
    "and"              { tok KW_and          }
    "assign"           { tok KW_assign       }
    "automatic"        { tok KW_automatic    }
    "begin"            { tok KW_begin        }
    "bit"              { tok KW_bit          }
    "buf"              { tok KW_buf          }
    "byte"             { tok KW_byte         }
    "case"             { tok KW_case         }
    "casex"            { tok KW_casex        }
    "casez"            { tok KW_casez        }
    "default"          { tok KW_default      }
    "defparam"         { tok KW_defparam     }
    "do"               { tok KW_do           }
    "else"             { tok KW_else         }
    "end"              { tok KW_end          }
    "endcase"          { tok KW_endcase      }
    "endfunction"      { tok KW_endfunction  }
    "endgenerate"      { tok KW_endgenerate  }
    "endinterface"     { tok KW_endinterface }
    "endmodule"        { tok KW_endmodule    }
    "endtask"          { tok KW_endtask      }
    "enum"             { tok KW_enum         }
    "extern"           { tok KW_extern       }
    "for"              { tok KW_for          }
    "forever"          { tok KW_forever      }
    "function"         { tok KW_function     }
    "generate"         { tok KW_generate     }
    "genvar"           { tok KW_genvar       }
    "if"               { tok KW_if           }
    "initial"          { tok KW_initial      }
    "inout"            { tok KW_inout        }
    "input"            { tok KW_input        }
    "int"              { tok KW_int          }
    "integer"          { tok KW_integer      }
    "interface"        { tok KW_interface    }
    "localparam"       { tok KW_localparam   }
    "logic"            { tok KW_logic        }
    "longint"          { tok KW_longint      }
    "modport"          { tok KW_modport      }
    "module"           { tok KW_module       }
    "nand"             { tok KW_nand         }
    "negedge"          { tok KW_negedge      }
    "nor"              { tok KW_nor          }
    "not"              { tok KW_not          }
    "or"               { tok KW_or           }
    "output"           { tok KW_output       }
    "packed"           { tok KW_packed       }
    "parameter"        { tok KW_parameter    }
    "posedge"          { tok KW_posedge      }
    "real"             { tok KW_real         }
    "realtime"         { tok KW_realtime     }
    "reg"              { tok KW_reg          }
    "repeat"           { tok KW_repeat       }
    "return"           { tok KW_return       }
    "shortint"         { tok KW_shortint     }
    "shortreal"        { tok KW_shortreal    }
    "signed"           { tok KW_signed       }
    "static"           { tok KW_static       }
    "struct"           { tok KW_struct       }
    "supply0"          { tok KW_supply0      }
    "supply1"          { tok KW_supply1      }
    "task"             { tok KW_task         }
    "time"             { tok KW_time         }
    "tri"              { tok KW_tri          }
    "tri0"             { tok KW_tri0         }
    "tri1"             { tok KW_tri1         }
    "triand"           { tok KW_triand       }
    "trior"            { tok KW_trior        }
    "trireg"           { tok KW_trireg       }
    "typedef"          { tok KW_typedef      }
    "unique"           { tok KW_unique       }
    "unsigned"         { tok KW_unsigned     }
    "uwire"            { tok KW_uwire        }
    "wand"             { tok KW_wand         }
    "while"            { tok KW_while        }
    "wire"             { tok KW_wire         }
    "wor"              { tok KW_wor          }
    "xnor"             { tok KW_xnor         }
    "xor"              { tok KW_xor          }

    @simpleIdentifier  { tok Id_simple  }
    @escapedIdentifier { tok Id_escaped }
    @systemIdentifier  { tok Id_system  }

    @number            { tok Lit_number }
    @string            { tok Lit_string }

    "("                { tok Sym_paren_l }
    ")"                { tok Sym_paren_r }
    "["                { tok Sym_brack_l }
    "]"                { tok Sym_brack_r }
    "{"                { tok Sym_brace_l }
    "}"                { tok Sym_brace_r }
    "~"                { tok Sym_tildy }
    "!"                { tok Sym_bang }
    "@"                { tok Sym_at }
    "#"                { tok Sym_pound }
    "%"                { tok Sym_percent }
    "^"                { tok Sym_hat }
    "&"                { tok Sym_amp }
    "|"                { tok Sym_bar }
    "*"                { tok Sym_aster }
    "."                { tok Sym_dot }
    ","                { tok Sym_comma }
    ":"                { tok Sym_colon }
    ";"                { tok Sym_semi }
    "="                { tok Sym_eq }
    "<"                { tok Sym_lt }
    ">"                { tok Sym_gt }
    "+"                { tok Sym_plus }
    "-"                { tok Sym_dash }
    "?"                { tok Sym_question }
    "/"                { tok Sym_slash }
    "$"                { tok Sym_dollar }
    "'"                { tok Sym_s_quote }

    "~&"               { tok Sym_tildy_amp }
    "~|"               { tok Sym_tildy_bar }
    "~^"               { tok Sym_tildy_hat }
    "^~"               { tok Sym_hat_tildy }
    "=="               { tok Sym_eq_eq }
    "!="               { tok Sym_bang_eq }
    "&&"               { tok Sym_amp_amp }
    "||"               { tok Sym_bar_bar }
    "**"               { tok Sym_aster_aster }
    "<="               { tok Sym_lt_eq }
    ">="               { tok Sym_gt_eq }
    ">>"               { tok Sym_gt_gt }
    "<<"               { tok Sym_lt_lt }
    "++"               { tok Sym_plus_plus }
    "--"               { tok Sym_dash_dash }
    "+="               { tok Sym_plus_eq }
    "-="               { tok Sym_dash_eq }
    "*="               { tok Sym_aster_eq }
    "/="               { tok Sym_slash_eq }
    "%="               { tok Sym_percent_eq }
    "&="               { tok Sym_amp_eq }
    "|="               { tok Sym_bar_eq }
    "^="               { tok Sym_hat_eq }
    "+:"               { tok Sym_plus_colon }
    "-:"               { tok Sym_dash_colon }
    "::"               { tok Sym_colon_colon }
    ".*"               { tok Sym_dot_aster }
    "->"               { tok Sym_dash_gt }
    ":="               { tok Sym_colon_eq }
    ":/"               { tok Sym_colon_slash }
    "##"               { tok Sym_pound_pound }
    "[*"               { tok Sym_brack_l_aster }
    "[="               { tok Sym_brack_l_eq }
    "=>"               { tok Sym_eq_gt }
    "@*"               { tok Sym_at_aster }
    "(*"               { tok Sym_paren_l_aster }
    "*)"               { tok Sym_aster_paren_r }
    "*>"               { tok Sym_aster_gt }

    "==="              { tok Sym_eq_eq_eq }
    "!=="              { tok Sym_bang_eq_eq }
    "==?"              { tok Sym_eq_eq_question }
    "!=?"              { tok Sym_bang_eq_question }
    ">>>"              { tok Sym_gt_gt_gt }
    "<<<"              { tok Sym_lt_lt_lt }
    "<<="              { tok Sym_lt_lt_eq }
    ">>="              { tok Sym_gt_gt_eq }
    "|->"              { tok Sym_bar_dash_gt }
    "|=>"              { tok Sym_bar_eq_gt }
    "[->"              { tok Sym_brack_l_dash_gt }
    "@@("              { tok Sym_at_at_paren_l }
    "(*)"              { tok Sym_paren_l_aster_paren_r }
    "->>"              { tok Sym_dash_gt_gt }
    "&&&"              { tok Sym_amp_amp_amp }

    "<<<="             { tok Sym_lt_lt_lt_eq }
    ">>>="             { tok Sym_gt_gt_gt_eq }

    @directive         { handleDirective }
    @commentLine       { removeUntil "\n" }
    @commentBlock      { removeUntil "*/" }

    $white             ;

    .                  { tok Unknown }

{

-- our actions don't return any data
type Action = AlexInput -> Int -> Alex ()

-- keeps track of the state of an if-else cascade level
data Cond
    = CurrentlyTrue
    | PreviouslyTrue
    | NeverTrue
    deriving (Eq, Show)

-- our custom lexer state
data AlexUserState = LS
    { lsToks         :: [Token] -- tokens read so far
    , lsCurrFile     :: FilePath -- currently active filename
    , lsEnv          :: Map.Map String (String, [String]) -- active macro definitions
    , lsCondStack    :: [Cond] -- if-else cascade state
    , lsIncludePaths :: [FilePath] -- folders to search for includes
    } deriving (Eq, Show)

-- this initial user state does not contain the initial filename, environment,
-- or include paths; alex requires that this be defined; we override it before
-- we begin the actual lexing procedure
alexInitUserState :: AlexUserState
alexInitUserState = LS [] "" Map.empty [] []

-- public-facing lexer entrypoint
lexFile :: [String] -> [(String, String)] -> FilePath -> IO [Token]
lexFile includePaths env path = do
    str <- readFile path
    let result = runAlex str $ setEnv >> alexMonadScan >> get
    return $ case result of
        Left msg -> error $ "Lexical Error: " ++ msg
        Right finalState ->
            if null $ lsCondStack finalState
                then lsToks finalState
                else error $ "unfinished conditional directives: " ++
                        (show $ length $ lsCondStack finalState)
    where
        initialEnv = Map.map (\a -> (a, [])) $ Map.fromList env
        setEnv = modify $ \s -> s
            { lsEnv = initialEnv
            , lsIncludePaths = includePaths
            , lsCurrFile = path
            }

-- invoked by alexMonadScan
alexEOF :: Alex ()
alexEOF = return ()

-- raises an alexError with the current file position appended
lexicalError :: String -> Alex a
lexicalError msg = do
    (pn, _, _, _) <- alexGetInput
    pos <- toTokPos pn
    alexError $ msg ++ ", at " ++ show pos

-- get the current user state
get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

-- get the current user state and apply a function to it
gets :: (AlexUserState -> a) -> Alex a
gets f = get >>= return . f

-- apply a transformation to the current user state
modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex func
    where func s = Right (s { alex_ust = new }, ())
            where new = f (alex_ust s)

-- helpers specifically accessing the current file state
getCurrentFile :: Alex String
getCurrentFile = gets lsCurrFile
setCurrentFile :: String -> Alex ()
setCurrentFile x = modify $ \s -> s { lsCurrFile = x }

-- find the given file for inclusion
includeSearch :: FilePath -> Alex FilePath
includeSearch file = do
    base <- getCurrentFile
    includePaths <- gets lsIncludePaths
    let directories = dropFileName base : includePaths
    let result = unsafePerformIO $ findFile directories file
    case result of
        Just path -> return path
        Nothing -> lexicalError $ "Could not find file " ++ show file ++
                        ", included from " ++ show base

-- read in the given file
loadFile :: FilePath -> Alex String
loadFile = return . unsafePerformIO . readFile

isIdentChar :: Char -> Bool
isIdentChar ch =
    ('a' <= ch && ch <= 'z') ||
    ('A' <= ch && ch <= 'Z') ||
    ('0' <= ch && ch <= '9') ||
    (ch == '_') || (ch == '$')

takeString :: Alex String
takeString = do
    (pos, _, _, str) <- alexGetInput
    let (x, rest) = span isIdentChar str
    let lastChar = if null x then ' ' else last x
    alexSetInput (foldl alexMove pos x, lastChar, [], rest)
    return x

toTokPos :: AlexPosn -> Alex Position
toTokPos (AlexPn _ l c) = do
    file <- getCurrentFile
    return $ Position file l c

-- read tokens after the name until the first (un-escaped) newline
takeUntilNewline :: Alex String
takeUntilNewline = do
    (pos, _, _, str) <- alexGetInput
    case str of
        []                 -> return ""
        '\n' :        _    -> do
            return ""
        '\\' : '\n' : rest -> do
            let newPos = alexMove (alexMove pos '\\') '\n'
            alexSetInput (newPos, '\n', [], rest)
            takeUntilNewline >>= return . (' ' :)
        ch   :        rest -> do
            let newPos = alexMove pos ch
            alexSetInput (newPos, ch, [], rest)
            takeUntilNewline >>= return . (ch :)

-- select characters up to and including the given character
takeThrough :: Char -> Alex String
takeThrough goal = do
    (_, _, _, str) <- alexGetInput
    if null str
        then lexicalError $
                "unexpected end of input, looking for " ++ (show goal)
        else do
            ch <- takeChar
            if ch == goal
                then return [ch]
                else do
                    rest <- takeThrough goal
                    return $ ch : rest

-- pop one character from the input stream
takeChar :: Alex Char
takeChar = do
    (pos, _, _, ch : str) <- alexGetInput
    let newPos = alexMove pos ch
    alexSetInput (newPos, ch, [], str)
    return ch

-- drop spaces in the input until a non-space is reached or EOF
dropSpaces :: Alex ()
dropSpaces = do
    (_, _, _, str) <- alexGetInput
    if null str || head str /= ' '
        then return ()
        else dropSpace >> dropSpaces
    where
        dropSpace :: Alex ()
        dropSpace = do
            (pos, _, _, str) <- alexGetInput
            case str of
                [] -> return ()
                ' ' : rest -> alexSetInput (alexMove pos ' ', ' ', [], rest)
                ch : _ -> lexicalError $ "expected ' ', but found: " ++ show ch


-- removes and returns a quoted string such as <foo.bar> or "foo.bar"
takeQuotedString :: Alex String
takeQuotedString = do
    dropSpaces
    ch <- takeChar
    end <-
        case ch of
            '"' -> return '"'
            '<' -> return '>'
            _ -> lexicalError $ "bad beginning of include arg: " ++ (show ch)
    rest <- takeThrough end
    let res = ch : rest
    if end == '>'
        then lexicalError $ "library includes are not supported: " ++ res
        else return res

peekChar :: Alex Char
peekChar = do
    (_, _, _, str) <- alexGetInput
    return $ if null str
        then '\n'
        else head str

takeMacroArgNames :: Alex [String]
takeMacroArgNames = do
    dropSpaces
    name <- takeString
    dropSpaces
    ch <- takeChar
    rest <- case ch of
        ',' -> takeMacroArgNames
        ')' -> return []
        _   -> lexicalError $ "unexpected char in macro defn. args: " ++ show ch
    return $ name : rest

-- TODO FIXME: We don't currently support macro arguments with default values!
takeMacroDefinition :: Alex (String, [String])
takeMacroDefinition = do
    leadCh <- peekChar
    if leadCh /= '('
        then do
            body <- takeUntilNewline
            return (body, [])
        else do
            '(' <- takeChar
            args <- takeMacroArgNames
            body <- takeUntilNewline
            if null args
                then lexicalError "macros cannot have 0 args"
                else return (body, args)

-- commas and right parens are forbidden outside matched pairs of: (), [], {},
-- "", except to delimit arguments or end the list of arguments; see 22.5.1
takeMacroArguments :: Alex [String]
takeMacroArguments = do
    dropSpaces
    '(' <- takeChar
    loop "" []
    where
        loop :: String -> [Char] -> Alex [String]
        loop curr stack = do
            ch <- takeChar
            case (stack, ch) of
                (      s,'\\') -> do
                    ch2 <- takeChar
                    loop (curr ++ [ch, ch2]) s
                ([     ], ',') -> do
                    rest <- loop "" stack
                    return $ curr : rest
                ([     ], ')') -> return [curr]

                ('"' : s, '"') -> loop (curr ++ [ch]) s
                (      s, '"') -> loop (curr ++ [ch]) ('"' : s)
                ('[' : s, ']') -> loop (curr ++ [ch]) s
                (      s, '[') -> loop (curr ++ [ch]) ('[' : s)
                ('(' : s, ')') -> loop (curr ++ [ch]) s
                (      s, '(') -> loop (curr ++ [ch]) ('(' : s)
                ('{' : s, '}') -> loop (curr ++ [ch]) s
                (      s, '{') -> loop (curr ++ [ch]) ('{' : s)

                (      s,'\n') -> loop (curr ++ [' ']) s
                (      s, _  ) -> loop (curr ++ [ch ]) s

findUnescapedQuote :: String -> (String, String)
findUnescapedQuote [] = ([], [])
findUnescapedQuote ('`' : '\\' : '`' : '"' : rest) = ('\\' : '"' : start, end)
    where (start, end) = findUnescapedQuote rest
findUnescapedQuote ('\\' : '"' : rest) = ('\\' : '"' : start, end)
    where (start, end) = findUnescapedQuote rest
findUnescapedQuote ('"' : rest) = ("\"", rest)
findUnescapedQuote (ch : rest) = (ch : start, end)
    where (start, end) = findUnescapedQuote rest

-- substitute in the arguments for a macro expension
substituteArgs :: String -> [String] -> [String] -> String
substituteArgs "" _ _ = ""
substituteArgs ('`' : '`' : body) names args =
    substituteArgs body names args
substituteArgs ('"' : body) names args =
    '"' : start ++ substituteArgs rest names args
    where (start, rest) = findUnescapedQuote body
substituteArgs ('`' : '"' : body) names args =
    '"' : substituteArgs (init start) names args
    ++ '"' : substituteArgs rest names args
    where (start, rest) = findUnescapedQuote body
substituteArgs body names args =
    case findIndex isPresent names of
        Nothing -> head body : substituteArgs (tail body) names args
        Just idx ->
            (args !! idx) ++ substituteArgs (drop nameLen body) names args
            where nameLen = length $ names !! idx
    where isPresent a = isPrefixOf a body

-- directives that must always be processed even if the current code block is
-- being excluded; we have to process conditions so we can match them up with
-- their ending tag, even if they're being skipped
unskippableDirectives :: [String]
unskippableDirectives = ["else", "elsif", "endif", "ifdef", "ifndef"]

handleDirective :: Action
handleDirective (posOrig, _, _, strOrig) len = do
    let thisTokenStr = take len strOrig
    let directive = tail $ thisTokenStr
    let newPos = foldl alexMove posOrig thisTokenStr
    alexSetInput (newPos, last thisTokenStr, [], drop len strOrig)

    env <- gets lsEnv
    tempInput <- alexGetInput
    let dropUntilNewline = removeUntil "\n" tempInput 0

    condStack <- gets lsCondStack
    if not (null condStack)
        && head condStack /= CurrentlyTrue
        && not (elem directive unskippableDirectives)
    then alexMonadScan
    else case directive of

        "default_nettype" -> dropUntilNewline
        "timescale" -> dropUntilNewline

        "__FILE__" -> do
            tokPos <- toTokPos posOrig
            currFile <- gets lsCurrFile
            let tokStr = show currFile
            modify $ push $ Token Lit_string tokStr tokPos
            alexMonadScan
        "__LINE__" -> do
            tokPos <- toTokPos posOrig
            let Position _ currLine _ = tokPos
            let tokStr = show currLine
            modify $ push $ Token Lit_number tokStr tokPos
            alexMonadScan

        "include" -> do
            quotedFilename <- takeQuotedString
            inputFollow <- alexGetInput
            fileFollow <- getCurrentFile
            -- process the included file
            let filename = init $ tail quotedFilename
            path <- includeSearch filename
            content <- loadFile path
            let inputIncluded = (alexStartPos, ' ', [], content)
            setCurrentFile path
            alexSetInput inputIncluded
            alexMonadScan
            -- resume processing the original file
            setCurrentFile fileFollow
            alexSetInput inputFollow
            alexMonadScan

        "ifdef" -> do
            dropSpaces
            name <- takeString
            let newCond = if Map.member name env
                    then CurrentlyTrue
                    else NeverTrue
            modify $ \s -> s { lsCondStack = newCond : condStack }
            alexMonadScan
        "ifndef" -> do
            dropSpaces
            name <- takeString
            let newCond = if Map.notMember name env
                    then CurrentlyTrue
                    else NeverTrue
            modify $ \s -> s { lsCondStack = newCond : condStack }
            alexMonadScan
        "else" -> do
            let newCond = if head condStack == NeverTrue
                    then CurrentlyTrue
                    else NeverTrue
            modify $ \s -> s { lsCondStack = newCond : tail condStack }
            alexMonadScan
        "elsif" -> do
            dropSpaces
            name <- takeString
            let currCond = head condStack
            let newCond =
                    if currCond /= NeverTrue then
                        PreviouslyTrue
                    else if Map.member name env then
                        CurrentlyTrue
                    else
                        NeverTrue
            modify $ \s -> s { lsCondStack = newCond : tail condStack }
            alexMonadScan
        "endif" -> do
            modify $ \s -> s { lsCondStack = tail condStack }
            alexMonadScan

        "define" -> do
            dropSpaces
            name <- takeString
            defn <- takeMacroDefinition
            modify $ \s -> s { lsEnv = Map.insert name defn env }
            alexMonadScan
        "undef" -> do
            dropSpaces
            name <- takeString
            modify $ \s -> s { lsEnv = Map.delete name env }
            alexMonadScan
        "undefineall" -> do
            modify $ \s -> s { lsEnv = Map.empty }
            alexMonadScan

        _ -> do
            case Map.lookup directive env of
                Nothing -> lexicalError $ "Undefined macro: " ++ directive
                Just (body, formalArgs) -> do
                    -- TODO: How should we track the file position when we
                    -- substitute in a macro?
                    replacement <- if null formalArgs
                        then return body
                        else do
                            actualArgs <- takeMacroArguments
                            if length formalArgs == length actualArgs
                                then return $ substituteArgs body formalArgs actualArgs
                                else lexicalError $
                                        "different number of macro args: " ++
                                        (show $ length formalArgs) ++ " vs. " ++
                                        (show $ length actualArgs)
                    let size = length replacement
                    (AlexPn f l c, _, [], str) <- alexGetInput
                    let pos = AlexPn (f - size) l (c - size)
                    alexSetInput (pos, ' ', [], replacement ++ str)
                    alexMonadScan

-- remove characters from the input until the pattern is reached
removeUntil :: String -> Action
removeUntil pattern _ _ = loop
    where
        patternLen = length pattern
        wantNewline = pattern == "\n"
        loop = do
            (pos, _, _, str) <- alexGetInput
            let found = (null str && wantNewline)
                     || pattern == take patternLen str
            let nextPos = alexMove pos (head str)
            let afterPos = if wantNewline
                    then alexMove pos '\n'
                    else foldl alexMove pos pattern
            let (newPos, newStr) = if found
                    then (afterPos, drop patternLen str)
                    else (nextPos, drop 1 str)
            if not found && null str
                then lexicalError $ "Reached EOF while looking for: " ++
                        show pattern
                else do
                    alexSetInput (newPos, ' ', [], newStr)
                    if found
                        then alexMonadScan
                        else loop

push :: Token -> AlexUserState -> AlexUserState
push t s = s { lsToks = (lsToks s) ++ [t] }

tok :: TokenName -> Action
tok tokId (pos, _, _, input) len = do
    let tokStr = take len input
    tokPos <- toTokPos pos
    condStack <- gets lsCondStack
    () <- if not (null condStack) && head condStack /= CurrentlyTrue
        then modify id
        else modify (push $ Token tokId tokStr tokPos)
    alexMonadScan
}
