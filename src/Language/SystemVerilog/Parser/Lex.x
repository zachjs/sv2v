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

    "`include"         { includeFile }
    @directive         { handleDirective }

    @commentLine       { removeUntil "\n" }
    @commentBlock      { removeUntil "*/" }

    $white             ;

    .                  { tok Unknown }

{

data Cond
    = CurrentlyTrue
    | PreviouslyTrue
    | NeverTrue
    deriving (Eq, Show)

data AlexUserState = LS
    { lsToks :: [Token]
    , lsCurrFile :: FilePath
    , lsEnv :: Map.Map String String
    , lsCondStack :: [Cond]
    , lsIncludePaths :: [FilePath]
    } deriving (Eq, Show)

alexInitUserState :: AlexUserState
alexInitUserState = LS [] "" Map.empty [] []

lexFile :: [String] -> [(String, String)] -> FilePath -> IO [Token]
lexFile includePaths env path = do
    str <- readFile path
    let result = runAlex str $ setEnv >> alexMonadScan >> get
    return $ case result of
        Left msg -> error $ "Lexical Error: " ++ msg
        Right tokens -> lsToks tokens
    where
        initialEnv = Map.fromList env
        setEnv = modify $ \s -> s
            { lsEnv = initialEnv
            , lsIncludePaths = includePaths
            , lsCurrFile = path
            }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

gets :: (AlexUserState -> a) -> Alex a
gets f = get >>= return . f

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex func
    where func s = Right (s { alex_ust = new }, ())
            where new = f (alex_ust s)

getCurrentFile :: Alex String
getCurrentFile = gets lsCurrFile

setCurrentFile :: String -> Alex ()
setCurrentFile x = modify $ \s -> s { lsCurrFile = x }

alexEOF :: Alex ()
alexEOF = return ()

type Action = AlexInput -> Int -> Alex ()

breakAfter :: (a -> Bool) -> [a] -> ([a], [a])
breakAfter f l = (a ++ [b], bs)
    where (a, b : bs) = break f l

includeSearch :: FilePath -> Alex FilePath
includeSearch file = do
    base <- getCurrentFile
    includePaths <- gets lsIncludePaths
    let directories = dropFileName base : includePaths
    let result = unsafePerformIO $ findFile directories file
    case result of
        Just path -> return path
        Nothing ->
            alexError
                $ "Could not find file " ++ file ++ " included from " ++ base

loadFile :: String -> Alex String
loadFile s = return $ unsafePerformIO $ readFile s

includeFile :: Action
includeFile (AlexPn f l c, _, _, str) len = do
    let (dropped , rest1) = breakAfter (== '"') (drop len str)
    let (filename, rest2) = break (== '"') rest1
    let rest3 = if null rest2 then [] else tail rest2
    let offset = len + length dropped + length filename + 1
    let inputFollow = (AlexPn (f + offset) l (c + offset), ' ', [], rest3)
    fileFollow <- getCurrentFile
    -- process the the included file
    path <- includeSearch filename
    content <- loadFile path
    let inputIncluded = (AlexPn 0 0 0, ' ', [], content)
    setCurrentFile path
    alexSetInput inputIncluded
    alexMonadScan
    -- resume processing the original file
    setCurrentFile fileFollow
    alexSetInput inputFollow
    alexMonadScan

unskippableDirectives :: [String]
unskippableDirectives = ["else", "elsif", "endif", "ifdef", "ifndef"]

isIdentChar :: Char -> Bool
isIdentChar ch =
    ('a' <= ch && ch <= 'z') ||
    ('A' <= ch && ch <= 'Z') ||
    ('0' <= ch && ch <= '9') ||
    (ch == '_') || (ch == '$')

takeString :: Alex String
takeString = do
    (AlexPn f l c, _, _, str) <- alexGetInput
    let (x, rest) = span isIdentChar str
    let len = length x
    alexSetInput (AlexPn (f+len) l (c+len), ' ', [], rest)
    return x

getCurrentPos :: Alex Position
getCurrentPos = do
    (AlexPn _ l c, _, _, _) <- alexGetInput
    file <- getCurrentFile
    return $ Position file l c

dropSpace :: Alex ()
dropSpace = do
    (AlexPn f l c, _, _, str) <- alexGetInput
    case str of
        [] -> return ()
        ' ' : rest -> alexSetInput (AlexPn (f+1) l (c+1), ' ', [], rest)
        ch : _ -> do
            pos <- getCurrentPos
            alexError $ "dropSpace encountered bad char: " ++ show ch ++
                " at " ++ show pos

-- read tokens after the name until the first (un-escaped) newline
takeUntilNewline :: Alex String
takeUntilNewline = do
    (AlexPn f l c, _, _, str) <- alexGetInput
    case str of
        []                 -> return ""
        '\n' :        _    -> do
            return ""
        '\\' : '\n' : rest -> do
            alexSetInput (AlexPn (f+2) (l+1) 0, ' ', [], rest)
            takeUntilNewline >>= return . (' ' :)
        ch   :        rest -> do
            alexSetInput (AlexPn (f+1) l (c+1), ' ', [], rest)
            takeUntilNewline >>= return . (ch :)

handleDirective :: Action
handleDirective (AlexPn fOrig lOrig cOrig, _, _, strOrig) len = do
    let directive = tail $ take len strOrig
    let newPos = AlexPn (fOrig + len) lOrig (cOrig + len)
    alexSetInput (newPos, ' ', [], drop len strOrig)

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

        "ifdef" -> do
            dropSpace
            name <- takeString
            let newCond = if Map.member name env
                    then CurrentlyTrue
                    else NeverTrue
            modify $ \s -> s { lsCondStack = newCond : condStack }
            alexMonadScan
        "ifndef" -> do
            dropSpace
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
            dropSpace
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
            -- TODO: We don't yet support macros with arguments!
            dropSpace
            name <- takeString
            defn <- takeUntilNewline
            modify $ \s -> s { lsEnv = Map.insert name defn env }
            alexMonadScan
        "undef" -> do
            dropSpace
            name <- takeString
            modify $ \s -> s { lsEnv = Map.delete name env }
            alexMonadScan
        "undefineall" -> do
            modify $ \s -> s { lsEnv = Map.empty }
            alexMonadScan

        _ -> do
            case Map.lookup directive env of
                Nothing -> do
                    pos <- getCurrentPos >>= return . show
                    alexError $ "Undefined macro: " ++ directive ++ " at " ++ pos
                Just replacement -> do
                    let size = length replacement
                    -- TODO: How should we track the file position when we
                    -- substitute in a macro?
                    (AlexPn f l c, ' ', [], str) <- alexGetInput
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
            (AlexPn f l c, _, _, str) <- alexGetInput
            let found = (null str && wantNewline)
                     || pattern == take patternLen str
            let nextPos = if head str == '\n'
                    then AlexPn (f+1) (l+1) 0
                    else AlexPn (f+1) l (c+1)
            let afterPos = if wantNewline
                    then AlexPn (f+1) (l+1) 0
                    else AlexPn (f+1) l (c + patternLen)
            let (newPos, newStr) = if found
                    then (afterPos, drop patternLen str)
                    else (nextPos, drop 1 str)
            alexSetInput (newPos, ' ', [], newStr)
            if found
                then alexMonadScan
                else loop

tok :: TokenName -> Action
tok tokId ((AlexPn _ l c), _, _, input) len = do
    currFile <- gets lsCurrFile
    let tokStr = take len input
    let tokPos = Position currFile l c
    condStack <- gets lsCondStack
    () <- if not (null condStack) && head condStack /= CurrentlyTrue
        then modify id
        else modify (push $ Token tokId tokStr tokPos)
    alexMonadScan
    where push t s = s { lsToks = (lsToks s) ++ [t] }
}
