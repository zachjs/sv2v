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
 -
 - It may be possible to separate the preprocessor from the lexer by having a
 - preprocessor which produces location annotations. This could improve error
 - messaging and remove the include file and macro boundary hacks.
 -}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- The above pragma gets rid of annoying warning caused by alex 3.2.4. This has
-- been fixed on their development branch, so this can be removed once they roll
-- a new release. (no new release as of 3/29/2018)

module Language.SystemVerilog.Parser.Lex
    ( lexFile
    , Env
    ) where

import System.FilePath (dropFileName)
import System.Directory (findFile)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map
import Data.List (span, elemIndex, dropWhileEnd)
import Data.Maybe (isJust, fromJust)

import Language.SystemVerilog.Parser.Tokens
}

%wrapper "monadUserState"

-- Numbers

@nonZeroDecimalDigit = [1-9]
@decimalDigit = [0-9]
@xDigit       = [xX]
@zDigit       = [zZ\?]
@binaryDigit  = @xDigit | @zDigit | [0-1]
@octalDigit   = @xDigit | @zDigit | [0-7]
@hexDigit     = @xDigit | @zDigit | [0-9a-fA-F]

@decimalBase = "'" [sS]? [dD]
@binaryBase  = "'" [sS]? [bB]
@octalBase   = "'" [sS]? [oO]
@hexBase     = "'" [sS]? [hH]

@nonZeroUnsignedNumber = @nonZeroDecimalDigit ("_" | @decimalDigit)*
@unsignedNumber        = @decimalDigit        ("_" | @decimalDigit)*
@binaryValue           = @binaryDigit         ("_" | @binaryDigit )*
@octalValue            = @octalDigit          ("_" | @octalDigit  )*
@hexValue              = @hexDigit            ("_" | @hexDigit    )*

@exp = [eE]
@sign = [\-\+]
@fixedPointNumber = @unsignedNumber "." @unsignedNumber
@realNumber
    = @fixedPointNumber
    | @unsignedNumber ("." @unsignedNumber)? @exp @sign? @unsignedNumber

@size = @nonZeroUnsignedNumber " "?

@binaryNumber = @size? @binaryBase " "? @binaryValue
@octalNumber  = @size? @octalBase  " "? @octalValue
@hexNumber    = @size? @hexBase    " "? @hexValue

@unbasedUnsizedLiteral = "'" ( 0 | 1 | x | X | z | Z )

@decimalNumber
    = @unsignedNumber
    | @size? @decimalBase " "? @unsignedNumber
    | @size? @decimalBase " "? @xDigit "_"*
    | @size? @decimalBase " "? @zDigit "_"*
@integralNumber
    = @decimalNumber
    | @octalNumber
    | @binaryNumber
    | @hexNumber
    | @unbasedUnsizedLiteral
@number
    = @integralNumber
    | @realNumber

-- Strings

@string = \" (\\\"|\\\r?\n|[^\"\r\n])* \"

-- Times

@timeUnit = s | ms | us | ns | ps | fs
@time
    = @unsignedNumber @timeUnit
    | @fixedPointNumber @timeUnit

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

    "$bits"                { tok KW_dollar_bits                }
    "$dimensions"          { tok KW_dollar_dimensions          }
    "$unpacked_dimensions" { tok KW_dollar_unpacked_dimensions }
    "$left"                { tok KW_dollar_left                }
    "$right"               { tok KW_dollar_right               }
    "$low"                 { tok KW_dollar_low                 }
    "$high"                { tok KW_dollar_high                }
    "$increment"           { tok KW_dollar_increment           }
    "$size"                { tok KW_dollar_size                }

    "accept_on"        { tok KW_accept_on    }
    "alias"            { tok KW_alias        }
    "always"           { tok KW_always       }
    "always_comb"      { tok KW_always_comb  }
    "always_ff"        { tok KW_always_ff    }
    "always_latch"     { tok KW_always_latch }
    "and"              { tok KW_and          }
    "assert"           { tok KW_assert       }
    "assign"           { tok KW_assign       }
    "assume"           { tok KW_assume       }
    "automatic"        { tok KW_automatic    }
    "before"           { tok KW_before       }
    "begin"            { tok KW_begin        }
    "bind"             { tok KW_bind         }
    "bins"             { tok KW_bins         }
    "binsof"           { tok KW_binsof       }
    "bit"              { tok KW_bit          }
    "break"            { tok KW_break        }
    "buf"              { tok KW_buf          }
    "bufif0"           { tok KW_bufif0       }
    "bufif1"           { tok KW_bufif1       }
    "byte"             { tok KW_byte         }
    "case"             { tok KW_case         }
    "casex"            { tok KW_casex        }
    "casez"            { tok KW_casez        }
    "cell"             { tok KW_cell         }
    "chandle"          { tok KW_chandle      }
    "checker"          { tok KW_checker      }
    "class"            { tok KW_class        }
    "clocking"         { tok KW_clocking     }
    "cmos"             { tok KW_cmos         }
    "config"           { tok KW_config       }
    "const"            { tok KW_const        }
    "constraint"       { tok KW_constraint   }
    "context"          { tok KW_context      }
    "continue"         { tok KW_continue     }
    "cover"            { tok KW_cover        }
    "covergroup"       { tok KW_covergroup   }
    "coverpoint"       { tok KW_coverpoint   }
    "cross"            { tok KW_cross        }
    "deassign"         { tok KW_deassign     }
    "default"          { tok KW_default      }
    "defparam"         { tok KW_defparam     }
    "design"           { tok KW_design       }
    "disable"          { tok KW_disable      }
    "dist"             { tok KW_dist         }
    "do"               { tok KW_do           }
    "edge"             { tok KW_edge         }
    "else"             { tok KW_else         }
    "end"              { tok KW_end          }
    "endcase"          { tok KW_endcase      }
    "endchecker"       { tok KW_endchecker   }
    "endclass"         { tok KW_endclass     }
    "endclocking"      { tok KW_endclocking  }
    "endconfig"        { tok KW_endconfig    }
    "endfunction"      { tok KW_endfunction  }
    "endgenerate"      { tok KW_endgenerate  }
    "endgroup"         { tok KW_endgroup     }
    "endinterface"     { tok KW_endinterface }
    "endmodule"        { tok KW_endmodule    }
    "endpackage"       { tok KW_endpackage   }
    "endprimitive"     { tok KW_endprimitive }
    "endprogram"       { tok KW_endprogram   }
    "endproperty"      { tok KW_endproperty  }
    "endspecify"       { tok KW_endspecify   }
    "endsequence"      { tok KW_endsequence  }
    "endtable"         { tok KW_endtable     }
    "endtask"          { tok KW_endtask      }
    "enum"             { tok KW_enum         }
    "event"            { tok KW_event        }
    "eventually"       { tok KW_eventually   }
    "expect"           { tok KW_expect       }
    "export"           { tok KW_export       }
    "extends"          { tok KW_extends      }
    "extern"           { tok KW_extern       }
    "final"            { tok KW_final        }
    "first_match"      { tok KW_first_match  }
    "for"              { tok KW_for          }
    "force"            { tok KW_force        }
    "foreach"          { tok KW_foreach      }
    "forever"          { tok KW_forever      }
    "fork"             { tok KW_fork         }
    "forkjoin"         { tok KW_forkjoin     }
    "function"         { tok KW_function     }
    "generate"         { tok KW_generate     }
    "genvar"           { tok KW_genvar       }
    "global"           { tok KW_global       }
    "highz0"           { tok KW_highz0       }
    "highz1"           { tok KW_highz1       }
    "if"               { tok KW_if           }
    "iff"              { tok KW_iff          }
    "ifnone"           { tok KW_ifnone       }
    "ignore_bins"      { tok KW_ignore_bins  }
    "illegal_bins"     { tok KW_illegal_bins }
    "implements"       { tok KW_implements   }
    "implies"          { tok KW_implies      }
    "import"           { tok KW_import       }
    "incdir"           { tok KW_incdir       }
    "include"          { tok KW_include      }
    "initial"          { tok KW_initial      }
    "inout"            { tok KW_inout        }
    "input"            { tok KW_input        }
    "inside"           { tok KW_inside       }
    "instance"         { tok KW_instance     }
    "int"              { tok KW_int          }
    "integer"          { tok KW_integer      }
    "interconnect"     { tok KW_interconnect }
    "interface"        { tok KW_interface    }
    "intersect"        { tok KW_intersect    }
    "join"             { tok KW_join         }
    "join_any"         { tok KW_join_any     }
    "join_none"        { tok KW_join_none    }
    "large"            { tok KW_large        }
    "let"              { tok KW_let          }
    "liblist"          { tok KW_liblist      }
    "library"          { tok KW_library      }
    "local"            { tok KW_local        }
    "localparam"       { tok KW_localparam   }
    "logic"            { tok KW_logic        }
    "longint"          { tok KW_longint      }
    "macromodule"      { tok KW_macromodule  }
    "matches"          { tok KW_matches      }
    "medium"           { tok KW_medium       }
    "modport"          { tok KW_modport      }
    "module"           { tok KW_module       }
    "nand"             { tok KW_nand         }
    "negedge"          { tok KW_negedge      }
    "nettype"          { tok KW_nettype      }
    "new"              { tok KW_new          }
    "nexttime"         { tok KW_nexttime     }
    "nmos"             { tok KW_nmos         }
    "nor"              { tok KW_nor          }
    "noshowcancelled"  { tok KW_noshowcancelled }
    "not"              { tok KW_not          }
    "notif0"           { tok KW_notif0       }
    "notif1"           { tok KW_notif1       }
    "null"             { tok KW_null         }
    "or"               { tok KW_or           }
    "output"           { tok KW_output       }
    "package"          { tok KW_package      }
    "packed"           { tok KW_packed       }
    "parameter"        { tok KW_parameter    }
    "pmos"             { tok KW_pmos         }
    "posedge"          { tok KW_posedge      }
    "primitive"        { tok KW_primitive    }
    "priority"         { tok KW_priority     }
    "program"          { tok KW_program      }
    "property"         { tok KW_property     }
    "protected"        { tok KW_protected    }
    "pull0"            { tok KW_pull0        }
    "pull1"            { tok KW_pull1        }
    "pulldown"         { tok KW_pulldown     }
    "pullup"           { tok KW_pullup       }
    "pulsestyle_ondetect" { tok KW_pulsestyle_ondetect }
    "pulsestyle_onevent"  { tok KW_pulsestyle_onevent }
    "pure"             { tok KW_pure         }
    "rand"             { tok KW_rand         }
    "randc"            { tok KW_randc        }
    "randcase"         { tok KW_randcase     }
    "randsequence"     { tok KW_randsequence }
    "rcmos"            { tok KW_rcmos        }
    "real"             { tok KW_real         }
    "realtime"         { tok KW_realtime     }
    "ref"              { tok KW_ref          }
    "reg"              { tok KW_reg          }
    "reject_on"        { tok KW_reject_on    }
    "release"          { tok KW_release      }
    "repeat"           { tok KW_repeat       }
    "restrict"         { tok KW_restrict     }
    "return"           { tok KW_return       }
    "rnmos"            { tok KW_rnmos        }
    "rpmos"            { tok KW_rpmos        }
    "rtran"            { tok KW_rtran        }
    "rtranif0"         { tok KW_rtranif0     }
    "rtranif1"         { tok KW_rtranif1     }
    "s_always"         { tok KW_s_always     }
    "s_eventually"     { tok KW_s_eventually }
    "s_nexttime"       { tok KW_s_nexttime   }
    "s_until"          { tok KW_s_until      }
    "s_until_with"     { tok KW_s_until_with }
    "scalared"         { tok KW_scalared     }
    "sequence"         { tok KW_sequence     }
    "shortint"         { tok KW_shortint     }
    "shortreal"        { tok KW_shortreal    }
    "showcancelled"    { tok KW_showcancelled }
    "signed"           { tok KW_signed       }
    "small"            { tok KW_small        }
    "soft"             { tok KW_soft         }
    "solve"            { tok KW_solve        }
    "specify"          { tok KW_specify      }
    "specparam"        { tok KW_specparam    }
    "static"           { tok KW_static       }
    "string"           { tok KW_string       }
    "strong"           { tok KW_strong       }
    "strong0"          { tok KW_strong0      }
    "strong1"          { tok KW_strong1      }
    "struct"           { tok KW_struct       }
    "super"            { tok KW_super        }
    "supply0"          { tok KW_supply0      }
    "supply1"          { tok KW_supply1      }
    "sync_accept_on"   { tok KW_sync_accept_on }
    "sync_reject_on"   { tok KW_sync_reject_on }
    "table"            { tok KW_table        }
    "tagged"           { tok KW_tagged       }
    "task"             { tok KW_task         }
    "this"             { tok KW_this         }
    "throughout"       { tok KW_throughout   }
    "time"             { tok KW_time         }
    "timeprecision"    { tok KW_timeprecision }
    "timeunit"         { tok KW_timeunit     }
    "tran"             { tok KW_tran         }
    "tranif0"          { tok KW_tranif0      }
    "tranif1"          { tok KW_tranif1      }
    "tri"              { tok KW_tri          }
    "tri0"             { tok KW_tri0         }
    "tri1"             { tok KW_tri1         }
    "triand"           { tok KW_triand       }
    "trior"            { tok KW_trior        }
    "trireg"           { tok KW_trireg       }
    "type"             { tok KW_type         }
    "typedef"          { tok KW_typedef      }
    "union"            { tok KW_union        }
    "unique"           { tok KW_unique       }
    "unique0"          { tok KW_unique0      }
    "unsigned"         { tok KW_unsigned     }
    "until"            { tok KW_until        }
    "until_with"       { tok KW_until_with   }
    "untyped"          { tok KW_untyped      }
    "use"              { tok KW_use          }
    "uwire"            { tok KW_uwire        }
    "var"              { tok KW_var          }
    "vectored"         { tok KW_vectored     }
    "virtual"          { tok KW_virtual      }
    "void"             { tok KW_void         }
    "wait"             { tok KW_wait         }
    "wait_order"       { tok KW_wait_order   }
    "wand"             { tok KW_wand         }
    "weak"             { tok KW_weak         }
    "weak0"            { tok KW_weak0        }
    "weak1"            { tok KW_weak1        }
    "while"            { tok KW_while        }
    "wildcard"         { tok KW_wildcard     }
    "wire"             { tok KW_wire         }
    "with"             { tok KW_with         }
    "within"           { tok KW_within       }
    "wor"              { tok KW_wor          }
    "xnor"             { tok KW_xnor         }
    "xor"              { tok KW_xor          }

    @simpleIdentifier  { tok Id_simple  }
    @escapedIdentifier { tok Id_escaped }
    @systemIdentifier  { tok Id_system  }

    @number            { tok Lit_number }
    @string            { tok Lit_string }
    @time              { tok Lit_time }

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
    "<->"              { tok Sym_lt_dash_gt }
    "|->"              { tok Sym_bar_dash_gt }
    "|=>"              { tok Sym_bar_eq_gt }
    "[->"              { tok Sym_brack_l_dash_gt }
    "#-#"              { tok Sym_pound_dash_pound }
    "#=#"              { tok Sym_pound_eq_pound }
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

-- map from macro to definition, plus arguments
type Env = Map.Map String (String, [(String, Maybe String)])

-- our custom lexer state
data AlexUserState = LS
    { lsToks         :: [Token] -- tokens read so far, *in reverse order* for efficiency
    , lsCurrFile     :: FilePath -- currently active filename
    , lsEnv          :: Env -- active macro definitions
    , lsCondStack    :: [Cond] -- if-else cascade state
    , lsIncludePaths :: [FilePath] -- folders to search for includes
    } deriving (Eq, Show)

-- this initial user state does not contain the initial filename, environment,
-- or include paths; alex requires that this be defined; we override it before
-- we begin the actual lexing procedure
alexInitUserState :: AlexUserState
alexInitUserState = LS [] "" Map.empty [] []

-- public-facing lexer entrypoint
lexFile :: [String] -> Env -> FilePath -> IO (Either String ([Token], Env))
lexFile includePaths env path = do
    str <- readFile path
    let result = runAlex str $ setEnv >> alexMonadScan >> get
    return $ case result of
        Left msg -> Left msg
        Right finalState ->
            if null $ lsCondStack finalState
                then Right (finalToks, lsEnv finalState)
                else Left $ path ++ ": unfinished conditional directives: " ++
                        (show $ length $ lsCondStack finalState)
            where finalToks = coalesce $ reverse $ lsToks finalState
    where
        setEnv = do
            modify $ \s -> s
                { lsEnv = env
                , lsIncludePaths = includePaths
                , lsCurrFile = path
                }

-- combines identifiers and numbers that cross macro boundaries
coalesce :: [Token] -> [Token]
coalesce [] = []
coalesce (Token MacroBoundary _ _ : rest) = coalesce rest
coalesce (Token t1 str1 pn1 : Token MacroBoundary _ _ : Token t2 str2 pn2 : rest) =
    case (t1, t2, immediatelyFollows) of
        (Lit_number, Lit_number, _) ->
            Token t1 (str1 ++ str2) pn1 : (coalesce rest)
        (Id_simple, Id_simple, True) ->
            Token t1 (str1 ++ str2) pn1 : (coalesce rest)
        _ ->
            Token t1 str1 pn1 : (coalesce $ Token t2 str2 pn2 : rest)
    where
        Position _ l1 c1 = pn1
        Position _ l2 c2 = pn2
        apn1 = AlexPn 0 l1 c1
        apn2 = AlexPn (length str1) l2 c2
        immediatelyFollows = apn2 == foldl alexMove apn1 str1
coalesce (x : xs) = x : coalesce xs

-- invoked by alexMonadScan
alexEOF :: Alex ()
alexEOF = return ()

-- raises an alexError with the current file position appended
lexicalError :: String -> Alex a
lexicalError msg = do
    (pn, _, _, _) <- alexGetInput
    pos <- toTokPos pn
    alexError $ show pos ++ ": Lexical error: " ++ msg

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
        '/' : '/' : _ -> do
            remainder <- takeThrough '\n'
            case last $ init remainder of
                '\\' -> takeUntilNewline >>= return . (' ' :)
                _ -> return ""
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
    (pos, _, _, str) <- alexGetInput
    (ch, chs) <-
        if null str
            then lexicalError "unexpected end of input"
            else return (head str, tail str)
    let newPos = alexMove pos ch
    alexSetInput (newPos, ch, [], chs)
    return ch

-- drop spaces in the input until a non-space is reached or EOF
dropSpaces :: Alex ()
dropSpaces = do
    (pos, _, _, str) <- alexGetInput
    if null str then
        return ()
    else do
        let ch : rest = str
        if ch == '\t' || ch == ' ' then do
            alexSetInput (alexMove pos ch, ch, [], tail str)
            dropSpaces
        else
            return ()

isWhitespaceChar :: Char -> Bool
isWhitespaceChar ch = elem ch [' ', '\t', '\n']

-- drop all leading whitespace in the input
dropWhitespace :: Alex ()
dropWhitespace = do
    (pos, _, _, str) <- alexGetInput
    case str of
        ch : chs ->
            if isWhitespaceChar ch
                then do
                    alexSetInput (alexMove pos ch, ch, [], chs)
                    dropWhitespace
                else return()
        [] -> return ()

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

-- removes and returns a decimal number
takeNumber :: Alex Int
takeNumber = do
    dropSpaces
    leadCh <- peekChar
    if '0' <= leadCh && leadCh <= '9'
        then step 0
        else lexicalError $ "expected number, but found unexpected char: "
                ++ show leadCh
    where
        step number = do
            ch <- takeChar
            if ch == ' ' || ch == '\n' then
                return number
            else if '0' <= ch && ch <= '9' then do
                let digit = ord ch - ord '0'
                step $ number * 10 + digit
            else
                lexicalError $ "unexpected char while reading number: "
                    ++ show ch

peekChar :: Alex Char
peekChar = do
    (_, _, _, str) <- alexGetInput
    return $ if null str
        then '\n'
        else head str

takeMacroDefinition :: Alex (String, [(String, Maybe String)])
takeMacroDefinition = do
    leadCh <- peekChar
    if leadCh /= '('
        then do
            body <- takeUntilNewline
            return (body, [])
        else do
            args <- takeMacroArguments
            body <- takeUntilNewline
            argsWithDefaults <- mapM splitArg args
            if null args
                then lexicalError "macros cannot have 0 args"
                else return (body, argsWithDefaults)
    where
        splitArg :: String -> Alex (String, Maybe String)
        splitArg [] = lexicalError "macro defn. empty argument"
        splitArg str = do
            let (name, rest) = span isIdentChar str
            if null name || not (all isIdentChar name) then
                lexicalError $ "invalid macro arg name: " ++ show name
            else if null rest then
                return (name, Nothing)
            else do
                let trimmed = dropWhile isWhitespaceChar rest
                let leadCh = head trimmed
                if leadCh /= '='
                then lexicalError $ "bad char after arg name: " ++ (show leadCh)
                else return (name, Just $ tail trimmed)

-- commas and right parens are forbidden outside matched pairs of: (), [], {},
-- "", except to delimit arguments or end the list of arguments; see 22.5.1
takeMacroArguments :: Alex [String]
takeMacroArguments = do
    dropWhitespace
    leadCh <- takeChar
    if leadCh == '('
        then argLoop
        else lexicalError $ "expected begining of macro arguments, but found "
                ++ show leadCh
    where
        argLoop :: Alex [String]
        argLoop = do
            dropWhitespace
            (arg, isEnd) <- loop "" []
            let arg' = dropWhileEnd isWhitespaceChar arg
            if isEnd
                then return [arg']
                else do
                    rest <- argLoop
                    return $ arg' : rest
        loop :: String -> [Char] -> Alex (String, Bool)
        loop curr stack = do
            ch <- takeChar
            case (stack, ch) of
                (      s,'\\') -> do
                    ch2 <- takeChar
                    loop (curr ++ [ch, ch2]) s
                ([     ], ',') -> return (curr, False)
                ([     ], ')') -> return (curr, True)

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
substituteArgs ('\\' : '"' : body) names args =
    '\\' : '"' : substituteArgs body names args
substituteArgs ('`' : '"' : body) names args =
    '"' : substituteArgs (init start) names args
    ++ '"' : substituteArgs rest names args
    where (start, rest) = findUnescapedQuote body
substituteArgs body names args =
    case span isIdentChar body of
        ([], _) -> head body : substituteArgs (tail body) names args
        (ident, rest) ->
            case elemIndex ident names of
                Nothing -> ident ++ substituteArgs rest names args
                Just idx -> (args !! idx) ++ substituteArgs rest names args

defaultMacroArgs :: [Maybe String] -> [String] -> Alex [String]
defaultMacroArgs [] [] = return []
defaultMacroArgs [] _ = lexicalError "too many macro arguments given"
defaultMacroArgs defaults [] = do
    if all isJust defaults
        then return $ map fromJust defaults
        else lexicalError "too few macro arguments given"
defaultMacroArgs (f : fs) (a : as) = do
    let arg = if a == "" && isJust f
            then fromJust f
            else a
    args <- defaultMacroArgs fs as
    return $ arg : args

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
    let passThrough = do
            rest <- takeUntilNewline
            let str = '`' : directive ++ rest
            tok Spe_Directive (posOrig, ' ', [], strOrig) (length str)

    condStack <- gets lsCondStack
    if any (/= CurrentlyTrue) condStack
        && not (elem directive unskippableDirectives)
    then alexMonadScan
    else case directive of

        "timescale" -> dropUntilNewline

        "celldefine" -> passThrough
        "endcelldefine" -> passThrough

        "unconnected_drive" -> passThrough
        "nounconnected_drive" -> passThrough

        "default_nettype" -> passThrough
        "pragma" -> passThrough
        "resetall" -> passThrough

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

        "line" -> do
            lineNumber <- takeNumber
            quotedFilename <- takeQuotedString
            _ <- takeNumber -- level, ignored
            let filename = init $ tail quotedFilename
            setCurrentFile filename
            (AlexPn f _ c, prev, _, str) <- alexGetInput
            alexSetInput (AlexPn f (lineNumber + 1) c, prev, [], str)
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
                    (AlexPn _ l c, _, _, _) <- alexGetInput
                    replacement <- if null formalArgs
                        then return body
                        else do
                            actualArgs <- takeMacroArguments
                            defaultedArgs <- defaultMacroArgs (map snd formalArgs) actualArgs
                            return $ substituteArgs body (map fst formalArgs) defaultedArgs
                    -- save our current state
                    currInput <- alexGetInput
                    currToks <- gets lsToks
                    modify $ \s -> s { lsToks = [] }
                    -- lex the macro expansion, preserving the file and line
                    alexSetInput (AlexPn 0 l 0, ' ', [], replacement)
                    alexMonadScan
                    -- re-tag and save tokens from the macro expansion
                    newToks <- gets lsToks
                    currFile <- getCurrentFile
                    let loc = "macro expansion of " ++ directive ++ " at " ++ currFile
                    let pos = Position loc l (c - length directive - 1)
                    let reTag (Token a b _) = Token a b pos
                    let boundary = Token MacroBoundary "" (Position "" 0 0)
                    let boundedToks = boundary : (map reTag newToks) ++ boundary : currToks
                    modify $ \s -> s { lsToks = boundedToks }
                    -- continue lexing after the macro
                    alexSetInput currInput
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
push t s = s { lsToks = t : (lsToks s) }

tok :: TokenName -> Action
tok tokId (pos, _, _, input) len = do
    let tokStr = take len input
    tokPos <- toTokPos pos
    condStack <- gets lsCondStack
    () <- if any (/= CurrentlyTrue) condStack
        then modify id
        else modify (push $ Token tokId tokStr tokPos)
    alexMonadScan
}
