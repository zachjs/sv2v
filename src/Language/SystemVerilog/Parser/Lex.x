{
{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Original Lexer Author: Tom Hawkins <tomahawkins@gmail.com>
 - vim: filetype=haskell
 -
 - SystemVerilog Lexer
 -
 - All preprocessor directives are handled separately by the preprocessor. The
 - `begin_keywords` and `end_keywords` lexer directives are handled here.
 -}

module Language.SystemVerilog.Parser.Lex
    ( lexStr
    ) where

import Control.Monad.Except
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Language.SystemVerilog.Parser.Keywords (specMap)
import Language.SystemVerilog.Parser.Tokens
}

%wrapper "posn"

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

    @realNumber        { tok Lit_real }
    @integralNumber    { tok Lit_number }
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

    "`celldefine"          { tok Dir_celldefine }
    "`endcelldefine"       { tok Dir_endcelldefine }
    "`unconnected_drive"   { tok Dir_unconnected_drive }
    "`nounconnected_drive" { tok Dir_nounconnected_drive }
    "`default_nettype"     { tok Dir_default_nettype }
    "`resetall"            { tok Dir_resetall }
    "`begin_keywords"      { tok Dir_begin_keywords }
    "`end_keywords"        { tok Dir_end_keywords }

    $white             ;

    .                  { tok Unknown }

{
-- lexer entrypoint
lexStr :: String -> [Position] -> Either String [Token]
lexStr chars positions =
    runExcept $ postProcess [] tokens
    where
        tokensRaw = alexScanTokens chars
        positionsVec = Vector.fromList positions
        tokens = map (\tkf -> tkf positionsVec) tokensRaw

-- process begin/end keywords directives
postProcess :: [Set.Set TokenName] -> [Token] -> Except String [Token]
postProcess stack [] =
    if null stack
        then return []
        else throwError $ "unterminated begin_keywords blocks: " ++ show stack
postProcess stack (Token Dir_begin_keywords _ pos : ts) =
    case ts of
        Token Lit_string quotedSpec _ : ts' ->
            case Map.lookup spec specMap of
                Nothing -> throwError $ show pos
                    ++ ": invalid keyword set name: " ++ show spec
                Just set -> postProcess (set : stack) ts'
            where spec = tail $ init quotedSpec
        _ -> throwError $ show pos ++ ": begin_keywords not followed by string"
postProcess stack (Token Dir_end_keywords _ pos : ts) =
    case stack of
        (_ : stack') -> postProcess stack' ts
        [] -> throwError $ show pos ++ ": unmatched end_keywords"
postProcess [] (t : ts) = do
    ts' <- postProcess [] ts
    return $ t : ts'
postProcess stack (t : ts) = do
    ts' <- postProcess stack ts
    return $ t' : ts'
    where
        Token tokId str pos = t
        t' = if Set.member tokId (head stack)
                then Token Id_simple ('_' : str) pos
                else t

tok :: TokenName -> AlexPosn -> String -> Vector.Vector Position -> Token
tok tokId (AlexPn charPos _ _) tokStr positions =
    Token tokId tokStr tokPos
    where tokPos = positions Vector.! charPos
}
