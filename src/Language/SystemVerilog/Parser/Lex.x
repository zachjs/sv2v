{
module Language.SystemVerilog.Parser.Lex (alexScanTokens) where

import Language.SystemVerilog.Parser.Tokens
}

%wrapper "posn"

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

@size = @unsignedNumber " "?

@decimalNumber = @size? @decimalBase " "? @unsignedNumber
@binaryNumber  = @size? @binaryBase  " "? @binaryValue
@octalNumber   = @size? @octalBase   " "? @octalValue
@hexNumber     = @size? @hexBase     " "? @hexValue

@unbasedUnsizedLiteral = "'" ( 0 | 1 | x | X | z | Z )

@number
  = @unsignedNumber
  | @decimalNumber
  | @octalNumber
  | @binaryNumber
  | @hexNumber
  | @unbasedUnsizedLiteral

-- Strings

@string = \" [^\r\n]* \"

-- Identifiers

@escapedIdentifier = "\" ($printable # $white)+ $white
@simpleIdentifier  = [a-zA-Z_] [a-zA-Z0-9_\$]*
@systemIdentifier = "$" [a-zA-Z0-9_\$]+

-- Comments

@commentBegin = "/*"
@commentEnd = "*/" | "**/"
@comment = "//" [^\n]* | "/**/"

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

  @comment           { tok Spe_Comment }
  @commentBegin      { tok Spe_CommentBegin }
  @commentEnd        { tok Spe_CommentEnd }
  @directive         { tok Spe_Directive }
  @newline           { tok Spe_Newline }

  @whitespace        ;

  .                  { tok Unknown }

{
tok :: TokenName -> AlexPosn -> String -> Token
tok t (AlexPn _ l c) s = Token t s $ Position "" l c
}
