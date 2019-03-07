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

@decimalBase = "'" [dD]
@binaryBase  = "'" [bB]
@octalBase   = "'" [oO]
@hexBase     = "'" [hH]

@binaryValue = @binaryDigit ("_" | @binaryDigit)*
@octalValue  = @octalDigit  ("_" | @octalDigit)*
@hexValue    = @hexDigit    ("_" | @hexDigit)*

@unsignedNumber = $decimalDigit ("_" | $decimalDigit)*

@size = @unsignedNumber

@decimalNumber
  = @unsignedNumber
  | @size? @decimalBase @unsignedNumber

@binaryNumber = @size? @binaryBase @binaryValue
@octalNumber  = @size? @octalBase  @octalValue
@hexNumber    = @size? @hexBase    @hexValue

@number = @decimalNumber | @octalNumber | @binaryNumber | @hexNumber

-- Strings

@string = \" [^\r\n]* \"

-- Identifiers

@escapedIdentifier = "\" ($printable # $white)+ $white
@simpleIdentifier  = [a-zA-Z_] [a-zA-Z0-9_\$]*
@systemIdentifier = "$" [a-zA-Z0-9_\$]+


tokens :-

  "always"           { tok KW_always       }
  "always_comb"      { tok KW_always_comb  }
  "always_ff"        { tok KW_always_ff    }
  "always_latch"     { tok KW_always_latch }
  "assign"           { tok KW_assign       }
  "automatic"        { tok KW_automatic    }
  "begin"            { tok KW_begin        }
  "case"             { tok KW_case         }
  "casex"            { tok KW_casex        }
  "casez"            { tok KW_casez        }
  "default"          { tok KW_default      }
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
  "integer"          { tok KW_integer      }
  "interface"        { tok KW_interface    }
  "localparam"       { tok KW_localparam   }
  "logic"            { tok KW_logic        }
  "modport"          { tok KW_modport      }
  "module"           { tok KW_module       }
  "negedge"          { tok KW_negedge      }
  "or"               { tok KW_or           }
  "output"           { tok KW_output       }
  "packed"           { tok KW_packed       }
  "parameter"        { tok KW_parameter    }
  "posedge"          { tok KW_posedge      }
  "reg"              { tok KW_reg          }
  "repeat"           { tok KW_repeat       }
  "return"           { tok KW_return       }
  "static"           { tok KW_static       }
  "struct"           { tok KW_struct       }
  "task"             { tok KW_task         }
  "typedef"          { tok KW_typedef      }
  "unique"           { tok KW_unique       }
  "while"            { tok KW_while        }
  "wire"             { tok KW_wire         }

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

  $white             ;

  .                  { tok Unknown }

{
tok :: TokenName -> AlexPosn -> String -> Token
tok t (AlexPn _ l c) s = Token t s $ Position "" l c
}
