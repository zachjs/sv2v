{- sv2v
 - Author: Zachary Snow <zach@zachjs.com>
 - Original Parser Author: Tom Hawkins <tomahawkins@gmail.com>
 - vim: filetype=haskell
 -
 - This file has been *heavily* modified and extended from the original version
 - in tomahawkins/verilog. I have added support for numerous SystemVerilog
 - constructs, which has necessitated rewriting nearly all of this.
 -
 - This file is the only remaining one that still uses 2-space indentation. I've
 - decided to leave it this way because I think it is too important to preserve
 - the ability to easily blame/diff this file.
 -}
{
{-# LANGUAGE BlockArguments #-}
module Language.SystemVerilog.Parser.Parse (parse) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import Language.SystemVerilog.AST
import Language.SystemVerilog.Parser.ParseDecl
import Language.SystemVerilog.Parser.Tokens
}

%monad { ParseState }
%lexer { positionKeep } { TokenEOF }
%name parse
%tokentype { Token }
%error { parseError }

%expect 0

%token

"$bits"                { Token KW_dollar_bits                _ _ }
"$dimensions"          { Token KW_dollar_dimensions          _ _ }
"$unpacked_dimensions" { Token KW_dollar_unpacked_dimensions _ _ }
"$left"                { Token KW_dollar_left                _ _ }
"$right"               { Token KW_dollar_right               _ _ }
"$low"                 { Token KW_dollar_low                 _ _ }
"$high"                { Token KW_dollar_high                _ _ }
"$increment"           { Token KW_dollar_increment           _ _ }
"$size"                { Token KW_dollar_size                _ _ }

"accept_on"        { Token KW_accept_on    _ _ }
"alias"            { Token KW_alias        _ _ }
"always"           { Token KW_always       _ _ }
"always_comb"      { Token KW_always_comb  _ _ }
"always_ff"        { Token KW_always_ff    _ _ }
"always_latch"     { Token KW_always_latch _ _ }
"and"              { Token KW_and          _ _ }
"assert"           { Token KW_assert       _ _ }
"assign"           { Token KW_assign       _ _ }
"assume"           { Token KW_assume       _ _ }
"automatic"        { Token KW_automatic    _ _ }
"before"           { Token KW_before       _ _ }
"begin"            { Token KW_begin        _ _ }
"bind"             { Token KW_bind         _ _ }
"bins"             { Token KW_bins         _ _ }
"binsof"           { Token KW_binsof       _ _ }
"bit"              { Token KW_bit          _ _ }
"break"            { Token KW_break        _ _ }
"buf"              { Token KW_buf          _ _ }
"bufif0"           { Token KW_bufif0       _ _ }
"bufif1"           { Token KW_bufif1       _ _ }
"byte"             { Token KW_byte         _ _ }
"case"             { Token KW_case         _ _ }
"casex"            { Token KW_casex        _ _ }
"casez"            { Token KW_casez        _ _ }
"cell"             { Token KW_cell         _ _ }
"chandle"          { Token KW_chandle      _ _ }
"checker"          { Token KW_checker      _ _ }
"class"            { Token KW_class        _ _ }
"clocking"         { Token KW_clocking     _ _ }
"cmos"             { Token KW_cmos         _ _ }
"config"           { Token KW_config       _ _ }
"const"            { Token KW_const        _ _ }
"constraint"       { Token KW_constraint   _ _ }
"context"          { Token KW_context      _ _ }
"continue"         { Token KW_continue     _ _ }
"cover"            { Token KW_cover        _ _ }
"covergroup"       { Token KW_covergroup   _ _ }
"coverpoint"       { Token KW_coverpoint   _ _ }
"cross"            { Token KW_cross        _ _ }
"deassign"         { Token KW_deassign     _ _ }
"default"          { Token KW_default      _ _ }
"defparam"         { Token KW_defparam     _ _ }
"design"           { Token KW_design       _ _ }
"disable"          { Token KW_disable      _ _ }
"dist"             { Token KW_dist         _ _ }
"do"               { Token KW_do           _ _ }
"edge"             { Token KW_edge         _ _ }
"else"             { Token KW_else         _ _ }
"end"              { Token KW_end          _ _ }
"endcase"          { Token KW_endcase      _ _ }
"endchecker"       { Token KW_endchecker   _ _ }
"endclass"         { Token KW_endclass     _ _ }
"endclocking"      { Token KW_endclocking  _ _ }
"endconfig"        { Token KW_endconfig    _ _ }
"endfunction"      { Token KW_endfunction  _ _ }
"endgenerate"      { Token KW_endgenerate  _ _ }
"endgroup"         { Token KW_endgroup     _ _ }
"endinterface"     { Token KW_endinterface _ _ }
"endmodule"        { Token KW_endmodule    _ _ }
"endpackage"       { Token KW_endpackage   _ _ }
"endprimitive"     { Token KW_endprimitive _ _ }
"endprogram"       { Token KW_endprogram   _ _ }
"endproperty"      { Token KW_endproperty  _ _ }
"endspecify"       { Token KW_endspecify   _ _ }
"endsequence"      { Token KW_endsequence  _ _ }
"endtable"         { Token KW_endtable     _ _ }
"endtask"          { Token KW_endtask      _ _ }
"enum"             { Token KW_enum         _ _ }
"event"            { Token KW_event        _ _ }
"eventually"       { Token KW_eventually   _ _ }
"expect"           { Token KW_expect       _ _ }
"export"           { Token KW_export       _ _ }
"extends"          { Token KW_extends      _ _ }
"extern"           { Token KW_extern       _ _ }
"final"            { Token KW_final        _ _ }
"first_match"      { Token KW_first_match  _ _ }
"for"              { Token KW_for          _ _ }
"force"            { Token KW_force        _ _ }
"foreach"          { Token KW_foreach      _ _ }
"forever"          { Token KW_forever      _ _ }
"fork"             { Token KW_fork         _ _ }
"forkjoin"         { Token KW_forkjoin     _ _ }
"function"         { Token KW_function     _ _ }
"generate"         { Token KW_generate     _ _ }
"genvar"           { Token KW_genvar       _ _ }
"global"           { Token KW_global       _ _ }
"highz0"           { Token KW_highz0       _ _ }
"highz1"           { Token KW_highz1       _ _ }
"if"               { Token KW_if           _ _ }
"iff"              { Token KW_iff          _ _ }
"ifnone"           { Token KW_ifnone       _ _ }
"ignore_bins"      { Token KW_ignore_bins  _ _ }
"illegal_bins"     { Token KW_illegal_bins _ _ }
"implements"       { Token KW_implements   _ _ }
"implies"          { Token KW_implies      _ _ }
"import"           { Token KW_import       _ _ }
"incdir"           { Token KW_incdir       _ _ }
"include"          { Token KW_include      _ _ }
"initial"          { Token KW_initial      _ _ }
"inout"            { Token KW_inout        _ _ }
"input"            { Token KW_input        _ _ }
"inside"           { Token KW_inside       _ _ }
"instance"         { Token KW_instance     _ _ }
"int"              { Token KW_int          _ _ }
"integer"          { Token KW_integer      _ _ }
"interconnect"     { Token KW_interconnect _ _ }
"interface"        { Token KW_interface    _ _ }
"intersect"        { Token KW_intersect    _ _ }
"join"             { Token KW_join         _ _ }
"join_any"         { Token KW_join_any     _ _ }
"join_none"        { Token KW_join_none    _ _ }
"large"            { Token KW_large        _ _ }
"let"              { Token KW_let          _ _ }
"liblist"          { Token KW_liblist      _ _ }
"library"          { Token KW_library      _ _ }
"local"            { Token KW_local        _ _ }
"localparam"       { Token KW_localparam   _ _ }
"logic"            { Token KW_logic        _ _ }
"longint"          { Token KW_longint      _ _ }
"macromodule"      { Token KW_macromodule  _ _ }
"matches"          { Token KW_matches      _ _ }
"medium"           { Token KW_medium       _ _ }
"modport"          { Token KW_modport      _ _ }
"module"           { Token KW_module       _ _ }
"nand"             { Token KW_nand         _ _ }
"negedge"          { Token KW_negedge      _ _ }
"nettype"          { Token KW_nettype      _ _ }
"new"              { Token KW_new          _ _ }
"nexttime"         { Token KW_nexttime     _ _ }
"nmos"             { Token KW_nmos         _ _ }
"nor"              { Token KW_nor          _ _ }
"noshowcancelled"  { Token KW_noshowcancelled _ _ }
"not"              { Token KW_not          _ _ }
"notif0"           { Token KW_notif0       _ _ }
"notif1"           { Token KW_notif1       _ _ }
"null"             { Token KW_null         _ _ }
"or"               { Token KW_or           _ _ }
"output"           { Token KW_output       _ _ }
"package"          { Token KW_package      _ _ }
"packed"           { Token KW_packed       _ _ }
"parameter"        { Token KW_parameter    _ _ }
"pmos"             { Token KW_pmos         _ _ }
"posedge"          { Token KW_posedge      _ _ }
"primitive"        { Token KW_primitive    _ _ }
"priority"         { Token KW_priority     _ _ }
"program"          { Token KW_program      _ _ }
"property"         { Token KW_property     _ _ }
"protected"        { Token KW_protected    _ _ }
"pull0"            { Token KW_pull0        _ _ }
"pull1"            { Token KW_pull1        _ _ }
"pulldown"         { Token KW_pulldown     _ _ }
"pullup"           { Token KW_pullup       _ _ }
"pulsestyle_ondetect" { Token KW_pulsestyle_ondetect _ _ }
"pulsestyle_onevent"  { Token KW_pulsestyle_onevent _ _ }
"pure"             { Token KW_pure         _ _ }
"rand"             { Token KW_rand         _ _ }
"randc"            { Token KW_randc        _ _ }
"randcase"         { Token KW_randcase     _ _ }
"randsequence"     { Token KW_randsequence _ _ }
"rcmos"            { Token KW_rcmos        _ _ }
"real"             { Token KW_real         _ _ }
"realtime"         { Token KW_realtime     _ _ }
"ref"              { Token KW_ref          _ _ }
"reg"              { Token KW_reg          _ _ }
"reject_on"        { Token KW_reject_on    _ _ }
"release"          { Token KW_release      _ _ }
"repeat"           { Token KW_repeat       _ _ }
"restrict"         { Token KW_restrict     _ _ }
"return"           { Token KW_return       _ _ }
"rnmos"            { Token KW_rnmos        _ _ }
"rpmos"            { Token KW_rpmos        _ _ }
"rtran"            { Token KW_rtran        _ _ }
"rtranif0"         { Token KW_rtranif0     _ _ }
"rtranif1"         { Token KW_rtranif1     _ _ }
"s_always"         { Token KW_s_always     _ _ }
"s_eventually"     { Token KW_s_eventually _ _ }
"s_nexttime"       { Token KW_s_nexttime   _ _ }
"s_until"          { Token KW_s_until      _ _ }
"s_until_with"     { Token KW_s_until_with _ _ }
"scalared"         { Token KW_scalared     _ _ }
"sequence"         { Token KW_sequence     _ _ }
"shortint"         { Token KW_shortint     _ _ }
"shortreal"        { Token KW_shortreal    _ _ }
"showcancelled"    { Token KW_showcancelled _ _ }
"signed"           { Token KW_signed       _ _ }
"small"            { Token KW_small        _ _ }
"soft"             { Token KW_soft         _ _ }
"solve"            { Token KW_solve        _ _ }
"specify"          { Token KW_specify      _ _ }
"specparam"        { Token KW_specparam    _ _ }
"static"           { Token KW_static       _ _ }
"string"           { Token KW_string       _ _ }
"strong"           { Token KW_strong       _ _ }
"strong0"          { Token KW_strong0      _ _ }
"strong1"          { Token KW_strong1      _ _ }
"struct"           { Token KW_struct       _ _ }
"super"            { Token KW_super        _ _ }
"supply0"          { Token KW_supply0      _ _ }
"supply1"          { Token KW_supply1      _ _ }
"sync_accept_on"   { Token KW_sync_accept_on _ _ }
"sync_reject_on"   { Token KW_sync_reject_on _ _ }
"table"            { Token KW_table        _ _ }
"tagged"           { Token KW_tagged       _ _ }
"task"             { Token KW_task         _ _ }
"this"             { Token KW_this         _ _ }
"throughout"       { Token KW_throughout   _ _ }
"time"             { Token KW_time         _ _ }
"timeprecision"    { Token KW_timeprecision _ _ }
"timeunit"         { Token KW_timeunit     _ _ }
"tran"             { Token KW_tran         _ _ }
"tranif0"          { Token KW_tranif0      _ _ }
"tranif1"          { Token KW_tranif1      _ _ }
"tri"              { Token KW_tri          _ _ }
"tri0"             { Token KW_tri0         _ _ }
"tri1"             { Token KW_tri1         _ _ }
"triand"           { Token KW_triand       _ _ }
"trior"            { Token KW_trior        _ _ }
"trireg"           { Token KW_trireg       _ _ }
"type"             { Token KW_type         _ _ }
"typedef"          { Token KW_typedef      _ _ }
"union"            { Token KW_union        _ _ }
"unique"           { Token KW_unique       _ _ }
"unique0"          { Token KW_unique0      _ _ }
"unsigned"         { Token KW_unsigned     _ _ }
"until"            { Token KW_until        _ _ }
"until_with"       { Token KW_until_with   _ _ }
"untyped"          { Token KW_untyped      _ _ }
"use"              { Token KW_use          _ _ }
"uwire"            { Token KW_uwire        _ _ }
"var"              { Token KW_var          _ _ }
"vectored"         { Token KW_vectored     _ _ }
"virtual"          { Token KW_virtual      _ _ }
"void"             { Token KW_void         _ _ }
"wait"             { Token KW_wait         _ _ }
"wait_order"       { Token KW_wait_order   _ _ }
"wand"             { Token KW_wand         _ _ }
"weak"             { Token KW_weak         _ _ }
"weak0"            { Token KW_weak0        _ _ }
"weak1"            { Token KW_weak1        _ _ }
"while"            { Token KW_while        _ _ }
"wildcard"         { Token KW_wildcard     _ _ }
"wire"             { Token KW_wire         _ _ }
"with"             { Token KW_with         _ _ }
"within"           { Token KW_within       _ _ }
"wor"              { Token KW_wor          _ _ }
"xnor"             { Token KW_xnor         _ _ }
"xor"              { Token KW_xor          _ _ }

simpleIdentifier   { Token Id_simple       _ _ }
escapedIdentifier  { Token Id_escaped      _ _ }
systemIdentifier   { Token Id_system       _ _ }
real               { Token Lit_real        _ _ }
number             { Token Lit_number      _ _ }
string             { Token Lit_string      _ _ }
time               { Token Lit_time        _ _ }

"`celldefine"          { Token Dir_celldefine _ _ }
"`endcelldefine"       { Token Dir_endcelldefine _ _ }
"`unconnected_drive"   { Token Dir_unconnected_drive _ _ }
"`nounconnected_drive" { Token Dir_nounconnected_drive _ _ }
"`default_nettype"     { Token Dir_default_nettype _ _ }
"`resetall"            { Token Dir_resetall _ _ }
"`begin_keywords"      { Token Dir_begin_keywords _ _ }
"`end_keywords"        { Token Dir_end_keywords _ _ }

"("                { Token Sym_paren_l _ _ }
")"                { Token Sym_paren_r _ _ }
"["                { Token Sym_brack_l _ _ }
"]"                { Token Sym_brack_r _ _ }
"{"                { Token Sym_brace_l _ _ }
"}"                { Token Sym_brace_r _ _ }
"~"                { Token Sym_tildy _ _ }
"!"                { Token Sym_bang _ _ }
"@"                { Token Sym_at _ _ }
"#"                { Token Sym_pound _ _ }
"%"                { Token Sym_percent _ _ }
"^"                { Token Sym_hat _ _ }
"&"                { Token Sym_amp _ _ }
"|"                { Token Sym_bar _ _ }
"*"                { Token Sym_aster _ _ }
"."                { Token Sym_dot _ _ }
","                { Token Sym_comma _ _ }
":"                { Token Sym_colon _ _ }
";"                { Token Sym_semi _ _ }
"="                { Token Sym_eq _ _ }
"<"                { Token Sym_lt _ _ }
">"                { Token Sym_gt _ _ }
"+"                { Token Sym_plus _ _ }
"-"                { Token Sym_dash _ _ }
"?"                { Token Sym_question _ _ }
"/"                { Token Sym_slash _ _ }
"$"                { Token Sym_dollar _ _ }
"'"                { Token Sym_s_quote _ _ }
"~&"               { Token Sym_tildy_amp _ _ }
"~|"               { Token Sym_tildy_bar _ _ }
"~^"               { Token Sym_tildy_hat _ _ }
"^~"               { Token Sym_hat_tildy _ _ }
"=="               { Token Sym_eq_eq _ _ }
"!="               { Token Sym_bang_eq _ _ }
"&&"               { Token Sym_amp_amp _ _ }
"||"               { Token Sym_bar_bar _ _ }
"**"               { Token Sym_aster_aster _ _ }
"<="               { Token Sym_lt_eq _ _ }
">="               { Token Sym_gt_eq _ _ }
">>"               { Token Sym_gt_gt _ _ }
"<<"               { Token Sym_lt_lt _ _ }
"++"               { Token Sym_plus_plus _ _ }
"--"               { Token Sym_dash_dash _ _ }
"+="               { Token Sym_plus_eq _ _ }
"-="               { Token Sym_dash_eq _ _ }
"*="               { Token Sym_aster_eq _ _ }
"/="               { Token Sym_slash_eq _ _ }
"%="               { Token Sym_percent_eq _ _ }
"&="               { Token Sym_amp_eq _ _ }
"|="               { Token Sym_bar_eq _ _ }
"^="               { Token Sym_hat_eq _ _ }
"+:"               { Token Sym_plus_colon _ _ }
"-:"               { Token Sym_dash_colon _ _ }
"::"               { Token Sym_colon_colon _ _ }
".*"               { Token Sym_dot_aster _ _ }
"->"               { Token Sym_dash_gt _ _ }
":="               { Token Sym_colon_eq _ _ }
":/"               { Token Sym_colon_slash _ _ }
"##"               { Token Sym_pound_pound _ _ }
"[*"               { Token Sym_brack_l_aster _ _ }
"[="               { Token Sym_brack_l_eq _ _ }
"=>"               { Token Sym_eq_gt _ _ }
"@*"               { Token Sym_at_aster _ _ }
"(*"               { Token Sym_paren_l_aster _ _ }
"*)"               { Token Sym_aster_paren_r _ _ }
"*>"               { Token Sym_aster_gt _ _ }
"==="              { Token Sym_eq_eq_eq _ _ }
"!=="              { Token Sym_bang_eq_eq _ _ }
"==?"              { Token Sym_eq_eq_question _ _ }
"!=?"              { Token Sym_bang_eq_question _ _ }
">>>"              { Token Sym_gt_gt_gt _ _ }
"<<<"              { Token Sym_lt_lt_lt _ _ }
"<<="              { Token Sym_lt_lt_eq _ _ }
">>="              { Token Sym_gt_gt_eq _ _ }
"<->"              { Token Sym_lt_dash_gt _ _ }
"|->"              { Token Sym_bar_dash_gt _ _ }
"|=>"              { Token Sym_bar_eq_gt _ _ }
"[->"              { Token Sym_brack_l_dash_gt _ _ }
"#-#"              { Token Sym_pound_dash_pound _ _ }
"#=#"              { Token Sym_pound_eq_pound _ _ }
"@@("              { Token Sym_at_at_paren_l _ _ }
"(*)"              { Token Sym_paren_l_aster_paren_r _ _ }
"->>"              { Token Sym_dash_gt_gt _ _ }
"&&&"              { Token Sym_amp_amp_amp _ _ }
"<<<="             { Token Sym_lt_lt_lt_eq _ _ }
">>>="             { Token Sym_gt_gt_gt_eq _ _ }


-- operator precedences, from *lowest* to *highest*
%nonassoc NoElse
%nonassoc "else"
%right  "|->" "|=>" "#-#" "#=#"
%right "iff"
%left "or"
%left "and"
%left "intersect"
%left "within"
%right "throughout"
%left "##"
%nonassoc "[*]" "[=]" "[->]"
%right "->" "<->"
%right "?" ":"
%left  "||"
%left  "&&"
%left  "|" "~|"
%left  "^" "^~" "~^"
%left  "&" "~&"
%left  "==" "!=" "===" "!==" "==?" "!=?"
%left  "<" "<=" ">" ">=" "inside" "dist"
%left  "<<" ">>" "<<<" ">>>"
%left  "+" "-"
%left  "*" "/" "%"
%left  "**"
%right REDUCE_OP "!" "~" "++" "--"
%left "'"
%left  "(" ")" "[" "]" "." "::" "#"

%%

opt(p)
  : p { Just $1 }
  |   { Nothing }

Descriptions :: { [Description] }
  : {- empty -}              { [] }
  | Descriptions ";"         { $1 }
  | Descriptions Description { $1 ++ $2 }

Description :: { [Description] }
  : Part(ModuleKW   , "endmodule"   ) { [$1] }
  | Part(InterfaceKW, "endinterface") { [$1] }
  | PackageDeclaration                { [$1] }
  | PackageItem { map PackageItem $1 }

Type :: { Type }
  : TypeNonIdent { $1 }
  | TypeAlias    { $1 }
TypeAlias :: { Type }
  :                               Identifier Dimensions { Alias   $1       $2 }
  | Identifier               "::" Identifier Dimensions { PSAlias $1    $3 $4 }
  | Identifier ParamBindings "::" Identifier Dimensions { CSAlias $1 $2 $4 $5 }
TypeNonIdent :: { Type }
  : PartialType OptSigning Dimensions { $1 $2 $3 }
  | "type" "(" Expr ")" { TypeOf $3 }
PartialType :: { Signing -> [Range] -> Type }
  : NetTypeAndStrength                      {                        Net           $1    }
  | IntegerVectorType                       {                        IntegerVector $1    }
  | IntegerAtomType                         { \sg          -> \[] -> IntegerAtom   $1 sg }
  | NonIntegerType                          { \Unspecified -> \[] -> NonInteger    $1    }
  | "enum" EnumBaseType "{" EnumItems   "}" { \Unspecified -> Enum   $2 $4 }
  | "struct" Packing    "{" StructItems "}" { \Unspecified -> Struct $2 $4 }
  | "union"  Packing    "{" StructItems "}" { \Unspecified -> Union  $2 $4 }
CastingType :: { Type }
  : IntegerVectorType { IntegerVector $1 Unspecified [] }
  | IntegerAtomType   { IntegerAtom   $1 Unspecified    }
  | NonIntegerType    { NonInteger    $1                }
  | Signing           { Implicit      $1             [] }
EnumBaseType :: { Type }
  : Type       { $1 }
  | Dimensions { Implicit Unspecified $1 }

NetTypeAndStrength :: { NetTypeAndStrength }
  : NetType                %prec "+" { NetType       $1    }
  | NetType DriveStrength  %prec "*" { NetTypeDrive  $1 $2 }
  | NetType ChargeStrength %prec "*" { NetTypeCharge $1 $2 }

Signing :: { Signing }
  : "signed"   { Signed   }
  | "unsigned" { Unsigned }
OptSigning :: { Signing }
  : Signing { $1 }
  | {- empty -} { Unspecified }

NetType :: { NetType }
  : "supply0"   { TSupply0   }
  | "supply1"   { TSupply1   }
  | "tri"       { TTri       }
  | "triand"    { TTriand    }
  | "trior"     { TTrior     }
  | "trireg"    { TTrireg    }
  | "tri0"      { TTri0      }
  | "tri1"      { TTri1      }
  | "uwire"     { TUwire     }
  | "wire"      { TWire      }
  | "wand"      { TWand      }
  | "wor"       { TWor       }
IntegerVectorType :: { IntegerVectorType }
  : "bit"       { TBit       }
  | "logic"     { TLogic     }
  | "reg"       { TReg       }
IntegerAtomType :: { IntegerAtomType }
  : "byte"      { TByte      }
  | "shortint"  { TShortint  }
  | "int"       { TInt       }
  | "longint"   { TLongint   }
  | "integer"   { TInteger   }
  | "time"      { TTime      }
NonIntegerType :: { NonIntegerType }
  : "shortreal" { TShortreal }
  | "real"      { TReal      }
  | "realtime"  { TRealtime  }
  | "string"    { TString    }
  | "event"     { TEvent     }

EnumItems :: { [(Identifier, Expr)] }
  : VariablePortIdentifiers { $1 }

StructItems :: { [(Type, Identifier)] }
  : StructItem             { $1 }
  | StructItems StructItem { $1 ++ $2 }
StructItem :: { [(Type, Identifier)] }
  : Type FieldDecls ";" { map (fieldDecl $1) $2 }

FieldDecls :: { [(Identifier, [Range])] }
  : FieldDecl                 { [$1] }
  | FieldDecls "," FieldDecl  { $1 ++ [$3] }
FieldDecl :: { (Identifier, [Range]) }
  : Identifier Dimensions { ($1, $2) }

Packing :: { Packing }
  : "packed" OptSigning { Packed $2 }
  | {- empty -}         { Unpacked }

Part(begin, end) :: { Description }
  : AttributeInstances          begin PartHeader ModuleItems end opt(Tag) { $3 $1 False $2 $4 }
  | AttributeInstances "extern" begin PartHeader                          { $4 $1 True  $3 [] }

PartHeader :: { [Attr] -> Bool -> PartKW -> [ModuleItem] -> Description }
  : Lifetime Identifier PackageImportDeclarations Params PortDecls ";" { \attrs extern kw items -> Part attrs extern kw $1 $2 (fst $5) ($3 ++ $4 ++ (snd $5) ++ items) }

ModuleKW :: { PartKW }
  : "module" { Module }
  | "macromodule" { Module }
InterfaceKW :: { PartKW }
  : "interface" { Interface }

PackageDeclaration :: { Description }
  : "package" Lifetime Identifier ";" PackageItems "endpackage" opt(Tag) { Package $2 $3 $5 }

Tag :: { Identifier }
  : ":" Identifier { $2 }

StrTag :: { Identifier }
  : {- empty -}    { "" }
  | ":" Identifier { $2 }

PackageImportDeclarations :: { [ModuleItem] }
  : PackageImportDeclaration PackageImportDeclarations { $1 ++ $2 }
  | {- empty -}                                        { [] }

PackageImportDeclaration :: { [ModuleItem] }
  : "import" PackageImportItems ";" { map (MIPackageItem . uncurry Import) $2 }

Params :: { [ModuleItem] }
  : {- empty -}          { [] }
  | "#" "(" ")"          { [] }
  | "#" "(" ParamsFollow { map (MIPackageItem . Decl) $3 }
ParamsFollow :: { [Decl] }
  : ParamAsgn ")"              { [$1] }
  | ParamAsgn "," ParamsFollow { $1 : $3 }
  |               ParamsDecl   { $1 }
ParamsDecl :: { [Decl] }
  : ModuleParameterDecl(")")            { $1 }
  | ModuleParameterDecl(",") ParamsDecl { $1 ++ $2 }
ParamAsgn :: { Decl }
  : Identifier "=" Expr { Param Parameter (Implicit Unspecified []) $1 $3 }

PortDecls :: { ([Identifier], [ModuleItem]) }
  : "(" PortDeclTokens(")") { parseDTsAsPortDecls $2 }
  | "("                ")"  { ([], []) }
  | {- empty -}             { ([], []) }

ModportItems :: { [(Identifier, [ModportDecl])] }
  : ModportItem                  { [$1] }
  | ModportItems "," ModportItem { $1 ++ [$3] }
ModportItem :: { (Identifier, [ModportDecl]) }
  : Identifier "(" ModportPortsDeclarations { ($1, $3) }
ModportPortsDeclarations :: { [ModportDecl] }
  : ModportPortsDeclaration(")")                          { $1 }
  | ModportPortsDeclaration(",") ModportPortsDeclarations { $1 ++ $2 }
ModportPortsDeclaration(delim) :: { [ModportDecl] }
  : ModportSimplePortsDeclaration(delim) { $1 }
ModportSimplePortsDeclaration(delim) :: { [ModportDecl] }
  : Direction ModportSimplePorts delim { map (\(a, b) -> ($1, a, b)) $2 }
ModportSimplePorts :: { [(Identifier, Expr)] }
  : ModportSimplePort                        { [$1] }
  | ModportSimplePorts "," ModportSimplePort { $1 ++ [$3] }
ModportSimplePort :: { (Identifier, Expr) }
  : "." Identifier "(" ExprOrNil ")" { ($2, $4) }
  | Identifier                       { ($1, Ident $1) }

Identifier :: { Identifier }
  : simpleIdentifier  { tokenString $1 }
  | escapedIdentifier { tokenString $1 }
  | systemIdentifier  { tokenString $1 }

Identifiers :: { [Identifier] }
  :                 Identifier { [$1] }
  | Identifiers "," Identifier { $1 ++ [$3] }

-- uses delimiter propagation hack to avoid conflicts
DeclTokens(delim) :: { [DeclToken] }
  : DeclTokensBase(DeclTokens(delim), delim) { $1 }
DeclTokensBase(repeat, delim) :: { [DeclToken] }
  : DeclToken                delim  { [$1] }
  | DeclToken                repeat { [$1] ++ $2 }
  | Identifier ParamBindings repeat {% posInject \p -> [DTIdent p $1, DTParams p $2] ++ $3 }
  | DeclTokenAsgn ","        repeat {% posInject \p -> [$1, DTComma p] ++ $3 }
  | DeclTokenAsgn            delim  {% posInject \p -> [$1] }
DeclToken :: { DeclToken }
  : ","                                {% posInject \p -> DTComma    p }
  | "[" "]"                            {% posInject \p -> DTAutoDim  p }
  | PartSelect                         {% posInject \p -> DTRange    p $1 }
  | Identifier                         {% posInject \p -> DTIdent    p $1 }
  | Direction                          {% posInject \p -> DTDir      p $1 }
  | "[" Expr "]"                       {% posInject \p -> DTBit      p $2 }
  | LHSConcat                          {% posInject \p -> DTConcat   p $1 }
  | PartialType                        {% posInject \p -> DTType     p $1 }
  | "." Identifier                     {% posInject \p -> DTDot      p $2 }
  | PortBindings                       {% posInject \p -> DTInstance p $1 }
  | Signing                            {% posInject \p -> DTSigning  p $1 }
  | ExplicitLifetime                   {% posInject \p -> DTLifetime p $1 }
  | "const" PartialType                {% posInject \p -> DTType     p $2 }
  | "{" StreamOp StreamSize Concat "}" {% posInject \p -> DTStream   p $2 $3           (map toLHS $4) }
  | "{" StreamOp            Concat "}" {% posInject \p -> DTStream   p $2 (RawNum 1) (map toLHS $3) }
  | opt("var") "type" "(" Expr ")"     {% posInject \p -> DTType     p (\Unspecified -> \[] -> TypeOf $4) }
  | "<=" opt(DelayOrEvent) Expr        {% posInject \p -> DTAsgn     p AsgnOpNonBlocking $2 $3 }
  | IncOrDecOperator                   {% posInject \p -> DTAsgn     p (AsgnOp $1) Nothing (RawNum 1) }
  | Identifier               "::" Identifier {% posInject \p -> DTPSIdent p $1    $3 }
  | Identifier ParamBindings "::" Identifier {% posInject \p -> DTCSIdent p $1 $2 $4 }
DeclTokenAsgn :: { DeclToken }
  : "=" opt(DelayOrEvent) Expr {% posInject \p -> DTAsgn p AsgnOpEq $2 $3 }
  | AsgnBinOp Expr             {% posInject \p -> DTAsgn p $1 Nothing $2 }
PortDeclTokens(delim) :: { [DeclToken] }
  : DeclTokensBase(PortDeclTokens(delim), delim) { $1 }
  | GenericInterfaceDecl   PortDeclTokens(delim) { $1 ++ $2}
  | GenericInterfaceDecl                  delim  { $1 }
  | AttributeInstance      PortDeclTokens(delim) {% posInject \p -> DTAttr p $1 : $2 }
GenericInterfaceDecl :: { [DeclToken] }
  : "interface" Identifier {% posInject \p -> [DTType p (\Unspecified -> InterfaceT "" Nothing), DTIdent p $2] }

VariablePortIdentifiers :: { [(Identifier, Expr)] }
  : VariablePortIdentifier                             { [$1] }
  | VariablePortIdentifiers "," VariablePortIdentifier { $1 ++ [$3] }
VariablePortIdentifier :: { (Identifier, Expr) }
  : Identifier          { ($1, Nil) }
  | Identifier "=" Expr { ($1, $3 ) }

Direction :: { Direction }
  : "inout"  { Inout  }
  | "input"  { Input  }
  | "output" { Output }

ModuleItems :: { [ModuleItem] }
  : {- empty -}                    { [] }
  | ";" ModuleItems                { $2 }
  | MITrace ModuleItem ModuleItems { $1 : $2 ++ $3 }

ModuleItem :: { [ModuleItem] }
  : NonGenerateModuleItem { $1 }
  | ConditionalGenerateConstruct      { [Generate [$1]] }
  | LoopGenerateConstruct             { [Generate [$1]] }
  | "generate" GenItems "endgenerate" { [Generate $2] }
NonGenerateModuleItem :: { [ModuleItem] }
  -- This item covers module instantiations and all declarations
  : DeclTokens(";")                      { parseDTsAsModuleItems $1 }
  | ParameterDecl(";")                   { map (MIPackageItem . Decl) $1 }
  | "defparam" LHSAsgns ";"              { map (uncurry Defparam) $2 }
  | "assign" AssignOption LHSAsgns ";"   { map (uncurry $ Assign $2) $3 }
  | AlwaysKW Stmt                        { [AlwaysC $1 $2] }
  | "initial" Stmt                       { [Initial $2] }
  | "final"   Stmt                       { [Final   $2] }
  | "genvar" Identifiers ";"             { map Genvar $2 }
  | "modport" ModportItems ";"           { map (uncurry Modport) $2 }
  | NonDeclPackageItem                   { map MIPackageItem $1 }
  | NInputGateKW  NInputGates  ";"       { map (\(a, b, c, d) -> NInputGate  $1 a b c d) $2 }
  | NOutputGateKW NOutputGates ";"       { map (\(a, b, c, d) -> NOutputGate $1 a b c d) $2 }
  | AttributeInstance ModuleItem         { map (MIAttr $1) $2 }
  | AssertionItem                        { [AssertionItem $1] }

AssignOption :: { AssignOption }
  : {- empty -}   { AssignOptionNone }
  | DelayControl  { AssignOptionDelay $1 }
  | DriveStrength { AssignOptionDrive $1 }

-- for ModuleItem, for now
AssertionItem :: { AssertionItem }
  : ConcurrentAssertionItem { $1 }

-- for Stmt, for now
ProceduralAssertionStatement :: { Assertion }
  : ConcurrentAssertionStatement { $1 }
  | ImmediateAssertionStatement  { $1 }

ConcurrentAssertionItem :: { AssertionItem }
  : Identifier ":" ConcurrentAssertionStatement { ($1, $3) }
  |                ConcurrentAssertionStatement { ("", $1) }
ConcurrentAssertionStatement :: { Assertion }
  : "assert" "property" "(" PropertySpec ")" ActionBlock { Assert (Left $4) $6 }
  | "assume" "property" "(" PropertySpec ")" ActionBlock { Assume (Left $4) $6 }
  | "cover"  "property" "(" PropertySpec ")" Stmt        { Cover  (Left $4) $6 }

ImmediateAssertionStatement :: { Assertion }
  : SimpleImmediateAssertionStatement { $1 }
SimpleImmediateAssertionStatement :: { Assertion }
  : "assert" "(" Expr ")" ActionBlock { Assert (Right $3) $5 }
  | "assume" "(" Expr ")" ActionBlock { Assume (Right $3) $5 }
  | "cover"  "(" Expr ")" Stmt        { Cover  (Right $3) $5 }

PropertySpec :: { PropertySpec }
  : opt(ClockingEvent) "disable" "iff" "(" Expr ")" PropExpr { PropertySpec $1 $5  $7 }
  | opt(ClockingEvent)                              PropExpr { PropertySpec $1 Nil $2 }

PropExpr :: { PropExpr }
  : SeqExpr { PropExpr $1 }
  | PropExprParens { $1 }
PropExprParens :: { PropExpr }
  : "(" PropExprParens ")" { $2 }
  | SeqExpr "|->" PropExpr { PropExprImpliesO  $1 $3 }
  | SeqExpr "|=>" PropExpr { PropExprImpliesNO $1 $3 }
  | SeqExpr "#-#" PropExpr { PropExprFollowsO  $1 $3 }
  | SeqExpr "#=#" PropExpr { PropExprFollowsNO $1 $3 }
  | PropExpr "iff" PropExpr { PropExprIff $1 $3 }
SeqExpr :: { SeqExpr }
  : Expr { SeqExpr $1 }
  | SeqExprParens { $1 }
SeqExprParens :: { SeqExpr }
  : "(" SeqExprParens ")" { $2 }
  | SeqExpr "and"        SeqExpr { SeqExprAnd        $1 $3 }
  | SeqExpr "or"         SeqExpr { SeqExprOr         $1 $3 }
  | SeqExpr "intersect"  SeqExpr { SeqExprIntersect  $1 $3 }
  | Expr    "throughout" SeqExpr { SeqExprThroughout $1 $3 }
  | SeqExpr "within"     SeqExpr { SeqExprWithin     $1 $3 }
  | SeqExpr "##" Number  SeqExpr { SeqExprDelay (Just $1) (Number $3) $4 }
  |         "##" Number  SeqExpr { SeqExprDelay (Nothing) (Number $2) $3 }
  | "first_match" "(" SeqExpr SeqMatchItems ")" { SeqExprFirstMatch $3 $4 }
SeqMatchItems :: { [SeqMatchItem] }
  : "," SeqMatchItem               { [$2] }
  | SeqMatchItems "," SeqMatchItem { $1 ++ [$3] }
SeqMatchItem :: { SeqMatchItem }
  : ForStepAssignment   { Left $1 }
  | Identifier CallArgs { Right ($1, $2) }

ActionBlock :: { ActionBlock }
  : Stmt %prec NoElse { ActionBlock $1 Null }
  |      "else" Stmt  { ActionBlock Null $2 }
  | Stmt "else" Stmt  { ActionBlock $1   $3 }

AttributeInstances :: { [Attr] }
  : {- empty -}                          { [] }
  | AttributeInstance AttributeInstances { $1 : $2 }
AttributeInstance :: { Attr }
  : "(*" AttrSpecs "*)" { Attr $2 }
AttrSpecs :: { [AttrSpec] }
  : AttrSpec               { [$1] }
  | AttrSpecs "," AttrSpec { $1 ++ [$3] }
AttrSpec :: { AttrSpec }
  : Identifier "=" Expr { ($1, $3 ) }
  | Identifier          { ($1, Nil) }

NInputGates :: { [(Expr, Identifier, LHS, [Expr])] }
  : NInputGate                 { [$1] }
  | NInputGates "," NInputGate { $1 ++ [$3]}
NOutputGates :: { [(Expr, Identifier, [LHS], Expr)] }
  : NOutputGate                  { [$1] }
  | NOutputGates "," NOutputGate { $1 ++ [$3]}

NInputGate :: { (Expr, Identifier, LHS, [Expr]) }
  : DelayControlOrNil opt(Identifier) "(" LHS "," Exprs ")" { ($1, fromMaybe "" $2, $4, $6) }
NOutputGate :: { (Expr, Identifier, [LHS], Expr) }
  : DelayControlOrNil opt(Identifier) "(" NOutputGateItems { ($1, fromMaybe "" $2, fst $4, snd $4) }
NOutputGateItems :: { ([LHS], Expr) }
  : Expr ")" { ([], $1) }
  | Expr "," NOutputGateItems { (toLHS $1 : fst $3, snd $3) }
DelayControlOrNil :: { Expr }
  : DelayControl { $1 }
  | {- empty -} { Nil }

NInputGateKW :: { NInputGateKW }
  : "and"  { GateAnd  }
  | "nand" { GateNand }
  | "or"   { GateOr   }
  | "nor"  { GateNor  }
  | "xor"  { GateXor  }
  | "xnor" { GateXnor }
NOutputGateKW :: { NOutputGateKW }
  : "buf"  { GateBuf  }
  | "not"  { GateNot  }

DriveStrength :: { DriveStrength }
  : "(" Strength0 "," Strength1 ")" { DriveStrength $2     $4     }
  | "(" Strength1 "," Strength0 ")" { DriveStrength $4     $2     }
  | "(" Strength0 "," "highz1"  ")" { DriveStrength $2     Highz1 }
  | "(" Strength1 "," "highz0"  ")" { DriveStrength Highz0 $2     }
  | "(" "highz0"  "," Strength1 ")" { DriveStrength Highz0 $4     }
  | "(" "highz1"  "," Strength0 ")" { DriveStrength $4     Highz1 }
Strength0 :: { Strength0 }
  : "supply0" { Supply0 }
  | "strong0" { Strong0 }
  | "pull0"   { Pull0   }
  | "weak0"   { Weak0   }
Strength1 :: { Strength1 }
  : "supply1" { Supply1 }
  | "strong1" { Strong1 }
  | "pull1"   { Pull1   }
  | "weak1"   { Weak1   }
ChargeStrength :: { ChargeStrength }
  : "(" "small"  ")" { Small  }
  | "(" "medium" ")" { Medium }
  | "(" "large"  ")" { Large  }

LHSAsgns :: { [(LHS, Expr)] }
  : LHSAsgn                   { [$1] }
  | LHSAsgns "," LHSAsgn { $1 ++ [$3] }
LHSAsgn :: { (LHS, Expr) }
  : LHS "=" Expr { ($1, $3) }

PackageItems :: { [PackageItem] }
  : {- empty -}                      { [] }
  | ";" PackageItems                 { $2 }
  | PITrace PackageItem PackageItems { $1 : $2 ++ $3 }
PackageItem :: { [PackageItem] }
  : DeclTokens(";")    { map Decl $ parseDTsAsDecls $1 }
  | ParameterDecl(";") { map (Decl . makeLocalparam) $1 }
  | NonDeclPackageItem { $1 }
NonDeclPackageItem :: { [PackageItem] }
  : "typedef" Type Identifier ";" { [Typedef $2 $3] }
  | "typedef" Type Identifier DimensionsNonEmpty ";" { [Typedef (UnpackedType $2 $4) $3] }
  | "function" Lifetime FuncRetAndName    TFItems DeclsAndStmts "endfunction" opt(Tag) { [Function $2 (fst $3) (snd $3) (map defaultFuncInput $ (map makeInput $4) ++ fst $5) (snd $5)] }
  | "function" Lifetime "void" Identifier TFItems DeclsAndStmts "endfunction" opt(Tag) { [Task     $2 $4                (map defaultFuncInput $ $5 ++ fst $6) (snd $6)] }
  | "task"     Lifetime Identifier        TFItems DeclsAndStmts "endtask"     opt(Tag) { [Task     $2 $3                (map defaultFuncInput $ $4 ++ fst $5) (snd $5)] }
  | "import" PackageImportItems ";" { map (uncurry Import) $2 }
  | "export" PackageImportItems ";" { map (Export .  Just) $2 }
  | "export" "*" "::" "*" ";"       { [Export Nothing] } -- "Nothing" being no restrictions
  | ForwardTypedef ";"              { $1 }
  | TimeunitsDeclaration            { $1 }
  | Directive                       { [Directive $1] }
ForwardTypedef :: { [PackageItem] }
  : "typedef"          Identifier { [] }
  | "typedef" "enum"   Identifier { [] }
  | "typedef" "struct" Identifier { [] }
  | "typedef" "union"  Identifier { [] }
TimeunitsDeclaration :: { [PackageItem] }
  : "timeunit" Time          ";" { [] }
  | "timeunit" Time "/" Time ";" { [] }
  | "timeprecision" Time     ";" { [] }

Directive :: { String }
  : "`celldefine"          { tokenString $1 }
  | "`endcelldefine"       { tokenString $1 }
  | "`unconnected_drive" Drive { tokenString $1 ++ " " ++ $2 }
  | "`nounconnected_drive" { tokenString $1 }
  | "`default_nettype" DefaultNetType { tokenString $1 ++ " " ++ $2 }
  | "`resetall"            { tokenString $1 }
Drive :: { String }
  : "pull0" { tokenString $1 }
  | "pull1" { tokenString $1 }
DefaultNetType :: { String }
  : NetType { show $1 }
  | Identifier { $1 }

PackageImportItems :: { [(Identifier, Maybe Identifier)] }
  : PackageImportItem                        { [$1] }
  | PackageImportItems "," PackageImportItem { $1 ++ [$3] }
PackageImportItem :: { (Identifier, Maybe Identifier) }
  : Identifier "::" Identifier { ($1, Just $3) }
  | Identifier "::" "*"        { ($1, Nothing) }

FuncRetAndName :: { (Type, Identifier) }
  : Type                       Identifier { ($1                     , $2) }
  |                            Identifier { (Implicit Unspecified [], $1) }
  | Signing                    Identifier { (Implicit $1          [], $2) }
  |         DimensionsNonEmpty Identifier { (Implicit Unspecified $1, $2) }
  | Signing DimensionsNonEmpty Identifier { (Implicit $1          $2, $3) }

AlwaysKW :: { AlwaysKW }
  : "always"       { Always      }
  | "always_comb"  { AlwaysComb  }
  | "always_ff"    { AlwaysFF    }
  | "always_latch" { AlwaysLatch }

Lifetime :: { Lifetime }
  : {- empty -} { Inherit }
  | ExplicitLifetime { $1 }
ExplicitLifetime :: { Lifetime }
  : "static"    { Static    }
  | "automatic" { Automatic }

TFItems :: { [Decl] }
  : "(" DeclTokens(")") ";" { parseDTsAsDecls $2 }
  | "("            ")"  ";" { [] }
  |                     ";" { [] }

ParamType :: { Type }
  : PartialType OptSigning Dimensions { $1 $2 $3 }
  | DimensionsNonEmpty { Implicit Unspecified $1 }
  | Signing Dimensions { Implicit $1          $2 }

Dimensions :: { [Range] }
  : {- empty -}        { [] }
  | DimensionsNonEmpty { $1 }
DimensionsNonEmpty :: { [Range] }
  : Dimension                    { [$1] }
  | DimensionsNonEmpty Dimension { $1 ++ [$2] }
Dimension :: { Range }
  : Range        { $1 }
  | "[" Expr "]" { (RawNum 0, BinOp Sub $2 (RawNum 1)) }

DeclAsgns :: { [(Identifier, Expr, [Range])] }
  : DeclAsgn               { [$1] }
  | DeclAsgns "," DeclAsgn { $1 ++ [$3] }
DeclAsgn :: { (Identifier, Expr, [Range]) }
  : Identifier                    "=" Expr { ($1, $3, []) }
  | Identifier DimensionsNonEmpty "=" Expr { ($1, $4, $2) }

Range :: { Range }
  : "[" Expr ":"  Expr "]" { ($2, $4) }

PartSelect :: { (PartSelectMode, Range) }
  : "[" Expr ":"  Expr "]" { (NonIndexed  , ($2, $4)) }
  | "[" Expr "+:" Expr "]" { (IndexedPlus , ($2, $4)) }
  | "[" Expr "-:" Expr "]" { (IndexedMinus, ($2, $4)) }

LHS :: { LHS }
  : Identifier         { LHSIdent  $1    }
  | LHS PartSelect     { LHSRange  $1 (fst $2) (snd $2) }
  | LHS "[" Expr "]"   { LHSBit    $1 $3 }
  | LHS "." Identifier { LHSDot    $1 $3 }
  | LHSConcat          { LHSConcat $1    }
  | "{" StreamOp StreamSize Concat "}" { LHSStream $2 $3         (map toLHS $4) }
  | "{" StreamOp            Concat "}" { LHSStream $2 (RawNum 1) (map toLHS $3) }

LHSConcat :: { [LHS] }
  : "{" LHSs "}" { $2 }
LHSs :: { [LHS] }
  : LHS           { [$1] }
  | LHSs "," LHS  { $1 ++ [$3] }

PortBindings :: { [PortBinding] }
  : "("                    ")" { [] }
  | "(" PortBindingsInside ")" { $2 }
PortBindingsInside :: { [PortBinding] }
  : PortBinding                        { [$1] }
  | PortBinding "," PortBindingsInside { $1 : $3}
PortBinding :: { PortBinding }
  : "." Identifier "(" ExprOrNil ")" { ($2, $4) }
  | "." Identifier                   { ($2, Ident $2) }
  | Expr                             { ("", $1) }
  | ".*"                             { ("*", Nil) }

ParamBindings :: { [ParamBinding] }
  : "#" "("                     ")" { [] }
  | "#" "(" ParamBindingsInside ")" { $3 }
ParamBindingsInside :: { [ParamBinding] }
  : ParamBinding                         { [$1] }
  | ParamBinding "," ParamBindingsInside { $1 : $3}
ParamBinding :: { ParamBinding }
  : "." Identifier "(" TypeOrExpr ")" { ($2, $4) }
  | "." Identifier "("            ")" { ($2, Right Nil) }
  | TypeOrExpr                        { ("", $1) }

Stmts :: { [Stmt] }
  : {- empty -} { [] }
  | Stmts Stmt  { $1 ++ [$2] }

Stmt :: { Stmt }
  : StmtTrace StmtAsgn    { Block Seq "" [] [$1, $2] }
  | StmtTrace StmtNonAsgn { $2 }

StmtAsgn :: { Stmt }
  : LHS "="  opt(DelayOrEvent) Expr ";" { Asgn AsgnOpEq $3 $1 $4 }
  | LHS "<=" opt(DelayOrEvent) Expr ";" { Asgn AsgnOpNonBlocking $3 $1 $4 }
  | LHS AsgnBinOp              Expr ";" { Asgn $2  Nothing $1 $3 }
  | LHS IncOrDecOperator ";" { Asgn (AsgnOp $2) Nothing $1 (RawNum 1) }
  | IncOrDecOperator LHS ";" { Asgn (AsgnOp $1) Nothing $2 (RawNum 1) }
  | LHS          ";" { Subroutine (lhsToExpr $1) (Args [] []) }
  | LHS CallArgs ";" { Subroutine (lhsToExpr $1) $2 }
StmtNonAsgn :: { Stmt }
  : StmtBlock(BlockKWSeq, "end" ) { $1 }
  | StmtBlock(BlockKWPar, "join") { $1 }
  |                StmtNonBlock   { $1 }
  | Identifier ":" StmtNonBlock   { Block Seq $1 [] [$3] }
StmtBlock(begin, end) :: { Stmt }
  :                begin StrTag DeclsAndStmts end StrTag { uncurry (Block $1 $ combineTags $2 $5) $3 }
  | Identifier ":" begin        DeclsAndStmts end StrTag { uncurry (Block $3 $ combineTags $1 $6) $4 }
StmtNonBlock :: { Stmt }
  : ";" { Null }
  | Unique "if" "(" Expr ")" Stmt "else" Stmt  { If $1 $4 $6 $8   }
  | Unique "if" "(" Expr ")" Stmt %prec NoElse { If $1 $4 $6 Null }
  | "for" "(" ForInit ForCond ForStep ")" Stmt { For $3 $4 $5 $7 }
  | Unique CaseKW "(" Expr ")" Cases "endcase" { Case $1 $2 $4 $6 }
  | TimingControl Stmt                         { Timing $1 $2 }
  | "return" ExprOrNil ";"                     { Return $2 }
  | "break"       ";"                          { Break }
  | "continue"    ";"                          { Continue }
  | "while"  "(" Expr ")" Stmt                 { While   $3 $5 }
  | "repeat" "(" Expr ")" Stmt                 { RepeatL $3 $5 }
  | "do"      Stmt "while" "(" Expr ")" ";"    { DoWhile $5 $2 }
  | "forever" Stmt                             { Forever $2 }
  | "foreach" "(" Identifier IdxVars ")" Stmt  { Foreach $3 $4 $6 }
  | "->"  Identifier ";"                       { Trigger True  $2 }
  | "->>" Identifier ";"                       { Trigger False $2 }
  | AttributeInstance Stmt                     { StmtAttr $1 $2 }
  | ProceduralAssertionStatement               { Assertion $1 }
  | "void" "'" "(" Expr CallArgs ")" ";"       { Subroutine $4 $5 }

BlockKWPar :: { BlockKW }
  : "fork" { Par }
BlockKWSeq :: { BlockKW }
  : "begin" { Seq }

Unique :: { ViolationCheck }
  : {- empty -} { NoCheck  }
  | "unique"    { Unique   }
  | "unique0"   { Unique0  }
  | "priority"  { Priority }

ForInit :: { Either [Decl] [(LHS, Expr)] }
  :            ";"  { Right [] }
  | DeclTokens(";") { parseDTsAsDeclsOrAsgns $1 }

ForCond :: { Expr }
  :      ";" { RawNum 1 }
  | Expr ";" { $1 }

ForStep :: { [(LHS, AsgnOp, Expr)] }
  : {- empty -}     { [] }
  | ForStepNonEmpty { $1 }
ForStepNonEmpty :: { [(LHS, AsgnOp, Expr)] }
  : ForStepAssignment                     { [$1] }
  | ForStepNonEmpty "," ForStepAssignment { $1 ++ [$3] }
ForStepAssignment :: { (LHS, AsgnOp, Expr) }
  : LHS AsgnOp Expr { ($1, $2, $3) }
  | IncOrDecOperator LHS { ($2, AsgnOp $1, RawNum 1) }
  | LHS IncOrDecOperator { ($1, AsgnOp $2, RawNum 1) }

IdxVars :: { [Identifier] }
  : "[" IdxVarsInside "]" { $2 }
IdxVarsInside :: { [Identifier] }
  : IdxVar                   { [$1] }
  | IdxVar "," IdxVarsInside { $1 : $3 }
IdxVar :: { Identifier }
  : {- empty -} { "" }
  | Identifier  { $1 }

DeclsAndStmts :: { ([Decl], [Stmt]) }
  : StmtTrace DeclOrStmt DeclsAndStmts { combineDeclsAndStmts $2 $3 }
  | StmtTrace StmtNonAsgn Stmts        { ([], $1 : $2 : $3) }
  | StmtTrace {- empty -}              { ([], []) }
DeclOrStmt :: { ([Decl], [Stmt]) }
  : DeclTokens(";")    { parseDTsAsDeclOrStmt $1 }
  | ParameterDecl(";") { ($1, []) }

ModuleParameterDecl(delim) :: { [Decl] }
  :    ParameterDecl(delim) { $1 }
  | "type" TypeAsgns delim  { map (uncurry $ ParamType Parameter) $2 }
ParameterDecl(delim) :: { [Decl] }
  : ParameterDeclKW           DeclAsgns delim { makeParamDecls $1 (Implicit Unspecified []) $2 }
  | ParameterDeclKW ParamType DeclAsgns delim { makeParamDecls $1 $2 $3 }
  | ParameterDeclKW TypeAlias DeclAsgns delim { makeParamDecls $1 $2 $3 }
  | ParameterDeclKW "type"    TypeAsgns delim { map (uncurry $ ParamType $1) $3 }
ParameterDeclKW :: { ParamScope }
  : "parameter"  { Parameter  }
  | "localparam" { Localparam }

TypeAsgns :: { [(Identifier, Maybe Type)] }
  : TypeAsgn               { [$1] }
  | TypeAsgns "," TypeAsgn { $1 ++ [$3] }
TypeAsgn :: { (Identifier, Maybe Type) }
  : Identifier "=" Type { ($1, Just $3) }
  | Identifier          { ($1, Nothing) }

-- TODO: This does not allow for @identifier
ClockingEvent :: { Sense }
  : "@" "(" Senses ")" { $3 }

TimingControl :: { Timing }
  : DelayOrEvent { $1 }
  | CycleDelay   { Cycle $1 }
DelayOrEvent :: { Timing }
  : DelayControl { Delay $1 }
  | EventControl { Event $1 }
DelayControl :: { Expr }
  : "#" Number { Number $2 }
  | "#" Time   { Time   $2 }
  | "#" "(" Expr ")"  { $3 }
  | "#" "(" Expr ":" Expr ":" Expr ")" { MinTypMax $3 $5 $7 }
  | "#"                               Identifier { Ident         $2 }
  | "#" Identifier               "::" Identifier { PSIdent $2    $4 }
  | "#" Identifier ParamBindings "::" Identifier { CSIdent $2 $3 $5 }
CycleDelay :: { Expr }
  : "##" Expr { $2 }
EventControl :: { Sense }
  : "@" "(" Senses ")" { $3 }
  | "@" "(*)"          { SenseStar }
  | "@*"               { SenseStar }
  | "@" Identifier     { Sense $ LHSIdent $2 }
Senses :: { Sense }
  : Sense             { $1 }
  | Senses "or" Sense { SenseOr $1 $3 }
  | Senses ","  Sense { SenseOr $1 $3 }
Sense :: { Sense }
  : "(" Sense ")"         {              $2 }
  |               LHS     { Sense        $1 }
  | "posedge"     LHS     { SensePosedge $2 }
  | "negedge"     LHS     { SenseNegedge $2 }
  | "posedge" "(" LHS ")" { SensePosedge $3 }
  | "negedge" "(" LHS ")" { SenseNegedge $3 }

CaseKW :: { CaseKW }
  : "case"  { CaseN }
  | "casex" { CaseX }
  | "casez" { CaseZ }

Cases :: { [Case] }
  : opt("inside") InsideCases { validateCases $1 $2 }
InsideCases :: { [([ExprOrRange], Stmt)] }
  : InsideCase             { [$1] }
  | InsideCases InsideCase { $1 ++ [$2] }
InsideCase :: { ([ExprOrRange], Stmt) }
  : OpenRangeList ":"  Stmt { ($1, $3) }
  | "default" opt(":") Stmt { ([], $3) }

Real :: { String }
  : real { tokenString $1 }

Number :: { Number }
  : number { parseNumber $ tokenString $1 }

String :: { String }
  : string { tail $ init $ tokenString $1 }

Time :: { String }
  : time { tokenString $1 }

CallArgs :: { Args }
  : "(" CallArgsInside ")" { $2 }
CallArgsInside :: { Args }
  : {- empty -}                        { Args [        ] [] }
  |                NamedCallArgsFollow { Args [        ] $1 }
  | Expr                 NamedCallArgs { Args [$1      ] $2 }
  |      UnnamedCallArgs NamedCallArgs { Args (Nil : $1) $2 }
  | Expr UnnamedCallArgs NamedCallArgs { Args ($1  : $2) $3 }
UnnamedCallArgs :: { [Expr] }
  : "," ExprOrNil                 { [$2] }
  | UnnamedCallArgs "," ExprOrNil { $1 ++ [$3] }
NamedCallArgs :: { [(Identifier, Expr)] }
  : {- empty -}        { [] }
  | "," NamedCallArgsFollow  { $2 }
NamedCallArgsFollow :: { [(Identifier, Expr)] }
  : NamedCallArg                         { [$1] }
  | NamedCallArgsFollow "," NamedCallArg { $1 ++ [$3] }
NamedCallArg :: { (Identifier, Expr) }
  : "." Identifier "(" ExprOrNil ")" { ($2, $4) }

Exprs :: { [Expr] }
  :           Expr  { [$1] }
  | Exprs "," Expr  { $1 ++ [$3] }

TypeOrExpr :: { TypeOrExpr }
  : TypeNonIdent { Left $1 }
  | Expr         { Right $1 }

OpenRangeList :: { [ExprOrRange] }
  : ValueRange                   { [$1] }
  | OpenRangeList "," ValueRange { $1 ++ [$3] }
ValueRange :: { ExprOrRange }
  : Expr  { Left  $1 }
  | Range { Right $1 }

Expr :: { Expr }
  : "(" Expr ")"                { $2 }
  | String                      { String $1 }
  | Real                        { Real   $1 }
  | Number                      { Number $1 }
  | Time                        { Time   $1 }
  | Expr CallArgs               { Call $1 $2 }
  | DimsFn "(" TypeOrExpr ")"   { DimsFn $1 $3 }
  | DimFn  "(" TypeOrExpr ")"   { DimFn  $1 $3 (RawNum 1) }
  | DimFn  "(" TypeOrExpr "," Expr ")" { DimFn $1 $3 $5 }
  | Expr PartSelect             { Range $1 (fst $2) (snd $2) }
  | Expr "[" Expr "]"           { Bit   $1 $3 }
  | "{" Expr Concat "}"         { Repeat $2 $3 }
  | Concat                      { Concat $1 }
  | Expr "?" Expr ":" Expr      { Mux $1 $3 $5 }
  | Expr "." Identifier         { Dot $1 $3 }
  | "'" "{" PatternItems "}"    { Pattern $3 }
  | CastingType "'" "(" Expr ")" { Cast (Left  $1) $4 }
  | Expr        "'" "(" Expr ")" { Cast (Right $1) $4 }
  | "{" StreamOp StreamSize Concat "}" { Stream $2 $3         $4 }
  | "{" StreamOp            Concat "}" { Stream $2 (RawNum 1) $3 }
  | Expr "inside" "{" OpenRangeList "}" { Inside $1 $4 }
  | "(" Expr ":" Expr ":" Expr ")" { MinTypMax $2 $4 $6 }
  | Identifier %prec REDUCE_OP {- defer -}   { Ident         $1 }
  | Identifier               "::" Identifier { PSIdent $1    $3 }
  | Identifier ParamBindings "::" Identifier { CSIdent $1 $2 $4 }
  -- binary expressions
  | Expr "||"  Expr { BinOp LogOr  $1 $3 }
  | Expr "&&"  Expr { BinOp LogAnd $1 $3 }
  | Expr "->"  Expr { BinOp LogImp $1 $3 }
  | Expr "<->" Expr { BinOp LogEq  $1 $3 }
  | Expr "|"   Expr { BinOp BitOr  $1 $3 }
  | Expr "^"   Expr { BinOp BitXor $1 $3 }
  | Expr "&"   Expr { BinOp BitAnd $1 $3 }
  | Expr "~^"  Expr { BinOp BitXnor $1 $3 }
  | Expr "^~"  Expr { BinOp BitXnor $1 $3 }
  | Expr "+"   Expr { BinOp Add $1 $3 }
  | Expr "-"   Expr { BinOp Sub $1 $3 }
  | Expr "*"   Expr { BinOp Mul $1 $3 }
  | Expr "/"   Expr { BinOp Div $1 $3 }
  | Expr "%"   Expr { BinOp Mod $1 $3 }
  | Expr "**"  Expr { BinOp Pow $1 $3 }
  | Expr "=="  Expr { BinOp Eq $1 $3 }
  | Expr "!="  Expr { BinOp Ne $1 $3 }
  | Expr "<"   Expr { BinOp Lt $1 $3 }
  | Expr "<="  Expr { BinOp Le $1 $3 }
  | Expr ">"   Expr { BinOp Gt $1 $3 }
  | Expr ">="  Expr { BinOp Ge $1 $3 }
  | Expr "===" Expr { BinOp TEq $1 $3 }
  | Expr "!==" Expr { BinOp TNe $1 $3 }
  | Expr "==?" Expr { BinOp WEq $1 $3 }
  | Expr "!=?" Expr { BinOp WNe $1 $3 }
  | Expr "<<"  Expr { BinOp ShiftL $1 $3 }
  | Expr ">>"  Expr { BinOp ShiftR $1 $3 }
  | Expr "<<<" Expr { BinOp ShiftAL $1 $3 }
  | Expr ">>>" Expr { BinOp ShiftAR $1 $3 }
  -- unary expressions
  | "!"  Expr                 { UniOp LogNot $2 }
  | "~"  Expr                 { UniOp BitNot $2 }
  | "+"  Expr %prec REDUCE_OP { UniOp UniAdd $2 }
  | "-"  Expr %prec REDUCE_OP { UniOp UniSub $2 }
  | "&"  Expr %prec REDUCE_OP { UniOp RedAnd  $2 }
  | "~&" Expr %prec REDUCE_OP { UniOp RedNand $2 }
  | "|"  Expr %prec REDUCE_OP { UniOp RedOr   $2 }
  | "~|" Expr %prec REDUCE_OP { UniOp RedNor  $2 }
  | "^"  Expr %prec REDUCE_OP { UniOp RedXor  $2 }
  | "~^" Expr %prec REDUCE_OP { UniOp RedXnor $2 }
  | "^~" Expr %prec REDUCE_OP { UniOp RedXnor $2 }

ExprOrNil :: { Expr }
  : Expr        { $1 }
  | {- empty -} { Nil }

PatternItems :: { [(Identifier, Expr)] }
  : PatternNamedItems   { $1 }
  | PatternUnnamedItems { zip (repeat "") $1 }
PatternNamedItems :: { [(Identifier, Expr)] }
  : PatternNamedItem                       { [$1] }
  | PatternNamedItems "," PatternNamedItem { $1 ++ [$3] }
PatternNamedItem :: { (Identifier, Expr) }
  : Identifier  ":" Expr { ($1       , $3) }
  | PartialType ":" Expr { (':' : show $1  , $3) }
  | "default"   ":" Expr { (':' : "default", $3) }
PatternUnnamedItems :: { [Expr] }
  :                         PatternUnnamedItem { [$1] }
  | PatternUnnamedItems "," PatternUnnamedItem { $1 ++ [$3] }
PatternUnnamedItem :: { Expr }
  : Expr        { $1 }
  | Expr Concat { Repeat $1 $2 }

Concat :: { [Expr] }
  : "{" Exprs "}" { $2 }

StreamOp :: { StreamOp }
  : "<<" { StreamL }
  | ">>" { StreamR }
StreamSize :: { Expr }
  : TypeNonIdent { DimsFn FnBits (Left $1) }
  | Expr         { $1 }

GenItemOrNull :: { GenItem }
  : GenItem { $1 }
  | ";"     { GenNull }

GenItems :: { [GenItem] }
  : {- empty -}      { [] }
  | GenItems ";"     { $1 }
  | GenItems GenItem { $1 ++ [$2] }

GenItem :: { GenItem }
  : GenBlock              { uncurry GenBlock $1 }
  | NonGenerateModuleItem { genItemsToGenItem $ map GenModuleItem $1 }
  | ConditionalGenerateConstruct { $1 }
  | LoopGenerateConstruct        { $1 }
ConditionalGenerateConstruct :: { GenItem }
  : "if" "(" Expr ")" GenItemOrNull "else" GenItemOrNull { GenIf $3 $5 $7      }
  | "if" "(" Expr ")" GenItemOrNull %prec NoElse         { GenIf $3 $5 GenNull }
  | "case" "(" Expr ")" GenCases "endcase" { GenCase $3 $5 }
LoopGenerateConstruct :: { GenItem }
  : "for" "(" GenvarInitialization ";" Expr ";" GenvarIteration ")" GenItem { $3 $5 $7 $9 }

GenBlock :: { (Identifier, [GenItem]) }
  : "begin" StrTag GenItems "end" StrTag { (combineTags $2 $5, $3) }

GenCases :: { [GenCase] }
  : GenCase          { [$1] }
  | GenCases GenCase { validateGenCases $ $1 ++ [$2] }
GenCase :: { GenCase }
  : Exprs         ":"  GenItemOrNull { ($1, $3) }
  | "default" opt(":") GenItemOrNull { ([], $3) }

GenvarInitialization :: { Expr -> (Identifier, AsgnOp, Expr) -> GenItem -> GenItem }
  : "genvar" Identifier "=" Expr { \a b c -> GenBlock "" [GenModuleItem (Genvar $2), GenFor ($2, $4) a b c] }
  |          Identifier "=" Expr { GenFor ($1, $3) }

GenvarIteration :: { (Identifier, AsgnOp, Expr) }
  : Identifier AsgnOp Expr { ($1, $2, $3) }
  | IncOrDecOperator Identifier { ($2, AsgnOp $1, RawNum 1) }
  | Identifier IncOrDecOperator { ($1, AsgnOp $2, RawNum 1) }

AsgnOp :: { AsgnOp }
  : "=" { AsgnOpEq }
  | AsgnBinOp { $1 }
AsgnBinOp :: { AsgnOp }
  : "+="   { AsgnOp Add }
  | "-="   { AsgnOp Sub }
  | "*="   { AsgnOp Mul }
  | "/="   { AsgnOp Div }
  | "%="   { AsgnOp Mod }
  | "&="   { AsgnOp BitAnd }
  | "|="   { AsgnOp BitOr  }
  | "^="   { AsgnOp BitXor }
  | "<<="  { AsgnOp ShiftL }
  | ">>="  { AsgnOp ShiftR }
  | "<<<=" { AsgnOp ShiftAL }
  | ">>>=" { AsgnOp ShiftAR }

IncOrDecOperator :: { BinOp }
  : "++" { Add }
  | "--" { Sub }

DimsFn :: { DimsFn }
  : "$bits"                { FnBits               }
  | "$dimensions"          { FnDimensions         }
  | "$unpacked_dimensions" { FnUnpackedDimensions }
DimFn :: { DimFn }
  : "$left"                { FnLeft               }
  | "$right"               { FnRight              }
  | "$low"                 { FnLow                }
  | "$high"                { FnHigh               }
  | "$increment"           { FnIncrement          }
  | "$size"                { FnSize               }

MITrace :: { ModuleItem }
  : PITrace { MIPackageItem $1 }
PITrace :: { PackageItem }
  : Trace { Decl $ CommentDecl $1 }
StmtTrace :: { Stmt }
  : Trace { CommentStmt $1 }
Trace :: { String }
  : position { "Trace: " ++ show $1 }
position :: { Position }
  : {- empty -} {% gets fst }

{

type ParseState = StateT (Position, [Token]) (ExceptT String IO)

posInject :: (Position -> a) -> ParseState a
posInject cont = do
  pos <- gets fst
  return $ cont pos

positionKeep :: (Token -> ParseState a) -> ParseState a
positionKeep cont = do
  tokens <- gets snd
  case tokens of
    [] -> cont TokenEOF
    tok : toks -> do
      put (tokenPosition tok, toks)
      cont tok

parseError :: Token -> ParseState a
parseError a = case a of
  TokenEOF    -> throwError $ "Parse error: no tokens left to parse."
  Token t s p -> throwError $ show p ++ ": Parse error: unexpected token '"
                  ++ s ++ "' (" ++ show t ++ ")."

genItemsToGenItem :: [GenItem] -> GenItem
genItemsToGenItem [x] = x
genItemsToGenItem xs = GenBlock "" xs

combineDeclsAndStmts :: ([Decl], [Stmt]) -> ([Decl], [Stmt]) -> ([Decl], [Stmt])
combineDeclsAndStmts (a1, b1) (a2, b2) = (a1 ++ a2, b1 ++ b2)

makeInput :: Decl -> Decl
makeInput (Variable Local t x a e) = Variable Input t x a e
makeInput (Variable Input t x a e) = Variable Input t x a e
makeInput (CommentDecl c) = CommentDecl c
makeInput other =
  error $ "unexpected non-var or non-input decl: " ++ (show other)

defaultFuncInput :: Decl -> Decl
defaultFuncInput (Variable dir (Implicit sg rs) x a e) =
  Variable dir t x a e
  where
    t = if dir == Input || dir == Inout
      then IntegerVector TLogic sg rs
      else Implicit sg rs
defaultFuncInput other = other

combineTags :: Identifier -> Identifier -> Identifier
combineTags a "" = a
combineTags "" b = b
combineTags a b =
  if a == b
    then a
    else error $ "tag mismatch: " ++ show (a, b)

toLHS :: Expr -> LHS
toLHS expr =
  case exprToLHS expr of
    Just lhs -> lhs
    Nothing -> error $ "Parse error: cannot convert expression to LHS: "
                ++ show expr

makeParamDecls
  :: ParamScope
  -> Type
  -> [(Identifier, Expr, [Range])]
  -> [Decl]
makeParamDecls s t items =
  map mapper items
  where
    (tf, rs) = typeRanges t
    mapper (x, e, a) = Param s (tf $ a ++ rs) x e

fieldDecl :: Type -> (Identifier, [Range]) -> (Type, Identifier)
fieldDecl t (x, rs2) =
  (tf $ rs2 ++ rs1, x)
  where (tf, rs1) = typeRanges t

validateCases :: Maybe Token -> [([ExprOrRange], Stmt)] -> [Case]
validateCases Nothing items =
  if length (filter null exprs) <= 1
    then zip exprs' stmts
    else error $ "multiple default cases: " ++ show items
  where
    (exprs, stmts) = unzip items
    exprs' = map (map unwrap) exprs
    unwrap (Left expr) = expr
    unwrap (Right range) =
      error $ "illegal use of a range (" ++ show range
        ++ ") in a non-inside case"
validateCases (Just _) items =
  if length (filter null sets) <= 1
    then zip sets' stmts
    else error $ "multiple default cases: " ++ show items
  where
    (sets, stmts) = unzip items
    sets' = map unwrap sets
    unwrap [] = []
    unwrap ls = [Inside Nil ls]

validateGenCases :: [GenCase] -> [GenCase]
validateGenCases items =
  if length (filter null exprs) <= 1
    then items
    else error $ "multiple default generate cases: " ++ show items
  where
    (exprs, _) = unzip items

makeLocalparam :: Decl -> Decl
makeLocalparam (Param _ t x e) = Param Localparam t x e
makeLocalparam (ParamType _ x mt) = ParamType Localparam x mt
makeLocalparam other = other

}
